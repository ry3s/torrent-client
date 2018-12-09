{-# LANGUAGE OverloadedStrings #-}
module Client
  (newClientState
  , newPeer
  , btListen
  , queryTracker
  , PeerData(..)
  , ClientState(..)
  , reachOutToPeer
  , mainPeerLoop
  , expectedChunkSize
  ) where

import           BencodeLenses
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import           Crypto.Hash.SHA1
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as BC
import           Data.ByteString.Conversion       (fromByteString)
import           Data.ByteString.Internal         as BI
import qualified Data.ByteString.Lazy             as BL
import           Data.Map.Strict                  as Map
import           Data.Maybe                       (fromJust)
import           Data.UUID                        hiding (fromByteString)
import           Data.UUID.V4
import qualified Data.Vector.Unboxed              as VU
import           Data.Word
import           Lens.Family2
import           Network.HTTP.Client
import           Network.Socket
import           System.FilePath
import           System.IO

import           Bencode
import           BencodeLenses
import           BitField                         (BitField)
import qualified BitField                         as BF
import qualified FileWriter                       as FW
import           MetaInfo
import           PeerMonad
import qualified PeerSelection                    as PS
import           PWP
import           Types

newPeer :: Word32 -> SockAddr -> ByteString -> IO PeerData
newPeer pieceCount addr peer = do
  c <- newChan
  return $ PeerData True False True False addr peer (BF.newBitField pieceCount) c 0

addToAvailability :: BitField -> ClientState -> STM ()
addToAvailability bf state = do
  avData <- readTVar (availabilityData state)
  let avData' = PS.addToAvailability bf avData
  writeTVar (availabilityData state) avData'

removeFromAvailability :: BitField -> ClientState -> STM ()
removeFromAvailability bf state = do
  avData <- readTVar (availabilityData state)
  let avData' = PS.removeFromAvailability bf avData
  writeTVar (availabilityData state) avData'

newClientState :: FilePath -> MetaInfo -> Word16 -> IO ClientState
newClientState dir meta listenPort = do
  peers <- newTVarIO Map.empty
  chunks <- newTVarIO Map.empty
  uuid <- nextRandom
  let peer = hash $ toASCIIBytes uuid
  let numPieces :: Integral a => a
      numPieces = fromIntegral (B.length $ pieces $ info meta) `quot` 20
  bit_field <- newTVarIO $ BF.newBitField numPieces
  forkIO $ forever $ do
    bf <- atomically $ readTVar bit_field
    print bf
    threadDelay 5000000
  outHandle <- openFile (dir </> BC.unpack (name (info meta))) ReadWriteMode
  outChan <- FW.operate outHandle
  avData <- newTVarIO $ VU.replicate numPieces 0
  return $ ClientState peers peer meta bit_field chunks outChan listenPort avData

btListen :: ClientState -> IO Socket
btListen state = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral $ ourPort state) 0)
  listen sock 10
  forkIO $ forever $ do
    (sock', addr) <- accept sock
    forkIO $ startFromPeerHandshake state sock' addr
  return sock

startFromPeerHandshake :: ClientState -> Socket -> SockAddr -> IO ()
startFromPeerHandshake state sock addr = do
  handle <- socketToHandle sock ReadWriteMode
  (rest, BHandshake hisInfoHash peer) <- readHandshake handle

  ourPeers <- atomically $ readTVar (statePeers state)

  let ourInfoHash = infoHash $ metaInfo state
      cond = hisInfoHash == ourInfoHash && Map.notMember peer ourPeers
      pieceCount = fromIntegral $ (`quot` 20) $ B.length $ pieces $ info $ metaInfo state

  when cond $ do
    writeHandshake handle state
    pData <- newPeer pieceCount addr peer
    atomically $ setPeer peer pData state

    forkIO $ peerEchoer (chan pData) handle

    bf <- atomically $
      readTVar (bitField state)

    writeChan (chan pData) $ Bitfield (BF.raw bf)

    mainPeerLoop state peer handle $ runGetIncremental get `pushChunk` rest


reachOutToPeer :: ClientState -> SockAddr -> IO ()
reachOutToPeer state addr = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock addr
  handle <- socketToHandle sock ReadWriteMode

  writeHandshake handle state
  (nextInput, BHandshake _ peer) <- readHandshake handle

  pd <- newPeer (fromIntegral $ (`quot` 20) $ B.length $ pieces $ info $ metaInfo state) addr peer
  atomically $ modifyTVar' (statePeers state) (Map.insert peer pd)
  forkIO $ peerEchoer (chan pd) handle

  mainPeerLoop state peer handle $ runGetIncremental get `pushChunk` nextInput

writeHandshake :: Handle -> ClientState -> IO ()
writeHandshake handle state = BL.hPut handle handshake
  where handshake = encode $ BHandshake (infoHash . metaInfo $ state) (myPeerId state)

readHandshake :: Handle -> IO (ByteString, BHandshake)
readHandshake handle = evaluator (runGetIncremental get)
  where evaluator Fail{} = do
          putStrLn "getHandshake: Their handshake is a phail"
          error "getHandshake: Their handshake is a phail"
        evaluator parser@(Partial _) = do
          hWaitForInput handle (-1)
          input <- B.hGetSome handle 1024
          evaluator $ parser `pushChunk` input
        evaluator (Done unused _ handshake) =
          return (unused `seq` handshake `seq` (unused, handshake))

peerEchoer :: Chan PWP -> Handle -> IO ()
peerEchoer c h = forever $ do
  msg <- readChan c
  BL.hPut h $ encode msg

mainPeerLoop :: ClientState -> ByteString -> Handle -> Decoder PWP -> IO ()
mainPeerLoop state peer handle parser =
  case parser of
    Fail _ _ err -> do
      putStrLn "mainPeerLoop fail parse"
      putStrLn err
    Partial _ -> do
      -- this improves test performance *greatly*
      -- for some reason we can read absolutely nothing
      -- many times in a row from local
      -- sockets with hGetSome
      {-# SCC "waiting-on-input" #-} hWaitForInput handle (-1)
      input <- B.hGetSome handle 1024
      mainPeerLoop state peer handle (parser `pushChunk` input)
    Done unused _ msg -> do
      handleMessage state peer msg
      let replacementParser = runGetIncremental get `pushChunk` unused
      mainPeerLoop state peer handle replacementParser

setPeer :: ByteString -> PeerData -> ClientState -> STM ()
setPeer peer peerData state = modifyTVar' (statePeers state) (Map.insert peer peerData)

handleMessage :: ClientState -> ByteString -> PWP -> IO ()
handleMessage state peer msg = do
  peers <- atomically $ readTVar $ statePeers state
  case Map.lookup peer peers of
    Just peerData -> act peerData msg
    Nothing       -> return ()
  where
    emit peerData = writeChan (chan peerData)
    act _ Unchoke = runTorrent state peer handleUnchoke
    act _ (Bitfield field) = runTorrent state peer (handleBitfield field)
    act _ (Piece ix offset d) =
      runTorrent state peer (receiveChunk ix offset d >> requestNextPiece)
    act peerData (Have ix) = do
      let peerData' = peerData { peerBitField = BF.set (peerBitField peerData) ix True }
      atomically $ do
        setPeer peer peerData' state
        removeFromAvailability (peerBitField peerData) state
        addToAvailability (peerBitField peerData') state
    act peerData Interested | amChoking peerData = do
      emit peerData Unchoke
      let peerData' = peerData { amChoking = False }
      atomically $ setPeer peer peerData' state
    act _ (Request ix offset len) =
      runTorrent state peer (serveChunk ix offset len)
    act _ m = do
      putStrLn "unhandled Message"
      print m


queryTracker :: ClientState -> IO ()
queryTracker state = do
  let meta = metaInfo state
  let url = fromByteString (announce meta) >>= parseUrl
  let req = setQueryString [ ("peer_id", Just (myPeerId state))
                           , ("info_hash", Just . infoHash $ meta)
                           , ("compact", Just "1")
                           , ("port", Just "8035")
                           ] (fromJust url)
  manager <- newManager defaultManagerSettings
  response <- httpLbs req manager
  let body = BL.toStrict $ responseBody response
  case AC.maybeResult $ AC.parse  value body of
    Just v  -> do
      let peers = getPeers $ BL.fromStrict $ v ^. (bkey "peers" . bstring)
      _ <- traverse (forkIO . reachOutToPeer state) peers
      threadDelay 100000000
    Nothing -> putStrLn "can't parse response"


getPeers :: BL.ByteString -> [SockAddr]
getPeers src | BL.null src = []
getPeers src = SockAddrInet port' ip : getPeers (BL.drop 6 src)
  where chunk = BL.take 6 src
        ipRaw = BL.take 4 chunk
        ip = runGet getWord32le ipRaw -- source is actually network order,
                                                   -- but HostAddress is too and Data.Binary
                                                   -- converts in `runGet`
                                                   -- we're avoiding this conversion
        portSlice = BL.drop 4 chunk
        port' = fromIntegral (decode portSlice :: Word16)
