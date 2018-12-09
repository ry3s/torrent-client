module Types where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Data.Binary
import           Data.ByteString.Internal    as BI
import           Data.Map.Strict             (Map)
import           Network.Socket

import           BitField                    (BitField)
import           ChunkField                  as CF
import qualified FileWriter                  as FW
import           MetaInfo                    as Meta
import qualified PeerSelection               as PS
import           PWP
import           Utility

maxRequestsPerPeer :: Word8
maxRequestsPerPeer = 3

data PeerData = PeerData
  { amChoking      :: Bool
  , amInterested   :: Bool
  , peerChoking    :: Bool
  , peerInterested :: Bool
  , address        :: SockAddr
  , peerId         :: ByteString
  , peerBitField   :: BitField
  , chan           :: Chan PWP
  , requestsLive   :: Word8
  }

data ClientState = ClientState
  { statePeers       :: TVar (Map ByteString PeerData)
  , myPeerId         :: ByteString
  , metaInfo         :: MetaInfo
  , bitField         :: TVar BitField
  , pieceChunks      :: TVar (Map Word32 (ChunkField, ByteString))
  , outputChan       :: Chan FW.Operation
  , ourPort          :: Word16
  , availabilityData :: TVar PS.AvailabilityData
  }

defaultChunkSize :: Word32
defaultChunkSize = 2 ^ (16 :: Word32)

chunksInPieces :: Word32 -> Word32 -> Word32
chunksInPieces = divideSize

expectedPieceSize :: Word32 -> Word32 -> Word32 -> Word32
expectedPieceSize totalSize pix pSize =
  if pix >= pCount
    then if totalSize `rem` pSize == 0
         then pSize
         else totalSize `rem` pSize
    else pSize
  where
    pCount = divideSize totalSize pSize - 1

expectedChunkSize :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
expectedChunkSize totalSize pix cix pSize cSize =
  if cix >= chunksInPiece
    then if expectedPSize `rem` cSize == 0
         then cSize
         else expectedPSize `rem` cSize
    else cSize
  where
    expectedPSize = expectedPieceSize totalSize pix pSize
    chunksInPiece = chunksInPieces expectedPSize cSize

