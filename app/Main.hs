{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Lazy            as BL

import           Bencode
import           Client
import           MetaInfo
import           System.Environment

openTorrentFile :: String -> IO (Maybe MetaInfo)
openTorrentFile filename = do
  contents <- BL.readFile filename
  return $ AL.maybeResult (AL.parse value contents) >>= parseMetaInfo


main :: IO ()
main = do
  args <- getArgs
  res <- openTorrentFile $ head args
  case res of
    Just meta -> do
      clientState <- newClientState "." meta 8035
      _ <- btListen clientState
      queryTracker clientState
      return ()
    Nothing   -> putStrLn "no files provided"
