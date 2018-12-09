module ChunkField
  ( ChunkStatus(..)
  , ChunkField
  , newChunkField
  , getNextChunk
  , getIncompleteChunks
  , markCompleted
  , markRequested
  , isCompleted
  , isRequested
  ) where

import           Data.Foldable as Foldable
import           Data.Sequence as Seq

data ChunkStatus = Missing | Requested | Completed
                   deriving(Eq, Show)

type ChunkField = Seq ChunkStatus

newChunkField :: Integral a => a -> ChunkField
newChunkField n = Seq.replicate (fromIntegral n) Missing

getNextChunk :: Integral a => ChunkField -> Maybe (ChunkField, a)
getNextChunk cf = (\i -> (Seq.update i Requested cf, fromIntegral i)) <$> ix
  where ix = Seq.elemIndexL Missing cf

getIncompleteChunks :: Integral a => ChunkField -> Maybe (ChunkField, [a])
getIncompleteChunks cf = Just (cf, Foldable.toList values)
  where values = fmap snd
               $ Seq.filter ((/=Completed) . fst)
               $ Seq.mapWithIndex (\i a -> (a, fromIntegral i)) cf

markCompleted :: Integral a => ChunkField -> a -> ChunkField
markCompleted cf ix = Seq.update (fromIntegral ix) Completed cf

markRequested :: Integral a => ChunkField -> a -> ChunkField
markRequested cf ix = Seq.update (fromIntegral ix) Requested cf

isCompleted :: ChunkField -> Bool
isCompleted = Foldable.all (==Completed)

isRequested :: ChunkField -> Bool
isRequested = Foldable.all (/=Missing)

