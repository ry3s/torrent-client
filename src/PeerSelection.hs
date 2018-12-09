module PeerSelection
  ( getNextPiece
  , AvailabilityData
  , addToAvailability
  , removeFromAvailability
  , getIncompletePieces
  ) where

import           Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as VU
import           Data.Word

import           BitField            (BitField)
import qualified BitField            as BF
import           Utility

type AvailabilityData = VU.Vector Word32

addToAvailability :: BitField -> AvailabilityData -> AvailabilityData
addToAvailability bf av = av // (f <$> [0..fromIntegral $ BF.length bf - 1])
  where f n = (fromIntegral n, (av ! n) + boolToWord (BF.get bf (fromIntegral n)))

removeFromAvailability :: BitField -> AvailabilityData -> AvailabilityData
removeFromAvailability bf av = av // (f <$> [0..fromIntegral $ BF.length bf - 1])
  where f n = (fromIntegral n, (av ! n) - boolToWord (BF.get bf (fromIntegral n)))

getNextPiece :: BitField -> AvailabilityData -> Maybe Word32
getNextPiece bf av = fromIntegral . fst <$> g
  where g = VU.ifoldl' (\counter index availability -> if BF.get bf (fromIntegral index)
            then counter
            else case counter of
              orig@(Just (_, a)) -> if availability > a
                               then Just (index, availability)
                               else orig
              Nothing -> Just (index, availability)
            ) Nothing av

getIncompletePieces bf = filter (not . BF.get bf) [0..BF.length bf - 1]

