{-# LANGUAGE OverloadedStrings #-}

module BitField
  ( BitField(..)
  , newBitField
  , BitField.length
  , lengthRaw
  , raw
  , get
  , set
  , completed
  ) where

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Foldable   as Foldable
import           Data.Word       (Word32)

import           Utility

data BitField = BitField ByteString Word32 deriving (Show, Eq)

newBitField :: Word32 -> BitField
newBitField len = BitField (B.replicate (fromIntegral byteLength) 0) len
  where
    byteLength = divideSize len 8

length :: BitField -> Word32
length (BitField _ s) = s

lengthRaw :: BitField -> Word32
lengthRaw (BitField b _) = fromIntegral $ B.length b

raw :: BitField -> ByteString
raw (BitField b _) = b

get :: BitField -> Word32 -> Bool
get (BitField field _) ix = testBit word (7 - fromIntegral ix `rem` 8)
  where
    byteIx = fromIntegral $ ix `quot`8
    word = B.index field byteIx

set :: BitField -> Word32 -> Bool -> BitField
set (BitField field len) ix val = (BitField $! updatedField) $! len
  where byteIx = fromIntegral ix `quot` 8
        word = B.index field byteIx
        modifier True  = setBit
        modifier False = clearBit
        updatedByte = modifier val word (7 - fromIntegral ix `rem` 8)
        updatedField = B.take byteIx field <> B.singleton updatedByte <> B.drop (byteIx + 1) field

completed :: BitField -> Float
completed (BitField b len) = fromIntegral (Foldable.sum (popCount <$> B.unpack b)) / fromIntegral len
