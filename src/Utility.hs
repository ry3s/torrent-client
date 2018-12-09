module Utility
  ( divideSize
  , boolToWord
  ) where

divideSize :: Integral a => a -> a -> a
divideSize a b | a `rem` b > 0 = (a `quot` b) + 1
divideSize a b = a `quot` b

boolToWord :: Integral a => Bool -> a
boolToWord True  = 1
boolToWord False = 0

