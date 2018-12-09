{-# LANGUAGE RankNTypes #-}

module BencodeLenses where

import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import           Data.Map.Strict       as Map
import           Data.Word             (Word32)
import           Lens.Family2
import           Prelude               hiding (take)

import           Bencode

bstring :: Traversal' BValue ByteString
bstring f (String s) = String <$> f s
bstring _ bv         = pure bv

bnumber :: Traversal' BValue Word32
bnumber f (Number n) = Number <$> f n
bnumber _ bv         = pure bv

blist :: Traversal' BValue BValue
blist f (List xs) = List <$> traverse f xs
blist _ bv        = pure bv

bkey :: ByteString -> Traversal' BValue BValue
bkey k f bv@(Dictionary m) = case Map.lookup k m of
                              Just v  -> f v
                              Nothing -> pure bv

bkey _ _ bv = pure bv
