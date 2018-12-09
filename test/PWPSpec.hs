{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PWPSpec where

import qualified BitField                  as BF
import           Data.Binary
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import           Data.Char                 (ord)
import           PWP
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances

instance Arbitrary BHandshake where
  arbitrary = do
    infoHash <- B.pack <$> sequence [fromIntegral . ord <$> arbitrary | _ <- [1..20]]
    peerId <- B.pack <$> sequence [fromIntegral . ord <$> arbitrary | _ <- [1..20]]
    return $ BHandshake infoHash peerId

instance Arbitrary PWP where
  arbitrary = do
    pieceId <- arbitrary
    bytestring <- arbitrary
    request <- Request <$> arbitrary <*> arbitrary <*> arbitrary
    piece <- Piece <$> arbitrary <*> arbitrary <*> arbitrary
    cancel <- Cancel <$> arbitrary <*> arbitrary <*> arbitrary
    elements [KeepAlive
            , Choke
            , Unchoke
            , Interested
            , Uninterested
            , Have pieceId
            , Bitfield bytestring
            , request
            , piece
            , cancel]

spec :: SpecWith ()
spec = do
  describe "PWP" $ do
    prop "decode . encode === id" $ \(a :: PWP) ->
      (decodeOrFail . encode) a === Right (BL.empty, BL.length $ encode a, a)

  describe "BHandshake" $ do
    prop "decode . encode === id" $ \(a :: BHandshake) ->
      (decodeOrFail . encode) a === Right (BL.empty, BL.length $ encode a, a)
