module PeerSelectionSpec where

import qualified BitField            as BF
import           Data.AEq
import qualified Data.Vector.Unboxed as VU
import           Numeric.IEEE
import           PeerSelection
import           Test.Hspec

spec :: SpecWith ()
spec = do
  it "gets the most demanded piece" $
    pending
  it "does not return anything after completing" $
    pending
