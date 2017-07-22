{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Protolude              hiding (and)
import Test.Hspec             (describe, hspec, it)
import Test.Hspec.SmallCheck  (property)
import Test.SmallCheck.Series (Serial, cons0, series, (\/))

import           Adders
import           Bits   (Bit (I, O))
import qualified Gates


instance Monad m => Serial m Bit where
    series = cons0 O \/ cons0 I


main :: IO ()
main = hspec $ do

  describe "Hardware gates" $ do

    it "and" $ property $ \x y -> Gates.and x y == Gates.andTT x y

    it "or"  $ property $ \x y -> Gates.or x y == Gates.orTT x y

    it "not" $ property $ \x -> Gates.not x == Gates.notTT x

    it "xor" $ property $ \x y -> Gates.xor x y == Gates.xorTT x y


  describe "Adders" $ do

    it "halfadder" $ property $ \x y -> halfadder x y == halfadderTT x y

    it "adder"  $ property $ \x y z -> adder x y z == adderTT x y z
