{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Protolude              hiding (and)
import Test.Hspec             (describe, hspec, it)
import Test.Hspec.SmallCheck  (property)
import Test.SmallCheck.Series (Serial, cons0, series, (\/))

import qualified Adder
import           Bit   (Bit (I, O))
import qualified Gate


instance Monad m => Serial m Bit where
    series = cons0 O \/ cons0 I


main :: IO ()
main = hspec $ do

  describe "Hardware gates" $ do

    it "and" $ property $ \x y -> Gate.and x y == Gate.andTT x y

    it "or"  $ property $ \x y -> Gate.or x y == Gate.orTT x y

    it "not" $ property $ \x -> Gate.not x == Gate.notTT x

    it "xor" $ property $ \x y -> Gate.xor x y == Gate.xorTT x y


  describe "Adders" $ do

    it "half adder" $ property $ \x y -> Adder.half x y == Adder.halfTT x y

    it "adder"  $ property $ \x y z -> Adder.full x y z == Adder.fullTT x y z
