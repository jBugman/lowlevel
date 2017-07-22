{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Protolude              hiding (and)
import Test.Hspec             (describe, hspec, it)
import Test.Hspec.SmallCheck  (property)
import Test.SmallCheck.Series (Serial, cons0, series, (\/))

import           Bits  (Bit (I, O))
import qualified Gates


instance Monad m => Serial m Bit where
    series = cons0 O \/ cons0 I


main :: IO ()
main = hspec $
  describe "Hardware gates" $ do

    it "and" $ property $ \x y -> Gates.and x y == Gates.and' x y

    it "not" $ property $ \x -> Gates.not x == Gates.not' x
