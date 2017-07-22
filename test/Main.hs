{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Protolude              hiding (and)
import Test.SmallCheck        (smallCheck)
import Test.SmallCheck.Series (Serial, cons0, series, (\/))

import Bits  (Bit (I, O))
import Gates


instance Monad m => Serial m Bit where
    series = cons0 O \/ cons0 I


main :: IO ()
main = smallCheck 4 $ \x y -> and x y == and' x y
