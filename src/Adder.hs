module Adder where

import Protolude

import           Bit  (Bit (I, O))
import qualified Gate


data Output = Output { unCarry :: Bit , unSum :: Bit }
    deriving (Show, Eq)


-- Truth tables --

halfTT :: Bit -> Bit -> Output
halfTT O O = Output O O
halfTT O I = Output O I
halfTT I O = Output O I
halfTT I I = Output I O

fullTT :: Bit -> Bit -> Bit -> Output
fullTT O O O = Output O O
fullTT O O I = Output O I
fullTT O I O = Output O I
fullTT O I I = Output I O
fullTT I O O = Output O I
fullTT I O I = Output I O
fullTT I I O = Output I O
fullTT I I I = Output I I


-- Hardware --

half :: Bit -> Bit -> Output
half a b = Output c s  where
    c = Gate.and a b
    s = Gate.xor a b

full :: Bit -> Bit -> Bit -> Output
full a b c' = Output c s  where
    s = Gate.xor x c'
    c = Gate.or  y z
    x = Gate.xor a b
    y = Gate.and x c'
    z = Gate.and a b
