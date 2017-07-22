module Adder where

import Protolude

import Bit (Bit (I, O))
-- import qualified Gate


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
half _ _ = Output I I

full :: Bit -> Bit -> Bit -> Output
full _ _ _ = Output O O
