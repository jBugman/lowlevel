module Adders where

import Protolude

import           Bits  (Bit (I, O))
import qualified Gates


data Output = Output { unCarry :: Bit , unSum :: Bit }
    deriving (Eq, Show)


-- Truth tables --

halfadderTT :: Bit -> Bit -> Output
halfadderTT O O = Output O O
halfadderTT O I = Output O I
halfadderTT I O = Output O I
halfadderTT I I = Output I O

adderTT :: Bit -> Bit -> Bit -> Output
adderTT O O O = Output O O
adderTT O O I = Output O I
adderTT O I O = Output O I
adderTT O I I = Output I O
adderTT I O O = Output O I
adderTT I O I = Output I O
adderTT I I O = Output I O
adderTT I I I = Output I I


-- Hardware --

halfadder :: Bit -> Bit -> Output
halfadder _ _ = Output I I

adder :: Bit -> Bit -> Bit -> Output
adder _ _ _ = Output O O
