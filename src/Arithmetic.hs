module Arithmetic where

import Protolude

import           Adder (Output (..), full)
import qualified Bit
import qualified Byte
import qualified Logic


type BT = Byte.Byte

addCarry :: Bit.Bit -> BT -> BT -> Output BT
addCarry c
        (Byte.Byte a7 a6 a5 a4 a3 a2 a1 a0)
        (Byte.Byte b7 b6 b5 b4 b3 b2 b1 b0)
    = Output c7 (Byte.Byte r7 r6 r5 r4 r3 r2 r1 r0)
    where
        (Output c0 r0) = full a0 b0 c
        (Output c1 r1) = full a1 b1 c0
        (Output c2 r2) = full a2 b2 c1
        (Output c3 r3) = full a3 b3 c2
        (Output c4 r4) = full a4 b4 c3
        (Output c5 r5) = full a5 b5 c4
        (Output c6 r6) = full a6 b6 c5
        (Output c7 r7) = full a7 b7 c6

add :: BT -> BT -> Output BT
add = addCarry Bit.O

-- TODO: substract

-- TODO: substract with borrow

negate :: BT -> Output BT
negate = inc . Logic.inv

inc :: BT -> Output BT
inc x = add x Byte.one

-- TODO: Decrement

