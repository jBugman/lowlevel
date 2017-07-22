module Logic where

import           Byte (Byte (..))
import qualified Gate

and :: Byte -> Byte -> Byte
and (Byte a7 a6 a5 a4 a3 a2 a1 a0) (Byte b7 b6 b5 b4 b3 b2 b1 b0) = Byte
    (Gate.and a7 b7)
    (Gate.and a6 b6)
    (Gate.and a5 b5)
    (Gate.and a4 b4)
    (Gate.and a3 b3)
    (Gate.and a2 b2)
    (Gate.and a1 b1)
    (Gate.and a0 b0)

or :: Byte -> Byte -> Byte
or (Byte a7 a6 a5 a4 a3 a2 a1 a0) (Byte b7 b6 b5 b4 b3 b2 b1 b0) = Byte
    (Gate.or a7 b7)
    (Gate.or a6 b6)
    (Gate.or a5 b5)
    (Gate.or a4 b4)
    (Gate.or a3 b3)
    (Gate.or a2 b2)
    (Gate.or a1 b1)
    (Gate.or a0 b0)

xor :: Byte -> Byte -> Byte
xor (Byte a7 a6 a5 a4 a3 a2 a1 a0) (Byte b7 b6 b5 b4 b3 b2 b1 b0) = Byte
    (Gate.xor a7 b7)
    (Gate.xor a6 b6)
    (Gate.xor a5 b5)
    (Gate.xor a4 b4)
    (Gate.xor a3 b3)
    (Gate.xor a2 b2)
    (Gate.xor a1 b1)
    (Gate.xor a0 b0)

inv :: Byte -> Byte
inv (Byte b7 b6 b5 b4 b3 b2 b1 b0) = Byte
    (Gate.not b7)
    (Gate.not b6)
    (Gate.not b5)
    (Gate.not b4)
    (Gate.not b3)
    (Gate.not b2)
    (Gate.not b1)
    (Gate.not b0)
