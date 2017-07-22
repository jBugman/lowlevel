module Gates where

import Bits (Bit (..))


-- Logic --

or' :: Bit -> Bit -> Bit
or' O O = O
or' _ _ = I

and' :: Bit -> Bit -> Bit
and' I I = I
and' _ _ = O

not' :: Bit -> Bit
not' I = O
not' O = I

xor' :: Bit -> Bit -> Bit
xor' O O = O
xor' O I = I
xor' I O = I
xor' I I = O


-- Hardware --
nand :: Bit -> Bit -> Bit
nand I I = O
nand _ _ = I

and :: Bit -> Bit -> Bit
and a b = nand c c
    where c = nand a b
