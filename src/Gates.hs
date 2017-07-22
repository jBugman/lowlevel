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


-- NAND-based --
and :: Bit -> Bit -> Bit
and a b = nand c c  where
    c = nand a b

not :: Bit -> Bit
not a = nand a a

or :: Bit -> Bit -> Bit
or a b = nand a' b' where
    a' = nand a a
    b' = nand b b

xor :: Bit -> Bit -> Bit
xor a b = nand c d  where
    c = nand a x
    d = nand b x
    x = nand a b
