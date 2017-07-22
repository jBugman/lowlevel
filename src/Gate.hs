module Gate where

import Bit (Bit (I, O))


-- Truth tables --

orTT :: Bit -> Bit -> Bit
orTT O O = O
orTT _ _ = I

andTT :: Bit -> Bit -> Bit
andTT I I = I
andTT _ _ = O

notTT :: Bit -> Bit
notTT I = O
notTT O = I

xorTT :: Bit -> Bit -> Bit
xorTT O O = O
xorTT O I = I
xorTT I O = I
xorTT I I = O


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
or a b = nand a' b'  where
    a' = nand a a
    b' = nand b b

xor :: Bit -> Bit -> Bit
xor a b = nand c d  where
    c = nand a x
    d = nand b x
    x = nand a b
