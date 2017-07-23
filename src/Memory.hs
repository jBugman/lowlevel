module Memory where

import Data.Vector (Vector)
import Protolude

import           Bit  (Bit (..))
import           Byte (Byte (..))
import qualified Gate


newtype Cell = Cell Byte
    deriving (Eq, Show)

type Mem = Vector Cell


dFlipFlop :: Bit -> Bit -> Bit
dFlipFlop clk d = q where
    q  = Gate.nand s q'
    q' = Gate.nand r q
    s  = Gate.nand clk d
    r  = Gate.nand clk d'
    d' = Gate.not  d

bitCell :: Bit -> Bit
bitCell = dFlipFlop I

cell :: Byte -> Cell
cell (Byte b7 b6 b5 b4 b3 b2 b1 b0) = Cell $ Byte
    (bitCell b7)
    (bitCell b6)
    (bitCell b5)
    (bitCell b4)
    (bitCell b3)
    (bitCell b2)
    (bitCell b1)
    (bitCell b0)
