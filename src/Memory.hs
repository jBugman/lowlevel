module Memory (get, set, empty) where

import qualified Data.Vector as V
import           Protolude   hiding (empty, get)

import           Bit  (Bit (..))
import           Byte (Byte (..))
import qualified Byte
import qualified Gate


newtype Cell = Cell { unCell :: Byte }
    deriving (Eq, Show)

type Mem = V.Vector Cell

type Address = Byte

empty :: Mem
empty = V.replicate 256 $ cell Byte.zero

get :: Mem -> Address -> Byte
get m a = fromMaybe Byte.zero $ unCell <$> m V.!? address a

set :: Mem -> Address -> Byte -> Mem
set m a x = V.update m $ V.singleton (address a , cell x)

address :: Address -> Int
address = Byte.toInt

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
