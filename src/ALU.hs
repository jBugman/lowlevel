module ALU where

import Protolude

import           Adder      (Output (..))
import qualified Arithmetic
import           Bit        (Bit (..))
import qualified Byte
import qualified Logic


data Status = Status
    { unZero   :: Bit
    , unCarry  :: Bit
    , unParity :: Bit
    , unSign   :: Bit
} deriving (Show, Eq)

nil :: Status
nil = Status O O O O

data Opcode = ADD | AND | OR | XOR | INC | NOP
    deriving (Show, Eq)

process :: Opcode -> Byte.Byte -> Byte.Byte -> Status -> Maybe (Byte.Byte, Status)
process ADD a b _ = Just (x, Status O c O O) where Output c x = Arithmetic.add a b
process AND a b _ = Just (x, nil) where x = Logic.and a b
process OR  a b _ = Just (x, nil) where x = Logic.or a b
process XOR a b _ = Just (x, nil) where x = Logic.xor a b
process INC a _ _ = Just (x, Status O c O O) where Output c x = Arithmetic.inc a
process _ _ _ _   = Nothing
