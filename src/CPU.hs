module CPU where

import Data.Maybe (fromJust)
import Protolude

import Byte (Byte (..), fromBitString)


newtype Opcode = Opcode Byte

mkOP :: Text -> Opcode
mkOP = Opcode . fromJust . fromBitString

-- No operation
opNOP :: Opcode
opNOP  = mkOP "00000000"

-- Load A from memory
opLDA :: Opcode
opLDA  = mkOP "00111010"

-- Add register B to A
opADDB :: Opcode
opADDB = mkOP "10000000"  -- 10000SSS

-- Add immediate to A
opADI :: Opcode
opADI  = mkOP "11000110"

-- Unconditional jump
opJMP :: Opcode
opJMP  = mkOP "11000011"

-- Halt processor
opHLT :: Opcode
opHLT  = mkOP "01110110"

-- Increment register A
opINRA :: Opcode
opINRA = mkOP "00111100"  -- 00DDD100

-- Increment register B
opINRB :: Opcode
opINRB = mkOP "00000100"  -- 00DDD100

-- AND register B with A
opANAB :: Opcode
opANAB = mkOP "10100000"  -- 10100SSS

-- AND immediate with A
opANI :: Opcode
opANI  = mkOP "11100110"

-- OR register B with A
opORAB :: Opcode
opORAB = mkOP "10110000"  -- 10110SSS

-- OR immediate with A
opORI :: Opcode
opORI  = mkOP "11100110"

-- XOR register B with A
opXRAB :: Opcode
opXRAB = mkOP "10101000"  -- 10101SSS

-- XOR immediate with A
opXRI :: Opcode
opXRI  = mkOP "11101110"
