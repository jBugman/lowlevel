module Byte where

import Data.Text (chunksOf)
import Protolude

import           Bit (Bit (..))
import qualified Bit


data Byte = Byte Bit Bit Bit Bit Bit Bit Bit Bit
    deriving (Eq, Show)

zero :: Byte
zero = Byte O O O O O O O O

fromList :: [Bit] -> Maybe Byte
fromList [b7 , b6 , b5 , b4 , b3 , b2 , b1 , b0] = Just (Byte b7 b6 b5 b4 b3 b2 b1 b0)
fromList _                                       = Nothing

toList :: Byte -> [Bit]
toList (Byte b7 b6 b5 b4 b3 b2 b1 b0) = [b7 , b6 , b5 , b4 , b3 , b2 , b1 , b0]


fromBitString :: Text -> Maybe Byte
fromBitString s = do
    bits <- sequence $ Bit.fromS <$> chunksOf 1 s
    fromList bits

toBitString :: Byte -> Text
toBitString (Byte b7 b6 b5 b4 b3 b2 b1 b0)
    =  Bit.toS b7
    <> Bit.toS b6
    <> Bit.toS b5
    <> Bit.toS b4
    <> Bit.toS b3
    <> Bit.toS b2
    <> Bit.toS b1
    <> Bit.toS b0


setBit :: Int -> Bit -> Byte -> Maybe Byte
setBit 0 x (Byte b7 b6 b5 b4 b3 b2 b1 _ ) = Just (Byte b7 b6 b5 b4 b3 b2 b1 x )
setBit 1 x (Byte b7 b6 b5 b4 b3 b2 _  b0) = Just (Byte b7 b6 b5 b4 b3 b2 x  b0)
setBit 2 x (Byte b7 b6 b5 b4 b3 _  b1 b0) = Just (Byte b7 b6 b5 b4 b3 x  b1 b0)
setBit 3 x (Byte b7 b6 b5 b4 _  b2 b1 b0) = Just (Byte b7 b6 b5 b4 x  b2 b1 b0)
setBit 4 x (Byte b7 b6 b5 _  b3 b2 b1 b0) = Just (Byte b7 b6 b5 x  b3 b2 b1 b0)
setBit 5 x (Byte b7 b6 _  b4 b3 b2 b1 b0) = Just (Byte b7 b6 x  b4 b3 b2 b1 b0)
setBit 6 x (Byte b7 _  b5 b4 b3 b2 b1 b0) = Just (Byte b7 x  b5 b4 b3 b2 b1 b0)
setBit 7 x (Byte _  b6 b5 b4 b3 b2 b1 b0) = Just (Byte x  b6 b5 b4 b3 b2 b1 b0)
setBit _ _ _                              = Nothing
