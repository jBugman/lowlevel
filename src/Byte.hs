module Byte where

import qualified Data.Text as T
import           Protolude hiding (toList)

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
fromBitString s = sequence (Bit.fromS <$> symbols) >>= fromList
    where
        symbols = T.chunksOf 1 s

toBitString :: Byte -> Text
toBitString = foldMap Bit.toS . toList


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
