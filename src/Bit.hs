module Bit where

import Protolude

data Bit = O | I
    deriving (Show, Eq)

fromS :: Text -> Maybe Bit
fromS "1" = Just I
fromS "0" = Just O
fromS _   = Nothing

toS :: Bit -> Text
toS O = "0"
toS I = "1"

fromI :: Int -> Maybe Bit
fromI 1 = Just I
fromI 0 = Just O
fromI _ = Nothing

toI :: Bit -> Int
toI O = 0
toI I = 1
