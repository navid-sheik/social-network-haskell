
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Types(
    User(..),
    Message (..)
) where

import Data.Time.Calendar
import Data.Time


data User = User{
    username :: String,
    email :: String,
    favorite_topics :: [String],
    messages::[Message]
} deriving (Show)

data Message  = Message{
    text :: String,
    from :: String,
    to :: String,
    date :: Day
} deriving (Show, Eq)



comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

instance Ord Message where
    compare = comparing date
