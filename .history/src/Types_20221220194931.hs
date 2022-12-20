{-|
Module      : Types
Copyright   : (c)  Navid Sheikh, 2022
-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Types(
    User(..),
    Message (..)
) where

-- | import the necessary libraries, data type and function 
import Data.Time.Calendar
import Data.Time

-- * Data Type
-- | Used to create threads and identify each user
data User = User{
    username :: String,
    email :: String,
    password:: String,
    favorite_topics :: [String]
} deriving (Show)

-- | Used as communications methods between two users
data Message  = Message{
    text :: String,
    from :: String,
    to :: String,
    date :: Day
} deriving (Show, Eq)


-- | Functions used to add ordering where sorting by date
comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

instance Ord Message where
    compare = comparing date
