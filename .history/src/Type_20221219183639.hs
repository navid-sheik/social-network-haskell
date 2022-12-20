
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Types{
    User(..),
    Message (..)
}


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
