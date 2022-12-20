
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Main (main) where

import Lib

import Control.Concurrent
import System.Random
import System.IO


data User = User{
    username :: String,
    email :: String
    messages :: Message
}deriving (Show)

data Message  = Message{
    text :: String,
    from :: String,
    to :: String
}deriving (Show)

addNewValues :: [User] -> [Message] -> [User]
addNewValues  (m:ms) (x:xs) = m{ messages = x:messages m }:addNewValues ms xs
addNewValues _ _ = []


main :: IO ()
main = do
    let user1 = User {username="navid", email="tddj@gmail.com", messages = []}  
    addNewValues [user1] [Message{text  = "Something", from = ua, to=ua}]
    -- let ua  = username user1
    -- let messages  = [Message{text  = "Something", from = ua, to=ua}]
    -- let new_message  = Message{text  = "anothitner", from = ua, to=ua}
    -- new_message : messages

    putStrLn(show user1)

    print "!!ERROR!!  Invalid option. Please try again.  !!ERROR!! "
    