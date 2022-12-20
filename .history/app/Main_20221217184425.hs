
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Main (main) where

import Lib

import Control.Concurrent
import System.Random
import System.IO


data User = User{
    username :: String,
    email :: String,
    messages::[Message]
}deriving (Show)

data Message  = Message{
    text :: String,
    from :: String,
    to :: String
}deriving (Show)



append :: Message -> [Message] -> [Message]
append a [] = [a]
append a (x:xs) = x : append a xs

-- addNewMessage :: Message  -> User
addNewMessage  message  user= user { messages = messages ++ [message] }

main :: IO ()
main = do
    let user1 = User {username="navid", email="tddj@gmail.com", messages=[]}  
    
    addNewMessage $ Message{text  = "anothitner", from = "ua", to="ua"} user1
    putStrLn(show user1)

    print "!!ERROR!!  Invalid option. Please try again.  !!ERROR!! "
    