
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
}deriving (Show)

data Message  = Message{
    text :: String,
    from :: String,
    to :: String
}deriving (Show)




main :: IO ()
main = do
    let user1 = User {username="navid", email="tddj@gmail.com"}  
    let ua  = username user1
    let messages  = [Message{text  = "Something", from = ua, to=ua}]
    let new_message  = Message{text  = "anothitner", from = ua, to=ua}
    messages : new_message

    putStrLn(show messages)

    print "!!ERROR!!  Invalid option. Please try again.  !!ERROR!! "
    