
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
    messages ::  [Message]
}deriving (Show)

data Message  = Message{
    text :: String,
    from :: String,
    to :: String
}deriving (Show)




main :: IO ()
main = do
    let user1 = User {username="navid", email="tddj@gmail.com", messages=[]}  
    append Message(text="somethign", from="somdins", to="vdlsvmfm") messages user1
    

    print "!!ERROR!!  Invalid option. Please try again.  !!ERROR!! "
    