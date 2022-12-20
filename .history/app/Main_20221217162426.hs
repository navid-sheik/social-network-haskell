
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
}(Show, Generic, Eq)

data Message  = Message{
    text :: String,
    from :: String,
    to :: String
}(Show, Generic, Eq)





main = do
    let user1 = User {username="navid", email="tddj@gmail.com"}  
    show user1
    print "Hope you've enjoyed using the app!"
