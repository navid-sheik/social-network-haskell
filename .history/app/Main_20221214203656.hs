module Main (main) where

import Lib

import Control.Concurrent
import System.Random
import System.IO


data User = User{
    username :: String,
    email :: String
}

data Message  = Message{
    text :: String,
    from :: String,
    to :: String
}

main = do
    print "Hope you've enjoyed using the app!"
