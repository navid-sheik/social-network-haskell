
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Main (main) where

import Lib

import Control.Concurrent
import System.Random
import System.IO
import Data.List



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

addNewMessage :: Message  -> User -> User
addNewMessage  message  user= 
    user { messages = existinsmessages ++ [message] }
    where existinsmessages  = messages user


-- process :: String ->  MVar Int -> MVar [Message] -> IO()
-- process name count messages =  do
--     messages_user  <- takeVar messages
    -- Create a user

    -- Create message 
    -- Random time send messages and put the var back
    -- Before putting var back, zçß√√√√√√√√√√√√√√√√√√√vw∑∑∑vvvvvvvvvvvvvvvvvvvvvçßßçßçç √©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©©© check the lenght of messages , 
    -- If the lenght is  equalq    √√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√√`åy6 to 100, we end all the process
    -- Else we put the messages back, delay and then return the process, .... 
-- process :: String ->  MVar Int -> MVar [Message] -> IO()
-- process user count messages =  do
--     messagesa <- takeVar messages
--     putStrLn(show user)
--     if count == 2:
--         putMVar   messagesa $ "Closing "
--     else do
--         let new_message  = Message{text  = "anothitner", from = "ua", to="ua"}
--         messagesss <- existinsmessages ++ [new_message]
--         putMVar box_messages messagesss
--         threadDelay 5

atRandIndex :: [a] -> IO a  -- note that this is gives itself an IO action
atRandIndex l = do
    i <- randomRIO (0, length l - 1)
    return $ l !! i

process :: String  -> MVar Int -> MVar [Message]->MVar String -> [User]->IO()
process  name count messages winner users = do
    c1 <- takeMVar count
    ms <- takeMVar messages
    putStrLn $ "The messages are" ++ show(ms)
    putStrLn $ "The count is " ++ show(c1)
    random <- atRandIndex users
    show (random)
    if c1 == 10 then
        putMVar winner $ "Count is " ++ name ++ " !" 
    else do
        putStrLn $ " Process with " ++ name ++ " increasing !"
        let newMessage  = Message{text  = "anothitner", from = "ua", to="ua"}
        let new_messages = ms ++ [newMessage]
        putMVar messages $! new_messages
        putMVar count $! c1 + 1
        threadDelay 5
        process  name count messages winner users


main :: IO ()
main = do
    let user1 = User {username="navid", email="tddj@gmail.com", messages=[]}  
    let user2 = User {username="kami", email="kami@gmail.com", messages=[]}  
    let array_user  = [user1, user2]
    let messages= []
    count <- newMVar 0
    winner <- newEmptyMVar
    messages <- newMVar messages
    forkIO(process  "user1"  count messages winner array_user)
    forkIO(process  "user2"  count messages winner array_user)
    w <- takeMVar winner
    putStrLn $ show(w)

    -- let user2 = addNewMessage  Message{text  = "anothitner", from = "ua", to="ua"} user1
    -- putStrLn(show user2)


    