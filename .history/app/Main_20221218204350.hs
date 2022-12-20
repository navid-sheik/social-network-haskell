
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Main (main) where

import Lib

import Control.Concurrent
import System.Random
import System.IO
import Data.List
import Control.Arrow ((&&&))


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


countOccurences :: Ord a => [a] -> [(a, Int)]
countOccurences = map (head &&& length) . group . sort
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


generateRandomMessage :: IO String
generateRandomMessage = do
    let messages = [ "This is it", "Something is not right" , "Hello, how are you?", "What are you doing ", "Something never change " , "This is will be"]
    random <- atRandIndex messages
    return random

printInformation :: (String, Int) -> String
printInformation (username, count) = "The username: " ++ username ++ " received abouut " ++ show (count)


printChatHistory :: (Message) -> String
printChatHistory  message  =  
    if from message == "navid"
        then "Somethign muight sshbcnjkc"
        else "Somethign sbhcjnskvsdklnvmkfjlfsnjkrvnsjkdvmdksvnmlksdmlvks"

process :: User  -> MVar Int -> MVar [Message]->MVar [Message] -> [User]->IO()
process  user count messages winner users = do
    c1 <- takeMVar count
    ms <- takeMVar messages
    putStrLn $ "The messages are" ++ show(ms)
    putStrLn $ "The count is " ++ show(c1)

    if c1 == 10 then
        putMVar winner $ ms
    else do
        putStrLn $ "--------------------------------------------------"
        putStrLn $ "The current user is  " ++ show(username user) ++ " !"
        random_user <- atRandIndex users
        let random_messages = [ "This is it", "Something is not right" , "Hello, how are you?", "What are you doing ", "Something never change " , "This is will be"]
        random_message <- atRandIndex random_messages
        putStrLn  $ "The random user selected is  " ++ show(username random_user)
        let newMessage  = Message{text  = random_message, from = username user, to=username random_user}
        let new_messages = ms ++ [newMessage]
        putMVar messages $! new_messages
        putMVar count $! c1 + 1
        i <- randomRIO (1000000, 5000000)
        threadDelay i
        process  user count messages winner users


main :: IO ()
main = do
    let user1 = User {username="navid", email="tddj@gmail.com", messages=[]}  
    let user2 = User {username="kami", email="kami@gmail.com", messages=[]}  
    
    let user3 = User {username="senio", email="senior@gmail.com", messages=[]}  
    let user4 = User {username="junior", email="junior@gmail.com", messages=[]}  
    
    let array_user  = [user1, user2]
    let messages= []
    count <- newMVar 0
    winner <- newEmptyMVar
    messages <- newMVar messages
    forkIO(process  user1  count messages winner [user2, user3, user4])
    forkIO(process  user2  count messages winner [user1, user3, user4])
    forkIO(process  user3  count messages winner [user1, user2, user4])
    forkIO(process  user4  count messages winner [user1, user1, user3])
    w <- takeMVar winner
    let somethigss = map to w
    let occurency = countOccurences somethigss
    let filtered  =filter(\message_uni -> to message_uni == "navid" && from message_uni == "navid") w
    -- putStrLn $ show(filtered)
    let another =  map printChatHistory $ filtered 
    putStrLn $ show(another)
    putStrLn . unlines . map printInformation $ occurency


    -- let user2 = addNewMessage  Message{text  = "anothitner", from = "ua", to="ua"} user1
    -- putStrLn(show user2)


    