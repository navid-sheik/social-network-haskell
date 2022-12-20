
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Main (main) where

import Lib

import Control.Concurrent
import System.Random
import System.IO
import Data.List
import Control.Arrow ((&&&))
import Data.Time.Calendar
import Data.Time


data User = User{
    username :: String,
    email :: String,
    favorite_topics :: [String],
    messages::[Message]
}deriving (Show)

data Message  = Message{
    text :: String,
    from :: String,
    to :: String,
    date :: Day
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



generateRandomDate :: IO Day
generateRandomDate = do
    let random_dates = [fromGregorian 2022 1 1, fromGregorian 2022 1 3 .. fromGregorian 2023 1 1]
    pickedDate  <- atRandIndex random_dates
    return pickedDate


generateRandomMessage :: IO String
generateRandomMessage = do
    let messages = [ "This is it", "Something is not right" , "Hello, how are you?", "What are you doing ", "Something never change " , "This is will be"]
    random <- atRandIndex messages
    return random

printInformation :: (String, Int) -> String
printInformation (username, count) = "The username: " ++ username ++ " received abouut " ++ show (count)


printChatHistory :: (Message) -> String->String
printChatHistory  message username =  
    if from message == username
        then "Me : "  ++ text message ++ " ."  ++ "Sent at  " ++ show(date message)
        else "" ++ from message ++ " : "  ++ text message ++ " ."   ++ "Sent at  " ++ show(date message)


exists :: Eq a => [a] -> [a] -> Bool
exists x y = any id $ (==) <$> x <*> y


filterOutCurrentUser :: User -> [User] -> [User]
filterOutCurrentUser userf users = do
    filter(\user -> (username userf) /= (username user)) users
    



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
        let filtered_users = filterOutCurrentUser user users
        random_user <- atRandIndex filtered_users
        putStrLn $ show(filtered_users)
        let my_topics = favorite_topics user
        let random_topics =  (favorite_topics random_user)  
        let same_topics = exists my_topics random_topics
        putStrLn $ show(same_topics)
        if same_topics == True 
            then do
                let random_messages = [ "This is it", "Something is not right" , "Hello, how are you?", "What are you doing ", "Something never change " , "This is will be"]
                random_message <- atRandIndex random_messages
                putStrLn  $ "The random user selected is  " ++ show(username random_user)
                generatedDate <- generateRandomDate
                putStrLn $ show(generatedDate)
                let newMessage  = Message{text  = random_message, from = username user, to=username random_user, date = generatedDate}
                let new_messages = ms ++ [newMessage]
                putMVar messages $! new_messages
                putMVar count $! c1 + 1
                i <- randomRIO (1000000, 5000000)
                threadDelay i
                process  user count messages winner users
            else do
                putStrLn $ "Not same "
                putMVar messages $! ms
                putMVar count $! c1
                i <- randomRIO (1000000, 5000000)
                threadDelay i
                process  user count messages winner users
             


main :: IO ()
main = do
    let user1 = User {username="navid", email="tddj@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"],  messages=[]}  
    let user2 = User {username="kami", email="kami@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"], messages=[]}  
    
    let user3 = User {username="senio", email="senior@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"], messages=[]}  
    let user4 = User {username="junior", email="junior@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"], messages=[]}  
    
    let array_user  = [user1, user2, user3, user4]
    let messages= []
    count <- newMVar 0
    winner <- newEmptyMVar
    messages <- newMVar messages
    forkIO(process  user1  count messages winner array_user)
    forkIO(process  user2  count messages winner array_user)
    forkIO(process  user3  count messages winner array_user)
    forkIO(process  user4  count messages winner array_user)
    w <- takeMVar winner
    let somethigss = map to w
    let occurency = countOccurences somethigss
    let filtered  = filter(\message_uni -> to message_uni == "navid" || from message_uni == "navid") $ sort w
    -- putStrLn $ show(filtered)
    putStrLn . unlines . map (\item -> printChatHistory item "navid") $ filtered 
    putStrLn . unlines . map printInformation $ occurency


    -- let user2 = addNewMessage  Message{text  = "anothitner", from = "ua", to="ua"} user1
    -- putStrLn(show user2)


    