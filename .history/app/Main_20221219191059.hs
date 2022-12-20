
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

import Types




append :: Message -> [Message] -> [Message]
append a [] = [a]
append a (x:xs) = x : append a xs

addNewMessage :: Message  -> User -> User
addNewMessage  message  user= 
    user { messages = existinsmessages ++ [message] }
    where existinsmessages  = messages user


countOccurences :: Ord a => [a] -> [(a, Int)]
countOccurences = map (head &&& length) . group . sort

 
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
    



showNumberOfReceivedMessages :: [Message] ->IO()
showNumberOfReceivedMessages messages_user= do
    let somethigss = map to messages_user
    let occurency = countOccurences somethigss
    putStrLn . unlines . map printInformation $ occurency
    

showHistory :: [Message] -> String -> IO()
showHistory messages_user logged_in_user = do
    let filtered  = filter(\message_uni -> to message_uni == logged_in_user || from message_uni == logged_in_user) $ sort messages_user
    putStrLn . unlines . map (\item -> printChatHistory item logged_in_user) $ filtered 


checkIfSameInterests :: User -> User -> Bool
checkIfSameInterests current_user random_user  = do
    let my_topics = favorite_topics current_user
    let random_topics =  (favorite_topics random_user)  
    let same_topics = exists my_topics random_topics
    return same_topics


process :: User  -> MVar Int -> MVar [Message]->MVar [Message] -> [User]->IO()
process  user count messages winner users = do
    -- Takevar for counting the message sent 
    c1 <- takeMVar count
    -- Messages shared across threads 
    ms <- takeMVar messages
    -- Helping messages 
    putStrLn $ "The messages are" ++ show(ms)
    putStrLn $ "The count is " ++ show(c1)

    -- Exit if the 100 messages are sent
    if c1 == 10 then
        putMVar winner $ ms
    else do
        putStrLn $ "--------------------------------------------------"
        putStrLn $ "The current user is  " ++ show(username user) ++ " !"
        -- Filter out the current user to prevent self message 
        let filtered_users = filterOutCurrentUser user users
        random_user <- atRandIndex filtered_users
        -- putStrLn $ show(filtered_users)
        -- let my_topics = favorite_topics user
        -- let random_topics =  (favorite_topics random_user)  
        -- let same_topics = exists my_topics random_topics
        -- putStrLn $ show(same_topics)
        same_interests <- checkIfSameInterests user random_user
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
             

-- menu :: IO()
-- menu user_loggedIn= do 
--     putStrLn "---------------------------------------------------------"
--     putStrLn "  WELCOME TO THE DASHBOARD                               "
--     putStrLn "  (1) Show all the message                               "
--     putStrLn "  (2) Display shows                                      "
--     putStrLn "  (3) Show summary of a show (ID)                        "
--     putStrLn "  (4) Show all programs that have ENDED                  "
--     putStrLn "  (5) Show all movies premiered before a certain YEAR    "
--     putStrLn "  (6) Guess the shows - GAME                             "
--     putStrLn "  (7) Count the number of shows that are still RUNNING   "
--     putStrLn "  (8) Quit                                               "
--     putStrLn "---------------------------------------------------------"

main :: IO ()
main = do
    -- Initialize users
    let user1 = User {username="navid", email="tddj@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"],  messages=[]}  
    let user2 = User {username="kami", email="kami@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"], messages=[]} 
    let user3 = User {username="senio", email="senior@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"], messages=[]}  
    let user4 = User {username="junior", email="junior@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"], messages=[]}  
    -- Array of users 
    let array_user  = [user1, user2, user3, user4]

    -- Initialiase shared variable for threads
    let messages= []
    count <- newMVar 0
    winner <- newEmptyMVar
    messages <- newMVar messages

    -- Intiialise threads 
    forkIO(process  user1  count messages winner array_user)
    forkIO(process  user2  count messages winner array_user)
    forkIO(process  user3  count messages winner array_user)
    forkIO(process  user4  count messages winner array_user)

    -- End threads and get values of messages 
    final_messages <- takeMVar winner

    -- Count the number of messages received for each user 
    -- let somethigss = map to final_messages
    -- let occurency = countOccurences somethigss
    -- putStrLn . unlines . map printInformation $ occurency
    showNumberOfReceivedMessages final_messages
    -- Get history for messages 
    showHistory final_messages "navid"
    -- let filtered  = filter(\message_uni -> to message_uni == "navid" || from message_uni == "navid") $ sort final_messages
    -- putStrLn . unlines . map (\item -> printChatHistory item "navid") $ filtered 



    