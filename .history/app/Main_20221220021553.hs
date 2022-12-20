
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
    random_message <- atRandIndex messages
    return random_message

printInformation :: (String, Int) -> String
printInformation (username_to, count) = "The username: " ++ username_to ++ " received abouut " ++ show (count)


printChatHistory :: (Message) -> String->String
printChatHistory  message username_logged =  
    if from message == username_logged
        then "Me : "  ++ text message ++ ". "  ++ "Sent at  " ++ show(date message)
        else "" ++ from message ++ " : "  ++ text message ++ ". "   ++ "Sent at  " ++ show(date message)


exists :: Eq a => [a] -> [a] -> Bool
exists x y = any id $ (==) <$> x <*> y


filterOutCurrentUser :: User -> [User] -> [User]
filterOutCurrentUser current_user_f users = do
    filter(\user -> (username current_user_f) /= (username user)) users
    



showNumberOfReceivedMessages :: [Message] ->IO()
showNumberOfReceivedMessages messages_user= do
    let users_receivers = map to messages_user
    let occurency = countOccurences users_receivers
    putStrLn . unlines . map printInformation $ occurency
    

showHistory :: [Message] -> String -> IO()
showHistory messages_user logged_in_user = do
    let filtered  = filter(\message_uni -> to message_uni == logged_in_user || from message_uni == logged_in_user) $ sort messages_user
    putStrLn . unlines . map (\item -> printChatHistory item logged_in_user) $ filtered 


showConversationBetweenTwo :: [Message] -> String -> String -> IO()
showConversationBetweenTwo all_message_users logged_user other_user  =  do
    let filtered_messages  = filter(\message_uni ->(from message_uni == logged_user &&  to message_uni == other_user ) || (from message_uni == other_user &&  to message_uni == logged_user ) ) $ sort all_message_users 
    putStrLn . unlines . map (\item -> printChatHistory item logged_user) $ filtered_messages 

checkIfSameInterests :: User -> User -> IO Bool
checkIfSameInterests current_user random_user  = do
    let my_topics = favorite_topics current_user
    let random_topics =  (favorite_topics random_user)  
    let same_topics = exists my_topics random_topics
    -- putStrLn $ show(same_topics)
    return same_topics




main_menu:: [Message] -> IO()
main_menu final_messages =  do

    putStrLn "---------------------------------------------------------"
    putStrLn "Hello, what's your name?"  
    username <- getLine :: IO String
    putStrLn "  Welcome to your account                                "
    putStrLn "  (1) Show all messages received                         "
    putStrLn "  (2) Show all messages sent                             "
    putStrLn "  (3) Show the conversation between you and another user "
    putStrLn "  (4) Show all interests                                 "
    putStrLn "  (8) Quit                                               "
    putStrLn "---------------------------------------------------------"
    putStr "Choose an option > "
    option <- readLn :: IO Int
    case option of
        1 -> do
            -- showHistory final_messages username
            main_menu final_messages
        8 ->print "Hope you've enjoyed using the app!"




    -- putStrLn $ show(final_messages)


process :: User  -> MVar Int -> MVar [Message]->MVar [Message] -> [User]->IO()
process  user count messages final_messages_shared users = do
    -- Takevar for counting the message sent 
    count_message <- takeMVar count
    -- Messages shared across threads 
    message_sent_so_far <- takeMVar messages
    -- Exit if the 100 messages are sent
    if count_message == 20 then
        putMVar final_messages_shared $ message_sent_so_far
    else do
        putStrLn $ "--------------------------------------------------"
        putStrLn $ "The current user is  " ++ show(username user) ++ " !"
        -- Filter out the current user to prevent self message 
        let filtered_users = filterOutCurrentUser user users
        random_user <- atRandIndex filtered_users
        -- Check if they have at the least one interest in common
        same_interests <- checkIfSameInterests user random_user
  
        if same_interests == True 
            then do
                putStrLn  $ "The current user " ++ show(username user)  ++ " is sending a message to " ++ show(username random_user) ++ " !"
                -- Generate Random messages 
                random_message <- generateRandomMessage
                -- Generate Random Date
                generatedDate <- generateRandomDate

                -- Create new message 
                let newMessage  = Message{text  = random_message, from = username user, to=username random_user, date = generatedDate}
                let new_messages = message_sent_so_far ++ [newMessage]
                -- Update threads variable with new data 
                putMVar messages $! new_messages
                putMVar count $! count_message + 1
                -- Delay for thread between 1-5 seconds 
                random_seconds_delay<- randomRIO (1000000, 5000000)
                threadDelay random_seconds_delay
                -- Call Process Again 
                process  user count messages final_messages_shared users
            else do
                putStrLn  $ "The current user " ++ show(username user)  ++ " doesn't have any interest in common (no message) with " ++ show(username random_user) ++ " !"
                -- Just leave the thread variable  as before, no changes made
                putMVar messages $! message_sent_so_far
                putMVar count $! count_message
                -- Delay for thread between 1-5 seconds 
                random_seconds_delay <- randomRIO (1000000, 5000000)
                threadDelay random_seconds_delay
                process  user count messages final_messages_shared users
             

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
    let user1 = User {username="navid", email="tddj@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"]}  
    let user2 = User {username="kami", email="kami@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"]} 
    let user3 = User {username="senio", email="senior@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"]}  
    let user4 = User {username="junior", email="junior@gmail.com",favorite_topics=["football" , "videogame", "computer", "programming"]}  
    -- Array of users 
    let array_user  = [user1, user2, user3, user4]

    -- Initialiase shared variable for threads
    let messages_array = []
    count <- newMVar 0
    final_messages_shared <- newEmptyMVar
    messages <- newMVar messages_array

    -- Intiialise threads 
    _ <- forkIO(process  user1  count messages final_messages_shared array_user)
    _ <- forkIO(process  user2  count messages final_messages_shared array_user)
    _ <- forkIO(process  user3  count messages final_messages_shared array_user)
    _ <- forkIO(process  user4  count messages final_messages_shared array_user)

    -- End threads and get values of messages 
    final_messages <- takeMVar final_messages_shared
    main_menu final_messages
    
    -- Count the number of messages received for each user 
    showNumberOfReceivedMessages final_messages
    -- Get history for messages 

    main_menu  final_messages
    -- showHistory final_messages "navid"

    -- showConversationBetweenTwo final_messages "navid" "junior"


    