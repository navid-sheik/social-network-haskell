{-|
Module      : Main
Copyright   : (c)  Navid Sheikh, 2022
-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Main (
    main, 
    process,
    logginUser,
    main_menu,
    countOccurences,
    atRandIndex, 
    generateRandomDate,
    generateRandomMessage,
    printInformation,
    printChatHistory,
    exists,
    filterOutCurrentUser,
    showNumberOfReceivedMessages,
    showHistory,
    showConversationBetweenTwo,
    checkIfSameInterests
 



    


    ) where

-- | import the necessary libraries, data type and function 
import Control.Concurrent
import System.Random
import System.IO
import Data.List
import Control.Arrow ((&&&))
import Data.Time.Calendar
import Data.Time
import Types




-- * Helper functions 


-- | Used to count the number of occurence, 
-- in this case the number of messages received for each user
countOccurences :: Ord a => [a] -> [(a, Int)]
countOccurences = map (head &&& length) . group . sort

 
-- | Used to select a random item from a list
atRandIndex :: [a] -> IO a  -- note that this is gives itself an IO action
atRandIndex l = do
    i <- randomRIO (0, length l - 1)
    return $ l !! i


-- | Pick a random date between 2022/01/01 - 2023/01/01
generateRandomDate :: IO Day
generateRandomDate = do
    let random_dates = [fromGregorian 2022 1 1, fromGregorian 2022 1 3 .. fromGregorian 2023 1 1]
    pickedDate  <- atRandIndex random_dates
    return pickedDate

-- | Pick a random message from the list
generateRandomMessage :: IO String
generateRandomMessage = do
    let messages = [ "Well hello there", "Where do you come from?" , "What's your name?", "My name is just unknown", "Indeed" , "Here we go", "what you looking for?", "Nothing much", "Is there anything you want try?","Yes, I love it", "I really like this phone, you should buy it", "Cool", "There is nothing to do", "I am bored"]
    random_message <- atRandIndex messages
    return random_message

-- | Used assists printing when counting the number of messages received by each user.
printInformation :: (String, Int) -> String
printInformation (username_to, count) = "The username: " ++ username_to ++ " received abouut " ++ show (count)

-- | Used to print chat history between the logged in user and the others 
printChatHistory :: (Message) -> String->String
printChatHistory  message username_logged =  
    if from message == username_logged
        then "Me : "  ++ text message ++ ". "  ++ "Sent at  " ++ show(date message)
        else "" ++ from message ++ " : "  ++ text message ++ ". "   ++ "Sent at  " ++ show(date message)

-- | Used to check if an item of one list contain at least one is present in other list 
exists :: Eq a => [a] -> [a] -> Bool
exists x y = any id $ (==) <$> x <*> y


-- | Used to filter out current users and return other users,
-- and help to prevent self-messaging 
filterOutCurrentUser :: User -> [User] -> [User]
filterOutCurrentUser current_user_f users = do
    filter(\user -> (username current_user_f) /= (username user)) users
    


-- | Display the number of messages received for each users 
showNumberOfReceivedMessages :: [Message] ->IO()
showNumberOfReceivedMessages messages_user= do
    let users_receivers = map to messages_user
    let occurency = countOccurences users_receivers
    putStrLn . unlines . map printInformation $ occurency
    
-- | Show between the current user and all other users 
showHistory :: [Message] -> String -> IO()
showHistory messages_user logged_in_user = do
    let filtered  = filter(\message_uni -> to message_uni == logged_in_user || from message_uni == logged_in_user) $ sort messages_user
    putStrLn . unlines . map (\item -> printChatHistory item logged_in_user) $ filtered 

-- | Show a conversation between two users 
showConversationBetweenTwo :: [Message] -> String -> String -> IO()
showConversationBetweenTwo all_message_users logged_user other_user  =  do
    let filtered_messages  = filter(\message_uni ->(from message_uni == logged_user &&  to message_uni == other_user ) || (from message_uni == other_user &&  to message_uni == logged_user ) ) $ sort all_message_users 
    putStrLn . unlines . map (\item -> printChatHistory item logged_user) $ filtered_messages 

-- | Check if two users have at least one common interest
checkIfSameInterests :: User -> User -> IO Bool
checkIfSameInterests current_user random_user  = do
    let my_topics = favorite_topics current_user
    let random_topics =  (favorite_topics random_user)  
    let same_topics = exists my_topics random_topics
    return same_topics


-- * Main functions 

-- | Login the user
--  Check credentials
--  Invoke menu that allow the user to check messaging history
logginUser :: [User] ->[Message]-> IO()
logginUser all_users messages_all_users= do
    putStrLn "-----------------------------------------------"  
    putStrLn "LOGIN TO YOUR ACCOUNT"  
    putStr "Enter your username or type(quit) >"  
    -- get username
    username_current <- getLine :: IO String
    if username_current /= "quit"
        then do
            -- Search users in the array 
            let users_filter  =  filter(\user -> username user == username_current) all_users
            if (null users_filter)
                then do
                    -- User doesn't exists in out system
                    putStrLn $ "User doesn't exists, try again!"
                    logginUser all_users messages_all_users
                else do
                    -- Check login details  
                    let user_logged_obj =  head users_filter
                    putStr $ "Enter password for  " ++ show(username user_logged_obj)  ++ " > "
                    password_for_user <- getLine :: IO String
                    let isLoggedIn = password_for_user == password user_logged_obj
                    if isLoggedIn == True 
                        then do 
                            main_menu messages_all_users username_current  user_logged_obj
                        else do
                            putStrLn $ "Incorrect password or username, try again!"
                            logginUser all_users messages_all_users
                  

        else do
            putStrLn $ "Quitted! Thank your for using our app"  
    
-- | Main menu that allow logged in user to see his/her conversations
main_menu:: [Message] -> String -> User ->IO()
main_menu final_messages username user_obj=  do

    putStrLn "---------------------------------------------------------"
    putStrLn "  Welcome to your account                                "
    putStrLn "  (1) Show all messages received & received              "
    putStrLn "  (2) Show the conversation between you and another user "
    putStrLn "  (3) Log out                                            "
    putStrLn "---------------------------------------------------------"
    putStr "Choose an option > "
    option <- readLn :: IO Int
    case option of
        1 -> do
            showHistory final_messages username
            main_menu final_messages username user_obj
        2 -> do
            putStrLn "Enter the name of other user"  
            other_user <- getLine :: IO String
            showConversationBetweenTwo final_messages username other_user
            main_menu final_messages username user_obj
        3 ->print "Logged you out, thank you for using our app"



-- | Create a new thread that allow a user to send a message to random user
--  The user must have at least one common interest
--  The process terminate once all the processes have sent 100 messages in total
--  Delay is set 1-5 seconds

process :: User  -> MVar Int -> MVar [Message]->MVar [Message] -> [User]->IO()
process  user count messages final_messages_shared users = do
    -- Takevar for counting the message sent 
    count_message <- takeMVar count
    -- Messages shared across threads 
    message_sent_so_far <- takeMVar messages
    -- Exit if the 100 messages are sent
    if count_message == 100 then
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
             

-- | Main functions that initiliase users, threads and invoke the other main functions
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    -- Initialize users
    let user1 = User {username="user1", email="user1@gmail.com",password="test1", favorite_topics=["interest1" , "interest2", "interest3", "interest4"]}  
    let user2 = User {username="user2", email="user2@gmail.com",password="test2", favorite_topics=["interest1"]} 
    let user3 = User {username="user3", email="user3@gmail.com",password="test3", favorite_topics=["interest2" ]}  
    let user4 = User {username="user4", email="user4@gmail.com",password="test4", favorite_topics=["interest1" ,  "interest5" ]}  
    let user5 = User {username="user5", email="user5@gmail.com",password="test5", favorite_topics=["interest3", "interest4", "interest5"]}  
    let user6 = User {username="user6", email="user6@gmail.com",password="test6", favorite_topics=["interest2" , "interest3", "interest4", "interest5"]} 
    let user7 = User {username="user7", email="user7@gmail.com",password="test7", favorite_topics=["interest1" , "interest3","interest5" ]}  
    let user8 = User {username="user8", email="user8@gmail.com",password="test8", favorite_topics=["interest3" ]} 
    let user9 = User {username="user9", email="user9@gmail.com",password="test9", favorite_topics=["interest1" , "interest3", "interest4"]} 
    let user10 = User {username="user10", email="user10@gmail.com",password="test10", favorite_topics=["interest1" , "interest3", "interest2", "interest4", "interest5" ]}  
    let array_user  = [user1, user2, user3, user4, user5, user6, user7, user8, user9, user10]

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
    _ <- forkIO(process  user5  count messages final_messages_shared array_user)
    _ <- forkIO(process  user6  count messages final_messages_shared array_user)
    _ <- forkIO(process  user7  count messages final_messages_shared array_user)
    _ <- forkIO(process  user8  count messages final_messages_shared array_user)
    _ <- forkIO(process  user9  count messages final_messages_shared array_user)
    _ <- forkIO(process  user10  count messages final_messages_shared array_user)


    -- End threads and get values of messages 
    final_messages <- takeMVar final_messages_shared
    
    -- Count the number of messages received for each user 
    showNumberOfReceivedMessages final_messages
    
    -- Loggin and enter account 
    logginUser array_user final_messages



    