module Main where

import           Control.Concurrent
import           Fork
import           System.IO.Unsafe
import           Tasks
import           Timer2


task1 :: Task Int
task1 = newTask $ do
    threadDelay 2000000  -- Wait 1 second
    putStrLn "Hello 1"
    threadDelay 1000000  -- Wait 1 second
    putStrLn "World 1"
    return 1

task2 :: Task Int
task2 = newTask $ do
    threadDelay 2000000
    putStrLn " Hello 2"
    threadDelay 2000000  -- Wait 1 second
    putStrLn "World 2"
    return 2

main1 :: IO ()
main1 = do
          forkIO  ( do  putStrLn "starting!"
                        sleepMs 2000
                        putStrLn "ending!"
                  )
          printMessagesFrom "Omar"

          return()

main2 :: IO ()
main2 = do
          n  <- run  task2 -- block until 'test3' is done
          print n

main3 :: IO ()
main3 = do  v <- run task2
            a <- run $ task1 >*-> \r -> r + 5
            print v
            print a


main4 :: IO ()
main4 = do  v <- run task2
            a <- run $ task1 >*> \r -> eitherResultIs r ( +v) undefined
            print a

main :: IO ()
main = main3
