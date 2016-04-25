module Main where

import           Control.Concurrent
import           Fork
import           System.IO.Unsafe
import           Tasks
import           Timer2


test1 :: Task Int
test1 = newTask $ do
    threadDelay 1000000  -- Wait 1 second
    putStrLn "Hello,"
    return 1

test2 :: Task Int
test2 = newTask $ do
    threadDelay 3000000
    putStrLn " world!"
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
          n   <- unwrap  test2 -- block until 'test3' is done
          print n

main3 :: IO ()
main3 = do  a <- unwrap $ test1 >*> \r -> r + uRun test2
            print a

main :: IO ()
main = main3
