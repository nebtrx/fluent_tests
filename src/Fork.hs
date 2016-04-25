module Fork
    (printMessagesFrom
    , sleepMs
    ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Data.Foldable      (for_)

sleepMs :: Int -> IO()
sleepMs n = threadDelay (n * 1000)

printMessagesFrom :: String -> IO()
printMessagesFrom name = for_ [1..3] printMessage
  where printMessage i = do putStrLn (name ++ " number " ++ show i)
                            sleepMs 200
