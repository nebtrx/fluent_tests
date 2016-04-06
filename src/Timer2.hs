module Timer2
    where

import           Control.Concurrent
import           Data.Maybe
import           System.Timer.Updatable

testTimer2 :: IO ()
testTimer2 = do
  t <- parallel (return "largoooo") $ 10^7
  s <- parallel (return "cortooo") $ 10^7
  forkIO $ waitIO t >>= print . (++"fffff") . fromJust
  forkIO $ waitIO s >>= print . (++"gggggg") . fromJust
  threadDelay $ 5 * 10 ^ 6
  renewIO t $ 6 * 10 ^ 6
  waitIO t >>= print . fromJust
  waitIO s >>= print . fromJust
