import           Control.Concurrent
import           Control.Concurrent.STM

interval :: Int
interval = 1000000

main :: IO()
main = do
    timer1 <- newTimer (4 * interval)
    waitTimer timer1
    putStrLn "Timer 1 expired"

    timer2 <- newTimer (1 * interval)
    _ <- ($)
          forkIO $ do
              waitTimer timer2
              putStrLn "Timer 2 expired"
    stopTimer timer2
    putStrLn "Timer 2 stopped"

data State = Start | Stop
type Timer = (TVar State, TMVar ())

waitTimer :: Timer -> IO ()
waitTimer (_, timer) = atomically $ readTMVar timer

stopTimer :: Timer -> IO ()
stopTimer (state, _) = atomically $ writeTVar state Stop

newTimer :: Int -> IO Timer
newTimer n = do
    state <- atomically $ newTVar Start
    timer <- atomically  newEmptyTMVar
    _ <- ($)
          forkIO $ do
              threadDelay n
              atomically $ do
                  runState <- readTVar state
                  case runState of
                      Start -> putTMVar timer ()
                      Stop  -> return ()
    return (state, timer)
