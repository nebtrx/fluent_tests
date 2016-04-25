module Tasks
    ( newTask
    , run
    , unwrap
    , uRun
    , (>*>)
    , Task
    ) where

import           Control.Concurrent.Async (Async, async, wait, waitCatch)
import           Control.Exception.Base
import           Control.Monad
import           System.IO.Unsafe

-- data TaskExecutionSummary a = TaskExecutionSummary  { result      :: Either SomeException a
--                                                     , isCompleted :: Bool
--                                                     }

data Task a = Task  { unwrap  :: IO a
                    , execute :: IO ( IO (Either SomeException a))
                    }

executeTaskInternalIO :: IO a -> IO (  IO (Either SomeException a ))
executeTaskInternalIO io = do aw <- async io
                              return (waitCatch aw)

newTask :: IO a -> Task a
newTask io = Task { unwrap = io, execute = executeTaskInternalIO io }

bind :: Task a -> (a -> Task b) -> Task b
bind aTask tobTask = Task { unwrap = io, execute = executeTaskInternalIO io }
  where
    io = buildIO aTask tobTask
      where
        buildIO :: Task a -> (a -> Task b) -> IO b
        buildIO aTask' tobTask' = do  a <- unwrap aTask'
                                      unwrap $ tobTask' a

apply :: Task ( a -> b ) -> Task a -> Task b
apply funcTask aTask = Task { unwrap = io, execute = executeTaskInternalIO io }
  where
    io = buildIO funcTask aTask
      where
        buildIO :: Task ( a -> b ) -> Task a-> IO b
        buildIO funcTask' aTask' = do func <- unwrap funcTask'
                                      a <- unwrap aTask'
                                      pure $ func a


continueWith :: Task a -> (a -> b) -> Task b
continueWith = flip fmap

(>*>) :: Task a -> (a -> b) -> Task b
(>*>) = continueWith

run :: Task a -> IO a
run t = gatherTaskResult . join $ execute t
  where
    gatherTaskResult ::  IO (Either SomeException a) -> IO a
    gatherTaskResult taskResult = taskResult >>= \ r -> case r of
                                                          Left er -> throw er
                                                          Right a -> return a

uRun :: Task a -> a
uRun = unsafePerformIO . unwrap

instance Monad Task where
  return  = newTask . return
  (>>=)   = bind

instance Applicative Task where
  pure    = return
  (<*>) = apply


instance Functor Task where
  fmap f    = apply (return f)
  (<$) a _  = pure a
