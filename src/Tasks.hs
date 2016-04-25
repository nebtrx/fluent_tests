module Tasks
    ( newTask
    , func
    , unwrap
    , uRun
    , (>*>)
    , Task
    ) where

import           Control.Concurrent.Async (Async, async, wait)
import           Control.Exception.Base
import           Control.Monad
import           System.IO.Unsafe

data TaskExecutionSummary a = TaskExecutionSummary  { result      :: a
                                                    , isCompleted :: Bool
                                                    , error       :: IOError
                                                    }

newtype Task a = Task { func :: IO (IO a) }

tryWait :: Exception e => Async a -> IO (Either e a)
tryWait = try . wait

newTask :: IO a -> Task a
newTask io = Task $ do aw <- async io
                       return (wait aw)

bind :: Task a -> (a -> Task b) -> Task b
bind m f = newTask $ do a <- unwrap m
                        join $ func $ f a

apply :: Task ( a -> b ) -> Task a -> Task b
apply fa a = newTask $ do f <- unwrap fa
                          a <- unwrap a
                          join $ func $ pure $ f a

continueWith :: Task a -> (a -> b) -> Task b
continueWith = flip fmap

(>*>) :: Task a -> (a -> b) -> Task b
(>*>) = continueWith

unwrap :: Task a -> IO a
unwrap m = join $ func m

uRun :: Task a -> a
uRun = unsafePerformIO . unwrap

instance Monad Task where
  return a = Task $ return (return a)
  (>>=)    = bind

instance Applicative Task where
  pure    = return
  (<*>) = apply


instance Functor Task where
  fmap f    = apply (return f)
  (<$) a _  = pure a
