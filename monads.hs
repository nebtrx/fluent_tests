data Fluent a = Fluent a

instance Show a => Show (Fluent a) where
  show (Fluent a) = show a

instance Monad Fluent where
  return = Fluent
  Fluent x >>= f = f x

instance Applicative Fluent where
  pure = Fluent
  Fluent f <*> Fluent a =  Fluent $ f a

instance Functor Fluent where
  fmap f (Fluent a) = Fluent $ f a

with :: String -> (String -> Fluent String)
with src x = Fluent (x ++ src)

testFluent :: Fluent String
testFluent = Fluent "hola"
              >>= with " cabron"
              >>= with " hijoeputa"

main :: IO()
main = print $ show testFluent
