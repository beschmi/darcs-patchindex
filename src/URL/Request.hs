module URL.Request where

import Data.Map ( Map )
import Data.List ( delete )
import Foreign.C.Types ( CInt )

data UrlRequest = UrlRequest { url :: String
                             , file :: FilePath
                             , cachable :: Cachable
                             , priority :: Priority }

data Cachable = Cachable | Uncachable | MaxAge !CInt
                deriving (Show, Eq)

data UrlState = UrlState { inProgress :: Map String ( FilePath
                                                    , [FilePath]
                                                    , Cachable )
                         , waitToStart :: Q String
                         , pipeLength :: Int
                         , randomJunk :: String }

data Q a = Q [a] [a]

readQ :: Q a -> Maybe (a, Q a)
readQ (Q (x:xs) ys) = Just (x, Q xs ys)
readQ (Q [] ys) = do x:xs <- Just $ reverse ys
                     Just (x, Q xs [])

insertQ :: a -> Q a -> Q a
insertQ y (Q xs ys) = Q xs (y:ys)

pushQ :: a -> Q a -> Q a
pushQ x (Q xs ys) = Q (x:xs) ys

deleteQ :: Eq a => a -> Q a -> Q a
deleteQ x (Q xs ys) = Q (delete x xs) (delete x ys)

elemQ :: Eq a => a -> Q a -> Bool
elemQ x (Q xs ys) = x `elem` xs || x `elem` ys

emptyQ :: Q a
emptyQ = Q [] []

nullQ :: Q a -> Bool
nullQ (Q [] []) = True
nullQ _         = False

data Priority = High | Low deriving Eq

-- | Data type to represent a connection error.
-- The following are the codes from libcurl
-- which map to each of the constructors:
-- * 6  -> CouldNotResolveHost : The remote host was not resolved.
-- * 7  -> CouldNotConnectToServer : Failed to connect() to host or proxy.
-- * 28 -> OperationTimeout: the specified time-out period was reached.
data ConnectionError = CouldNotResolveHost     |
                       CouldNotConnectToServer |
                       OperationTimeout
               deriving (Eq, Read, Show)
