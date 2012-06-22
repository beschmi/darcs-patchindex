{-# OPTIONS_GHC -fno-warn-dodgy-imports #-} -- needed for GHC 7.0/7.2
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module      : Exec
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable
--
-- Various utility functions that do not belong anywhere else.

module Darcs.Utils
    (
      nubsort
    , breakCommand
    , showHexLen
    , maybeGetEnv
    -- * Monads
    , firstJustIO
    -- * User prompts
    , askEnter
    , askUser
    , askUserListItem
    , PromptConfig(..)
    , promptYorn
    , promptChar
    -- * Errors and exceptions
    , catchall
    , clarifyErrors
    , prettyException
    , prettyError
    -- * Files and directories
    , getFileStatus
    , withCurrentDirectory
    , withUMask
    -- * Tree filtering.
    , filterFilePaths
    , filterPaths
    -- * Tree lookup.
    , treeHas
    , treeHasDir
    , treeHasFile
    , treeHasAnycase
    -- * Text formatting.
    , formatText
    , formatParas
    , formatPara
    ) where


import Prelude hiding ( catch )

import Control.Arrow ( first )
import Control.Exception.Extensible
             ( bracket, bracket_, catch,
               SomeException, Exception(fromException) )
import Control.Monad ( when, forM )
import Control.Monad.Error( MonadError )
import Control.Monad.State.Strict( gets )

import qualified Data.ByteString.Char8 as BSC

import Data.Char ( toUpper, toLower, isSpace )
import Data.List ( group, sort, intercalate )
import Data.Maybe ( isJust )

import Foreign.C.String ( CString, withCString )
import Foreign.C.Error ( throwErrno )
import Foreign.C.Types ( CInt(..) )

import Storage.Hashed.Monad( withDirectory, fileExists, directoryExists
                           , virtualTreeMonad, currentDirectory
                           , TreeMonad )
import qualified Storage.Hashed.Monad as HS ( exists, tree )

import Storage.Hashed.Tree( Tree, listImmediate, findTree )

import System.Console.Haskeline ( runInputT, defaultSettings, getInputLine,
                                  getInputChar, outputStr, outputStrLn )
import System.Environment ( getEnv )
import System.IO.Error ( isUserError, ioeGetErrorString
                       , isDoesNotExistError, ioeGetFileName )
import System.Posix.Files( getSymbolicLinkStatus, FileStatus )

import Darcs.Path( AnchoredPath(..), Name(..), isPrefix, floatPath,
                   FilePathLike, getCurrentDirectory, setCurrentDirectory, toFilePath )
import Darcs.SignalHandler ( catchNonSignal )

import Numeric ( showHex )
import Progress ( withoutProgress )


showHexLen :: (Integral a, Show a)
           => Int
           -> a
           -> String
showHexLen n x = let s = showHex x ""
                 in replicate (n - length s) ' ' ++ s


catchall :: IO a
         -> IO a
         -> IO a
a `catchall` b = a `catchNonSignal` (\_ -> b)


maybeGetEnv :: String
            -> IO (Maybe String)
maybeGetEnv s = fmap Just (getEnv s) `catchall` return Nothing -- err can only be isDoesNotExist


-- | The firstJustM returns the first Just entry in a list of monadic
-- operations. This is close to `listToMaybe `fmap` sequence`, but the sequence
-- operator evaluates all monadic members of the list before passing it along
-- (i.e. sequence is strict). The firstJustM is lazy in that list member monads
-- are only evaluated up to the point where the first Just entry is obtained.
firstJustM :: Monad m
           => [m (Maybe a)]
           -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (e:es) = e >>= (\v -> if isJust v then return v else firstJustM es)


-- | The firstJustIO is a slight modification to firstJustM: the entries in the
-- list must be IO monad operations and the firstJustIO will silently turn any
-- monad call that throws an exception into Nothing, basically causing it to be
-- ignored.
firstJustIO :: [IO (Maybe a)]
            -> IO (Maybe a)
firstJustIO = firstJustM . map (`catchall` return Nothing)


clarifyErrors :: IO a
              -> String
              -> IO a
clarifyErrors a e = a `catch` (\x -> fail $ unlines [prettyException x,e])


prettyException :: SomeException
                -> String
prettyException e | Just ioe <- fromException e, isUserError ioe = ioeGetErrorString ioe
prettyException e | Just ioe <- fromException e, isDoesNotExistError ioe =
  case ioeGetFileName ioe of
    Just f  -> f ++ " does not exist"
    Nothing -> show e
prettyException e = show e


prettyError :: IOError -> String
prettyError e | isUserError e = ioeGetErrorString e
              | otherwise = show e



withCurrentDirectory :: FilePathLike p
                     => p
                     -> IO a
                     -> IO a
withCurrentDirectory name m =
    bracket
        (do cwd <- getCurrentDirectory
            when (toFilePath name /= "") (setCurrentDirectory name)
            return cwd)
        (\oldwd -> setCurrentDirectory oldwd `catchall` return ())
        (const m)


foreign import ccall unsafe "umask.h set_umask" set_umask
    :: CString -> IO CInt
foreign import ccall unsafe "umask.h reset_umask" reset_umask
    :: CInt -> IO CInt


withUMask :: String
          -> IO a
          -> IO a
withUMask umask job =
    do rc <- withCString umask set_umask
       when (rc < 0) (throwErrno "Couldn't set umask")
       bracket_
           (return ())
           (reset_umask rc)
           job


-- | Ask the user for a line of input.
askUser :: String    -- ^ The prompt to display
        -> IO String -- ^ The string the user entered.
askUser prompt = withoutProgress $ runInputT defaultSettings $
                    getInputLine prompt
                        >>= maybe (error "askUser: unexpected end of input") return

-- | Ask the user to press Enter
askEnter :: String  -- ^ The prompt to display
         -> IO ()
askEnter prompt = askUser prompt >> return ()

-- | @askUserListItem prompt xs@ enumerates @xs@ on the screen, allowing
--   the user to choose one of the items
askUserListItem :: String
                -> [String]
                -> IO String
askUserListItem prompt xs = withoutProgress $ runInputT defaultSettings $ do
    outputStr . unlines $ zipWith (\n x -> show n ++ ". " ++ x) [1::Int ..] xs
    loop
  where
    loop = do
      answer <- getInputLine prompt
                  >>= maybe (error "askUser: unexpected end of input") return
      case maybeRead answer of
        Just n | n > 0 && n <= length xs -> return (xs !! (n-1))
        _ -> outputStrLn "Invalid response, try again!" >> loop


maybeRead :: Read a
          => String
          -> Maybe a
maybeRead s = case reads s of
    [(x, rest)] | all isSpace rest -> Just x
    _         -> Nothing


breakCommand :: String -> (String, [String])
breakCommand s = case words s of
                   (arg0:args) -> (arg0,args)
                   [] -> (s,[])

nubsort :: Ord a
        => [a]
        -> [a]
nubsort = map head . group . sort

data PromptConfig = PromptConfig { pPrompt :: String
                                 , pBasicCharacters :: [Char]
                                 , pAdvancedCharacters :: [Char] -- ^ only shown on help
                                 , pDefault :: Maybe Char
                                 , pHelp    :: [Char]
                                 }


-- | Prompt the user for a yes or no
promptYorn :: [Char] -> IO Bool
promptYorn p = (== 'y') `fmap` promptChar (PromptConfig p "yn" [] Nothing [])


promptChar :: PromptConfig -> IO Char
promptChar (PromptConfig p basic_chs adv_chs md help_chs) =
  withoutProgress $ runInputT defaultSettings loopChar
 where
 chs = basic_chs ++ adv_chs
 loopChar = do
    let chars = setDefault (basic_chs ++ (if null adv_chs then "" else "..."))
        prompt = p ++ " [" ++ chars ++ "]" ++ helpStr
    a <- getInputChar prompt >>= maybe (error "promptChar: unexpected end of input")
                                    return
    case () of
     _ | a `elem` chs                   -> return a
       | a == ' '                       -> maybe tryAgain return md
       | a `elem` help_chs              -> return a
       | otherwise                      -> tryAgain
 helpStr = case help_chs of
           []                      -> ""
           (h:_) | null adv_chs    -> ", or " ++ (h:" for help: ")
                 | otherwise       -> ", or " ++ (h:" for more options: ")
 tryAgain = do outputStrLn "Invalid response, try again!"
               loopChar
 setDefault s = case md of Nothing -> s
                           Just d  -> map (setUpper d) s
 setUpper d c = if d == c then toUpper c else c


-- | Construct a filter from a list of AnchoredPaths, that will accept any path
-- that is either a parent or a child of any of the listed paths, and discard
-- everything else.
filterPaths :: [AnchoredPath]
            -> AnchoredPath
            -> t
            -> Bool
filterPaths files p _ = any (\x -> x `isPrefix` p || p `isPrefix` x) files


-- | Same as 'filterPath', but for ordinary 'FilePath's (as opposed to
-- AnchoredPath).
filterFilePaths :: [FilePath]
                -> AnchoredPath
                -> t
                -> Bool
filterFilePaths = filterPaths . map floatPath


getFileStatus :: FilePath
              -> IO (Maybe FileStatus)
getFileStatus f =
  Just `fmap` getSymbolicLinkStatus f `catchall` return Nothing


treeHasAnycase :: (MonadError e m, Functor m, Monad m)
               => Tree m
               -> FilePath
               -> m Bool
treeHasAnycase tree path =
    fst `fmap` virtualTreeMonad (existsAnycase $ floatPath path) tree


existsAnycase :: (MonadError e m, Functor m, Monad m)
              => AnchoredPath
              -> TreeMonad m Bool
existsAnycase (AnchoredPath []) = return True
existsAnycase (AnchoredPath (Name x:xs)) = do
  do wd <- currentDirectory
     Just tree <- gets (flip findTree wd . HS.tree)
     let subs = [ AnchoredPath [Name n] | (Name n, _) <- listImmediate tree,
                                          BSC.map toLower n == BSC.map toLower x ]
     or `fmap` forM subs (\path -> do
       file <- fileExists path
       if file then return True
               else withDirectory path (existsAnycase $ AnchoredPath xs))


treeHas :: (MonadError e m, Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHas tree path = fst `fmap` virtualTreeMonad (HS.exists $ floatPath path) tree

treeHasDir :: (MonadError e m, Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHasDir tree path = fst `fmap` virtualTreeMonad (directoryExists $ floatPath path) tree

treeHasFile :: (MonadError e m, Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHasFile tree path = fst `fmap` virtualTreeMonad (fileExists $ floatPath path) tree

-- |Take a list of paragraphs and format them to the given line length, with
-- a blank line between paragraphs.
formatText :: Int -> [String] -> String
formatText linelen = unlines . formatParas linelen

formatParas :: Int -> [String] -> [String]
formatParas linelen = intercalate [""] .
                      map (map unwords . formatPara linelen . words)

-- |Take a list of words and split it up so that each chunk fits into the specified width
-- when spaces are included. Any words longer than the specified width end up in a chunk
-- of their own.
formatPara :: Int -> [[a]] -> [[[a]]]
formatPara w = para'
  where para' [] = []
        para' xs = uncurry (:) $ para'' w xs
        para'' r (x:xs) | w == r || length x < r = first (x:) $ para'' (r - length x - 1) xs
        para'' _ xs = ([], para' xs)
