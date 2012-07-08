--  Copyright (C) 2002-2004 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Darcs.UI.Arguments
    ( DarcsFlag( .. )
    , flagToString
    , applyDefaults
    , nubOptions
    , maxCount
    , isin
    , arein
    , fixFilePathOrStd
    , fixUrl
    , fixUrlFlag
    , fixSubPaths
    , maybeFixSubPaths
    , DarcsAtomicOption( .. )
    , atomicOptions
    , DarcsOption( .. )
    , optionFromDarcsOption
    , help
    , listOptions
    , anyVerbosity
    , disable
    , restrictPaths
    , notest
    , test
    , workingRepoDir
    , remoteRepo
    , leaveTestDir
    , possiblyRemoteRepoDir
    , getRepourl
    , author
    , getAuthor
    , getEasyAuthor
    , getSendmailCmd
    , fileHelpAuthor
    , environmentHelpEmail
    , patchnameOption
    , distnameOption
    , logfile
    , rmlogfile
    , fromOpt
    , subject
    , getSubject
    , charset
    , getCharset
    , inReplyTo
    , getInReplyTo
    , target
    , ccSend
    , ccApply
    , getCc
    , output
    , outputAutoName
    , recursive
    , patchFormatChoices
    , upgradeFormat
    , useWorkingDir
    , askdeps
    , ignoretimes
    , lookforadds
    , askLongComment
    , keepDate
    , sendmailCmd
    , environmentHelpSendmail
    , sign
    , verify
    , editDescription
    , reponame
    , creatorhash
    , applyConflictOptions
    , reply
    , pullConflictOptions
    , useExternalMerge
    , depsSel
    , nocompress
    , uncompressNocompress
    , repoCombinator
    , optionsLatex
    , reorderPatches
    , noskipBoring
    , allowProblematicFilenames
    , applyas
    , humanReadable
    , machineReadable
    , changesReverse
    , onlyToFiles
    , changesFormat
    , matchOneContext
    , matchOneNontag
    , matchMaxcount
    , sendToContext
    , getContext
    , pipeInteractive
    , allInteractive
    , allPipeInteractive
    , summary
    , unified
    , tokens
    , partial
    , diffCmdFlag
    , diffflags
    , unidiff
    , xmloutput
    , pauseForGui
    , forceReplace
    , dryRun
    , dryRunNoxml
    , matchOne
    , matchSeveral
    , matchRange
    , matchSeveralOrRange
    , happyForwarding
    , matchSeveralOrLast
    , setDefault
    , setScriptsExecutableOption
    , testStrategy
    , sibling
    , flagsToSiblings
    , relink
    , files
    , directories
    , pending
    , posthookCmd
    , posthookPrompt
    , prehookCmd
    , prehookPrompt
    , nullFlag
    , umaskOption
    , storeInMemory
    , patchSelectFlag
    , networkOptions
    , noCache
    , allowUnrelatedRepos
    , checkOrRepair
    , justThisRepo
    , optimizePristine
    , optimizeHTTP
    , getOutput
    , usePacks
    , amendUnrecord
    , patchIndex
    , noPatchIndex
    ) where

import System.Console.GetOpt
import System.Directory ( doesDirectoryExist )

import Data.List ( nub, intercalate )
import Data.Maybe
    ( fromMaybe
    , listToMaybe
    , maybeToList
    , isNothing
    , catMaybes
    , mapMaybe
    )
import Control.Monad ( unless )
import Control.Applicative( (<$>) )
import Data.Char ( isDigit )

import Darcs.UI.External
    ( catchall )
import Darcs.Utils
    ( askUser
    , askUserListItem
    , maybeGetEnv
    , firstJustIO
    , withCurrentDirectory
    )
import Darcs.Repository.Prefs
    ( getPreflist
    , getGlobal
    , globalPrefsDirDoc
    )
import Darcs.URL ( isFile )
import Darcs.Path
    ( AbsolutePath
    , AbsolutePathOrStd
    , SubPath
    , toFilePath
    , makeSubPathOf
    , ioAbsolute
    , ioAbsoluteOrStd
    , makeAbsolute
    , makeAbsoluteOrStd
    )
import Darcs.UI.Flags ( DarcsFlag(..), maxCount, defaultFlag )
import Darcs.Global ( darcsdir )
import Darcs.Repository.Lock ( writeLocaleFile )
import Printer ( putDocLn, text, ($$) )
import ByteStringUtils ( decodeString )
#include "impossible.h"

data FlagContent = NoContent
                 | AbsoluteContent AbsolutePath
                 | AbsoluteOrStdContent AbsolutePathOrStd
                 | StringContent String
                   deriving (Eq, Show, Ord)

-- getContent is very tedious to write, but this is the only way (that
-- I know of) to guarantee that it works for all flags (which then
-- guarantees that isAnAbsolute, isa, flagToString, etc also work
-- properly)

-- | 'getContentString' returns the content of a flag, if any.
-- For instance, the content of @Author \"Louis Aragon\"@ is @StringContent
-- \"Louis Aragon\"@, while the content of @Pipe@ is @NoContent@
getContent :: DarcsFlag -> FlagContent
getContent (PatchName s) = StringContent s
getContent (Output s) = AbsoluteOrStdContent s
getContent Verbose = NoContent
getContent Help = NoContent
getContent ListOptions = NoContent
getContent Test = NoContent
getContent NoTest = NoContent
getContent OnlyChangesToFiles = NoContent
getContent ChangesToAllFiles = NoContent
getContent LeaveTestDir = NoContent
getContent NoLeaveTestDir = NoContent
getContent Timings = NoContent
getContent Debug = NoContent
getContent DebugVerbose = NoContent
getContent DebugHTTP = NoContent
getContent NormalVerbosity = NoContent
getContent Quiet = NoContent
getContent (Target s) = StringContent s
getContent (Cc s) = StringContent s
getContent (Subject s) = StringContent s
getContent (Charset s) = StringContent s
getContent (InReplyTo s) = StringContent s
getContent (SendmailCmd s) = StringContent s
getContent (Author s) = StringContent s
getContent (OnePatch s) = StringContent s
getContent (SeveralPatch s) = StringContent s
getContent (AfterPatch s) = StringContent s
getContent (UpToPatch s) = StringContent s
getContent (TagName s) = StringContent s
getContent (LastN s) = StringContent (show s)
getContent (MaxCount s) = StringContent (show s)
getContent (OneTag s) = StringContent s
getContent (AfterTag s) = StringContent s
getContent (UpToTag s) = StringContent s
getContent (Context s) = AbsoluteContent s
getContent GenContext = NoContent
getContent (LogFile s) = AbsoluteContent s
getContent (OutputAutoName s) = AbsoluteContent s
getContent NumberPatches = NoContent
getContent (PatchIndexRange _ _) = NoContent -- FIXME this doesn't fit into a neat category
getContent Count = NoContent
getContent All = NoContent
getContent Recursive = NoContent
getContent NoRecursive = NoContent
getContent Reorder = NoContent
getContent RestrictPaths = NoContent
getContent DontRestrictPaths = NoContent
getContent AskDeps = NoContent
getContent NoAskDeps = NoContent
getContent RmLogFile = NoContent
getContent DontRmLogFile = NoContent
getContent (DistName s) = StringContent s
getContent (CreatorHash s) = StringContent s
getContent (SignAs s) = StringContent s
getContent (SignSSL s) = StringContent s
getContent (Verify s) = AbsoluteContent s
getContent (VerifySSL s) = AbsoluteContent s
getContent IgnoreTimes = NoContent
getContent DontIgnoreTimes = NoContent
getContent LookForAdds = NoContent
getContent NoLookForAdds = NoContent
getContent AnyOrder = NoContent
getContent Intersection = NoContent
getContent Unified = NoContent
getContent NonUnified = NoContent
getContent Union = NoContent
getContent Complement = NoContent
getContent Sign = NoContent
getContent NoSign = NoContent
getContent HappyForwarding = NoContent
getContent NoHappyForwarding = NoContent
getContent (RemoteDarcsOpt s) = StringContent s
getContent (Toks s) = StringContent s
getContent (WorkRepoDir s) = StringContent s
getContent (WorkRepoUrl s) = StringContent s
getContent (RemoteRepo s) = StringContent s
getContent (NewRepo s) = StringContent s
getContent (Reply s) = StringContent s
getContent EditDescription = NoContent
getContent NoEditDescription = NoContent
getContent EditLongComment = NoContent
getContent NoEditLongComment = NoContent
getContent PromptLongComment = NoContent
getContent KeepDate = NoContent
getContent NoKeepDate = NoContent
getContent AllowConflicts = NoContent
getContent MarkConflicts = NoContent
getContent NoAllowConflicts = NoContent
getContent SkipConflicts = NoContent
getContent Boring = NoContent
getContent SkipBoring = NoContent
getContent AllowCaseOnly = NoContent
getContent DontAllowCaseOnly = NoContent
getContent AllowWindowsReserved = NoContent
getContent DontAllowWindowsReserved = NoContent
getContent DontGrabDeps = NoContent
getContent DontPromptForDependencies = NoContent
getContent PromptForDependencies = NoContent
getContent Compress = NoContent
getContent NoCompress = NoContent
getContent UnCompress = NoContent
getContent MachineReadable = NoContent
getContent HumanReadable = NoContent
getContent Pipe = NoContent
getContent Interactive = NoContent
getContent Summary = NoContent
getContent NoSummary = NoContent
getContent (ApplyAs s) = StringContent s
getContent (DiffCmd s) = StringContent s
getContent (ExternalMerge s) = StringContent s
getContent PauseForGui = NoContent
getContent NoPauseForGui = NoContent
getContent (DiffFlags s) = StringContent s
getContent (OnePattern _) = NoContent -- FIXME!!!
getContent (SeveralPattern _) = NoContent -- FIXME!!!
getContent (UpToPattern _) = NoContent -- FIXME!!!
getContent (AfterPattern _) = NoContent -- FIXME!!!
getContent Reverse = NoContent
getContent Forward = NoContent
getContent Complete = NoContent
getContent Lazy = NoContent
getContent (FixFilePath _ _) = NoContent -- FIXME!!!
getContent XMLOutput = NoContent
getContent ForceReplace = NoContent
getContent NonApply = NoContent
getContent NonVerify = NoContent
getContent NonForce = NoContent
getContent DryRun = NoContent
getContent (SetDefault _) = NoContent
getContent (NoSetDefault _) = NoContent
getContent Disable = NoContent
getContent SetScriptsExecutable = NoContent
getContent DontSetScriptsExecutable = NoContent
getContent Once = NoContent
getContent Linear = NoContent
getContent Backoff = NoContent
getContent Bisect = NoContent
getContent UseHashedInventory = NoContent
getContent UseFormat2 = NoContent
getContent NoUpdateWorking = NoContent
getContent UpgradeFormat = NoContent
getContent Relink = NoContent
getContent Files = NoContent
getContent NoFiles = NoContent
getContent Directories = NoContent
getContent NoDirectories = NoContent
getContent Pending = NoContent
getContent NoPending = NoContent
getContent NoPosthook = NoContent
getContent AskPosthook = NoContent
getContent (Sibling s) = AbsoluteContent s
getContent (PosthookCmd s) = StringContent s
getContent RunPosthook = NoContent
getContent NoPrehook = NoContent
getContent RunPrehook = NoContent
getContent AskPrehook = NoContent
getContent StoreInMemory = NoContent
getContent ApplyOnDisk = NoContent
getContent NoHTTPPipelining = NoContent
getContent Packs = NoContent
getContent NoPacks = NoContent
getContent NoCache = NoContent
getContent NullFlag = NoContent
getContent (PrehookCmd s) = StringContent s
getContent (UMask s) = StringContent s
getContent AllowUnrelatedRepos = NoContent
getContent Check = NoContent
getContent Repair = NoContent
getContent JustThisRepo = NoContent
getContent OptimizePristine = NoContent
getContent OptimizeHTTP = NoContent
getContent AmendUnrecord = NoContent
getContent NoAmendUnrecord = NoContent
getContent UseWorkingDir = NoContent
getContent UseNoWorkingDir = NoContent
getContent PatchIndexFlag = NoContent
getContent NoPatchIndexFlag = NoContent

getContentString :: DarcsFlag -> Maybe String
getContentString f =
   do StringContent s <- Just $ getContent f
      return s

-- | @a `'isa'` b@ tests whether @a@ is flag @b@ with a string argument.
-- @b@ typically is a Flag constructor expecting a string
-- For example, @(Author \"Ted Hughes\") `isa` Author@ returns true.
isa :: DarcsFlag -> (String -> DarcsFlag) -> Bool
a `isa` b = case getContentString a of
            Nothing -> False
            Just s -> a == b s

-- | @a `'isAnAbsolute'` b@ tests whether @a@ is flag @b@ with an absolute path argument.
-- @b@ typically is a Flag constructor expecting an absolute path argument
-- For example, @(Context contextfile) `isAnAbsolute` Context@ returns true.
isAnAbsolute :: DarcsFlag -> (AbsolutePath -> DarcsFlag) -> Bool
isAnAbsolute f x = case getContent f of
                  AbsoluteContent s -> f == x s
                  _ -> False

-- | @a `'isAnAbsoluteOrStd'` b@ tests whether @a@ is flag @b@ with a path argument.
-- @b@ typically is a Flag constructor expecting a path argument
-- For example, @(Output o) `isAnAbsoluteOrStd` @ returns true.
isAnAbsoluteOrStd :: DarcsFlag -> (AbsolutePathOrStd -> DarcsFlag) -> Bool
isAnAbsoluteOrStd f x = case getContent f of
                          AbsoluteOrStdContent s -> f == x s
                          _ -> False

isin :: DarcsAtomicOption -> [DarcsFlag] -> Bool
(DarcsInternalOption f)             `isin` fs = f `elem` fs
(DarcsNoArgOption _ _ f _)          `isin` fs = f `elem` fs
(DarcsArgOption _ _ f _ _)          `isin` fs = any (`isa` f) fs
(DarcsAbsPathOption _ _ f _ _)      `isin` fs = any (`isAnAbsolute` f) fs
(DarcsAbsPathOrStdOption _ _ f _ _) `isin` fs = any (`isAnAbsoluteOrStd` f) fs
(DarcsOptAbsPathOption _ _ _ f _ _) `isin` fs = any (`isAnAbsolute` f) fs

arein :: DarcsOption -> [DarcsFlag] -> Bool
o `arein` fs = any (`isin` fs) (atomicOptions o)

-- | A type for darcs' options. The value contains the command line
-- switch(es) for the option, a help string, and a function to build a
-- @DarcsFlag@ from the command line arguments.  for each constructor,
-- 'shortSwitches' represents the list of short command line switches
-- which invoke the option, longSwitches the list of long command line
-- switches, optDescr the description of the option, and argDescr the description
-- of its argument, if any. mkFlag is a function which makes a @DarcsFlag@ from
-- the arguments of the option.
data DarcsAtomicOption
    = DarcsArgOption [Char] [String] (String->DarcsFlag) String String
    -- ^ @DarcsArgOption shortSwitches longSwitches mkFlag ArgDescr OptDescr@
    -- The constructor for options with a string argument, such as
    -- @--tag@

    | DarcsAbsPathOption [Char] [String] (AbsolutePath -> DarcsFlag) String String
    -- ^ @DarcsAbsPathOption shortSwitches longSwitches mkFlag ArgDescr OptDescr@
    -- The constructor for options with an absolute path argument, such as
    -- @--sibling@

    | DarcsAbsPathOrStdOption [Char] [String] (AbsolutePathOrStd -> DarcsFlag) String String
    -- ^ @DarcsAbsPathOrStdOption shortSwitches longSwitches mkFlag ArgDescr OptDescr@
    -- The constructor for options with a path argument, such as @-o@

    | DarcsOptAbsPathOption [Char] [String] String (AbsolutePath -> DarcsFlag) String String
    -- ^ @DarcsOptAbsPathOrStdOption shortSwitches longSwitches defaultPath
    -- mkFlag ArgDescr OptDescr@ where defaultPath is a default value
    -- for the Path, as a string to be parsed as if it had been given
    -- on the command line.
    -- The constructor for options with an optional path argument, such as @-O@

    | DarcsNoArgOption [Char] [String] DarcsFlag String
    -- ^ @DarcsNoArgOption shortSwitches longSwitches mkFlag optDescr@
    -- The constructon fon options with no arguments.

    | DarcsInternalOption DarcsFlag
    -- ^ @DarcsInternalOption@
    -- An option just for internal use (e.g. defaulting), not directly available to the user.


data DarcsOption
    = DarcsSingleOption DarcsAtomicOption
    | DarcsMultipleChoiceOption [DarcsAtomicOption]
    -- ^ A constructor for grouping related options together, such as
    -- @--hashed@ and @--darcs-2@.

    | DarcsMutuallyExclusive [DarcsAtomicOption]          -- choices
                             ([DarcsFlag] -> [DarcsFlag]) -- setter

type NoArgPieces = (DarcsFlag -> String -> DarcsAtomicOption, DarcsFlag , String)

mkMutuallyExclusive :: [NoArgPieces] -- ^ before
                    -> NoArgPieces   -- ^ default
                    -> [NoArgPieces] -- ^ after
                    -> DarcsOption
mkMutuallyExclusive os1 od_ os2 =
  DarcsMutuallyExclusive (map option (os1 ++ (od : os2)))
                         (defaultFlag (map flag (os1 ++ os2)) (flag od))
 where
  od = third (++ " [DEFAULT]") od_
  flag (_,f,_) = f
  option (x,y,z) = x y z
  third f (x,y,z) = (x,y,f z)

nubOptions :: [DarcsOption] -> [DarcsFlag] -> [DarcsFlag]
nubOptions [] opts = opts
nubOptions (DarcsMutuallyExclusive ch _:options) opts = nubOptions options $ collapse opts
  where collapse (x:xs) | x `elem` flags ch = x : clear xs
                        | otherwise = x : collapse xs
        collapse [] = []
        clear (x:xs) | x `elem` flags ch = clear xs
                     | otherwise = x : clear xs
        clear [] = []
        flags (DarcsNoArgOption _ _ fl _:xs) = fl : flags xs
        flags (DarcsInternalOption fl:xs) = fl : flags xs
        flags (_:xs) = flags xs
        flags [] = []
nubOptions (_:options) opts = nubOptions options opts

applyDefaults :: [DarcsOption] -> [DarcsFlag] -> [DarcsFlag]
applyDefaults opts = foldr (.) id (mapMaybe getSetter opts)
 where
  getSetter (DarcsMutuallyExclusive _ f) = Just f
  getSetter _ = Nothing

optionFromDarcsAtomicOption :: AbsolutePath -> DarcsAtomicOption -> Maybe (OptDescr DarcsFlag)
optionFromDarcsAtomicOption _ (DarcsInternalOption _) = Nothing
optionFromDarcsAtomicOption _ (DarcsNoArgOption a b c h) = Just $ Option a b (NoArg c) h
optionFromDarcsAtomicOption _ (DarcsArgOption a b c n h) = Just $ Option a b (ReqArg c n) h
optionFromDarcsAtomicOption wd (DarcsAbsPathOrStdOption a b c n h) =
  Just $ Option a b (ReqArg (c . makeAbsoluteOrStd wd) n) h
optionFromDarcsAtomicOption wd (DarcsAbsPathOption a b c n h) =
  Just $ Option a b (ReqArg (c . makeAbsolute wd) n) h
optionFromDarcsAtomicOption wd (DarcsOptAbsPathOption a b d c n h) =
  Just $ Option a b (OptArg (c . makeAbsolute wd . fromMaybe d) n) h

atomicOptions :: DarcsOption -> [DarcsAtomicOption]
atomicOptions (DarcsSingleOption x) = [x]
atomicOptions (DarcsMultipleChoiceOption xs) = xs
atomicOptions (DarcsMutuallyExclusive xs _) = xs

optionFromDarcsOption :: AbsolutePath -> DarcsOption -> [OptDescr DarcsFlag]
optionFromDarcsOption wd = mapMaybe (optionFromDarcsAtomicOption wd) . atomicOptions

-- | 'concatOptions' creates a DarcsMultipleChoiceOption from a list of
-- option, flattening any DarcsMultipleChoiceOption in the list.
concatOptions :: [DarcsOption] -> DarcsOption
concatOptions = DarcsMultipleChoiceOption . concatMap atomicOptions

extractFixPath :: [DarcsFlag] -> Maybe (AbsolutePath, AbsolutePath)
extractFixPath fs = listToMaybe [ (repo,orig) | FixFilePath repo orig <- fs]

fixFilePath :: [DarcsFlag] -> FilePath -> IO AbsolutePath
fixFilePath opts f = case extractFixPath opts of
                       Nothing -> bug "Can't fix path in fixFilePath"
                       Just (_,o) -> withCurrentDirectory o $ ioAbsolute f

fixFilePathOrStd :: [DarcsFlag] -> FilePath -> IO AbsolutePathOrStd
fixFilePathOrStd opts f =
    case extractFixPath opts of
      Nothing -> bug "Can't fix path in fixFilePathOrStd"
      Just (_,o) -> withCurrentDirectory o $ ioAbsoluteOrStd f

fixUrlFlag :: [DarcsFlag] -> DarcsFlag -> IO DarcsFlag
fixUrlFlag opts (RemoteRepo f) = RemoteRepo `fmap` fixUrl opts f
fixUrlFlag _ f = return f

-- | 'fixUrl' takes a String that may be a file path or a URL.
-- It returns either the URL, or an absolute version of the path.
fixUrl :: [DarcsFlag] -> String -> IO String
fixUrl opts f = if isFile f
                then toFilePath `fmap` fixFilePath opts f
                else return f

-- | @maybeFixSubPaths files@ tries to turn the file paths in its argument into
-- @SubPath@s.
--
-- When converting a relative path to an absolute one, this function first tries
-- to interpret the relative path with respect to the current working directory.
-- If that fails, it tries to interpret it with respect to the repository
-- directory. Only when that fails does it put a @Nothing@ in the result at the
-- position of the path that cannot be converted.
--
-- It is intended for validating file arguments to darcs commands.
maybeFixSubPaths :: [DarcsFlag] -> [FilePath] -> IO [Maybe SubPath]
maybeFixSubPaths flags fs = withCurrentDirectory o $ do
  fixedFs <- mapM fixit fs
  let bads = snd . unzip . filter (isNothing . fst) $ zip fixedFs fs
  unless (null bads) . putStrLn $ "Ignoring non-repository paths: " ++
    intercalate ", " bads
  return fixedFs
 where
    (r,o) = case extractFixPath flags of
            Just xxx -> xxx
            Nothing -> bug "Can't fix path in fixSubPaths"
    fixit p = do ap <- ioAbsolute p
                 case makeSubPathOf r ap of
                   Just sp -> return $ Just sp
                   Nothing -> withCurrentDirectory r $ do
                     absolutePathByRepodir <- ioAbsolute p
                     return $ makeSubPathOf r absolutePathByRepodir

-- | @fixSubPaths files@ returns the @SubPath@s for the paths in @files@ that
-- are inside the repository, preserving their order. Paths in @files@ that are
-- outside the repository directory are not in the result.
--
-- When converting a relative path to an absolute one, this function first tries
-- to interpret the relative path with respect to the current working directory.
-- If that fails, it tries to interpret it with respect to the repository
-- directory. Only when that fails does it omit the path from the result.
--
-- It is intended for validating file arguments to darcs commands.
fixSubPaths :: [DarcsFlag] -> [FilePath] -> IO [SubPath]
fixSubPaths flags fs = nub . catMaybes <$> (maybeFixSubPaths flags $
  filter (not . null) fs)

patchIndex :: DarcsOption
patchIndex = DarcsSingleOption $
  DarcsNoArgOption [] ["patch-index"] PatchIndexFlag
  "create, maintain, and use patch index"

noPatchIndex :: DarcsOption
noPatchIndex = DarcsSingleOption $
  DarcsNoArgOption [] ["no-patch-index"] NoPatchIndexFlag
  "Do not create, maintain, and use patch index"

-- | 'listOptions' is an option which lists the command's arguments
listOptions :: DarcsOption
listOptions = DarcsSingleOption $ DarcsNoArgOption [] ["list-options"] ListOptions
               "simply list the command's arguments"

flagToString :: [DarcsOption] -> DarcsFlag -> Maybe String
flagToString x f = listToMaybe $ mapMaybe f2o $ concatMap atomicOptions x
    where f2o (DarcsArgOption _ (s:_) c _ _) =
            do arg <- getContentString f
               if c arg == f
                  then return $ unwords [('-':'-':s), arg]
                  else Nothing
          f2o (DarcsNoArgOption _ (s:_) f' _) | f == f' = Just ('-':'-':s)
          f2o _ = Nothing

pipeInteractive, allPipeInteractive, allInteractive,
  humanReadable, machineReadable,
  diffflags, allowProblematicFilenames, noskipBoring,
  askLongComment, matchOneNontag, changesReverse, creatorhash,
  changesFormat, matchOneContext, happyForwarding, sendToContext,
  diffCmdFlag, storeInMemory, useExternalMerge,
  pauseForGui,
  pullConflictOptions, target, ccSend, ccApply, applyConflictOptions, reply, xmloutput,
  distnameOption, patchnameOption, editDescription,
  output, outputAutoName, unidiff, repoCombinator,
  unified, summary, uncompressNocompress, subject, charset, inReplyTo,
  nocompress, matchSeveralOrRange, matchSeveralOrLast,
  author, askdeps, lookforadds, ignoretimes, test, notest, help, forceReplace,
  allowUnrelatedRepos,
  matchOne, matchRange, matchSeveral, sendmailCmd,
  logfile, rmlogfile, leaveTestDir, fromOpt,  amendUnrecord
      :: DarcsOption

sign, applyas, verify :: DarcsOption

help = DarcsSingleOption $ DarcsNoArgOption ['h'] ["help"] Help
       "shows brief description of command and its arguments"

disable :: DarcsOption
disable = DarcsSingleOption $ DarcsNoArgOption [] ["disable"] Disable
        "disable this command"

anyVerbosity :: [DarcsOption]
anyVerbosity =[DarcsMultipleChoiceOption
                [DarcsNoArgOption [] ["debug"] Debug
                 "give only debug output",
                 DarcsNoArgOption [] ["debug-verbose"] DebugVerbose
                 "give debug and verbose output",
                 DarcsNoArgOption [] ["debug-http"] DebugHTTP
                 "give debug output for libcurl",
                 DarcsNoArgOption ['v'] ["verbose"] Verbose
                 "give verbose output",
                 DarcsNoArgOption ['q'] ["quiet"] Quiet
                 "suppress informational output",
                 DarcsNoArgOption [] ["standard-verbosity"] NormalVerbosity
                 "neither verbose nor quiet output"],
               DarcsSingleOption
                (DarcsNoArgOption [] ["timings"] Timings "provide debugging timings information")]

workingRepoDir :: DarcsOption
workingRepoDir = DarcsSingleOption $ DarcsArgOption [] ["repodir"] WorkRepoDir "DIRECTORY"
             "specify the repository directory in which to run"

possiblyRemoteRepoDir :: DarcsOption
possiblyRemoteRepoDir = DarcsSingleOption $ DarcsArgOption [] ["repo"] WorkRepoUrl "URL"
             "specify the repository URL"

-- | 'getRepourl' takes a list of flags and returns the url of the
-- repository specified by @Repodir \"directory\"@ in that list of flags, if any.
-- This flag is present if darcs was invoked with @--repodir=DIRECTORY@
getRepourl :: [DarcsFlag] -> Maybe String
getRepourl fs = listToMaybe [ d | WorkRepoUrl d <- fs, not (isFile d) ]

-- | 'remoteRepo' is the option used to specify the URL of the remote
-- repository to work with
remoteRepo :: DarcsOption
remoteRepo = DarcsSingleOption $ DarcsArgOption [] ["remote-repo"] RemoteRepo "URL"
             "specify the remote repository URL to work with"

patchnameOption = DarcsSingleOption $ DarcsArgOption ['m'] ["name"]
                   (PatchName . decodeString) "PATCHNAME" "name of patch"

sendToContext = DarcsSingleOption $ DarcsAbsPathOption [] ["context"] Context "FILENAME"
                  "send to context stored in FILENAME"

matchOneContext =
    DarcsMultipleChoiceOption
    [DarcsArgOption [] ["to-match"] mp "PATTERN"
     "select changes up to a patch matching PATTERN",
     DarcsArgOption [] ["to-patch"] OnePatch "REGEXP"
     "select changes up to a patch matching REGEXP",
     __tag,
     DarcsAbsPathOption [] ["context"] Context "FILENAME"
     "version specified by the context in FILENAME"
    ]
    where mp s = OnePattern s

matchOne = DarcsMultipleChoiceOption [__match, __patch, __tag, __index]
             -- [NOTE --index removed from matchOneNontag because issue1926]
             -- The --index option was removed for 2.5 release because it isn't handled
             -- by amend-record (see issue1926).
             --
             -- At this moment, amend-record is the only command that uses 'matchOneNontag',
             -- so there is no other command affected.
matchOneNontag  = DarcsMultipleChoiceOption [__match, __patch {- , __index -} ]
matchSeveral    = DarcsMultipleChoiceOption [__matches, __patches, __tags]
matchRange      = concatOptions [ matchTo, matchFrom
                                , DarcsMultipleChoiceOption [__match, __patch, __last, __indexes] ]
matchSeveralOrRange = concatOptions [ matchTo, matchFrom
                                    , DarcsMultipleChoiceOption [ __last, __indexes, __matches, __patches, __tags] ]
matchSeveralOrLast  = concatOptions [ matchFrom
                                    , DarcsMultipleChoiceOption [ __last, __matches, __patches, __tags] ]

matchTo, matchFrom :: DarcsOption
matchTo = DarcsMultipleChoiceOption
            [DarcsArgOption [] ["to-match"] uptop "PATTERN"
             "select changes up to a patch matching PATTERN",
             DarcsArgOption [] ["to-patch"] UpToPatch "REGEXP"
             "select changes up to a patch matching REGEXP",
             DarcsArgOption [] ["to-tag"] UpToTag "REGEXP"
             "select changes up to a tag matching REGEXP"]
    where uptop s = UpToPattern s
matchFrom = DarcsMultipleChoiceOption
              [DarcsArgOption [] ["from-match"] fromp "PATTERN"
               "select changes starting with a patch matching PATTERN",
               DarcsArgOption [] ["from-patch"] AfterPatch "REGEXP"
               "select changes starting with a patch matching REGEXP",
               DarcsArgOption [] ["from-tag"] AfterTag "REGEXP"
               "select changes starting with a tag matching REGEXP"]
    where fromp s = AfterPattern s

__tag, __tags, __patch, __patches, __match, __matches, __last, __index, __indexes :: DarcsAtomicOption

__tag = DarcsArgOption ['t'] ["tag"] OneTag "REGEXP"
       "select tag matching REGEXP"
__tags = DarcsArgOption ['t'] ["tags"] OneTag "REGEXP"
        "select tags matching REGEXP"

__patch = DarcsArgOption ['p'] ["patch"] OnePatch "REGEXP"
         "select a single patch matching REGEXP"
__patches = DarcsArgOption ['p'] ["patches"] SeveralPatch "REGEXP"
           "select patches matching REGEXP"

__match = DarcsArgOption [] ["match"] mp "PATTERN"
         "select a single patch matching PATTERN"
  where mp s = OnePattern s
__matches = DarcsArgOption [] ["matches"] mp "PATTERN"
           "select patches matching PATTERN"
  where mp s = SeveralPattern s

__last = DarcsArgOption [] ["last"] lastn "NUMBER"
         "select the last NUMBER patches"
    where lastn = LastN . numberString

__index = DarcsArgOption ['n'] ["index"] indexrange "N" "select one patch"
    where indexrange s = if all isDigit s
                         then PatchIndexRange (read s) (read s)
                         else PatchIndexRange 0 0

__indexes = DarcsArgOption ['n'] ["index"] indexrange "N-M" "select a range of patches"
    where indexrange s = if all isokay s
                         then if '-' `elem` s
                              then let x1 = takeWhile (/= '-') s
                                       x2 = reverse $ takeWhile (/= '-') $ reverse s
                                   in PatchIndexRange (read x1) (read x2)
                              else PatchIndexRange (read s) (read s)
                         else PatchIndexRange 0 0
          isokay c = isDigit c || c == '-'

matchMaxcount :: DarcsOption
matchMaxcount = DarcsSingleOption $ DarcsArgOption [] ["max-count"] mc "NUMBER"
         "return only NUMBER results"
    where mc = MaxCount . numberString


-- | 'getContext' takes a list of flags and returns the context
-- specified by @Context c@ in that list of flags, if any.
-- This flag is present if darcs was invoked with @--context=FILE@
getContext :: [DarcsFlag] -> Maybe AbsolutePath
getContext xs = listToMaybe [ c | Context c <- xs ]

notest = DarcsMultipleChoiceOption
         [DarcsNoArgOption [] ["no-test"] NoTest "don't run the test script",
          DarcsNoArgOption [] ["test"] Test "run the test script"]
test = DarcsMultipleChoiceOption
          [DarcsNoArgOption [] ["test"] Test "run the test script",
           DarcsNoArgOption [] ["no-test"] NoTest "don't run the test script"]
leaveTestDir = DarcsMultipleChoiceOption
                 [DarcsNoArgOption [] ["leave-test-directory"]
                  LeaveTestDir "don't remove the test directory",
                  DarcsNoArgOption [] ["remove-test-directory"]
                  NoLeaveTestDir "remove the test directory"]

amendUnrecord = DarcsMultipleChoiceOption
                [DarcsNoArgOption [] ["add"]
                 NoAmendUnrecord "add the changes to patch (default)",
                 DarcsNoArgOption [] ["unrecord"]
                 AmendUnrecord "subtract the changes from patch"]

ignoretimes =
    DarcsMultipleChoiceOption
    [DarcsNoArgOption [] ["ignore-times"] IgnoreTimes
                         "don't trust the file modification times"
    ,DarcsNoArgOption [] ["no-ignore-times"] DontIgnoreTimes
                      "trust modification times to find modified files [DEFAULT]"
    ]

lookforadds =
    DarcsMultipleChoiceOption
    [DarcsNoArgOption ['l'] ["look-for-adds"] LookForAdds
     "look for (non-boring) files that could be added",
     DarcsNoArgOption [] ["dont-look-for-adds","no-look-for-adds"] NoLookForAdds
     "don't look for any files that could be added [DEFAULT]"]

askdeps =
    DarcsMultipleChoiceOption
    [DarcsNoArgOption [] ["ask-deps"] AskDeps
     "ask for extra dependencies",
     DarcsNoArgOption [] ["no-ask-deps"] NoAskDeps
     "don't ask for extra dependencies"]

askLongComment =
    DarcsMultipleChoiceOption
    [DarcsNoArgOption [] ["edit-long-comment"] EditLongComment
     "edit the long comment by default",
     DarcsNoArgOption [] ["skip-long-comment"] NoEditLongComment
     "don't give a long comment",
     DarcsNoArgOption [] ["prompt-long-comment"] PromptLongComment
     "prompt for whether to edit the long comment"]

keepDate :: DarcsOption
keepDate =
    DarcsMultipleChoiceOption
    [DarcsNoArgOption [] ["keep-date"] KeepDate
     "keep the date of the original patch",
     DarcsNoArgOption [] ["no-keep-date"] NoKeepDate
     "use the current date for the amended patch"
    ]

logfile = DarcsSingleOption $ DarcsAbsPathOption [] ["logfile"] LogFile "FILE"
          "give patch name and comment in file"

rmlogfile = DarcsMultipleChoiceOption
            [DarcsNoArgOption [] ["delete-logfile"] RmLogFile
            "delete the logfile when done",
             DarcsNoArgOption [] ["no-delete-logfile"] DontRmLogFile
            "keep the logfile when done [DEFAULT]"]

author = DarcsSingleOption $
  DarcsArgOption ['A'] ["author"] (Author . decodeString) "EMAIL" "specify author id"
fromOpt = DarcsSingleOption $
  DarcsArgOption [] ["from"] (Author . decodeString) "EMAIL" "specify email address"

fileHelpAuthor :: [String]
fileHelpAuthor = [
 "Each patch is attributed to its author, usually by email address (for",
 "example, `Fred Bloggs <fred@example.net>').  Darcs looks in several",
 "places for this author string: the --author option, the files",
 "_darcs/prefs/author (in the repository) and " ++ globalPrefsDirDoc ++ "author (in your",
 "home directory), and the environment variables $DARCS_EMAIL and",
 "$EMAIL.  If none of those exist, Darcs will prompt you for an author",
 "string and write it to _darcs/prefs/author.  Note that if if you have more",
 "than one email address, note that you can put them all in " ++ globalPrefsDirDoc ++ "author,",
 "one author per line.  Darcs will still prompt you for an author, but it",
 "allows you to select from the list, or to type in an alternative."
 ]

environmentHelpEmail :: ([String], [String])
environmentHelpEmail = (["DARCS_EMAIL","EMAIL"], fileHelpAuthor)

-- | 'getAuthor' takes a list of flags and returns the author of the
-- change specified by @Author \"Leo Tolstoy\"@ in that list of flags, if any.
-- Otherwise, if @Pipe@ is present, asks the user who is the author and
-- returns the answer. If neither are present, try to guess the author,
-- from @_darcs/prefs@, and if it's not possible, ask the user.
getAuthor :: [DarcsFlag] -> IO String
getAuthor (Author a:_) = return a
getAuthor (Pipe:_) = askUser "Who is the author? "
getAuthor (_:flags) = getAuthor flags
getAuthor [] = do
  easy_author <- getEasyAuthor
  case easy_author of
    [a] -> return a
    []  -> askForAuthor shortPrompt longPrompt
    as  -> askForAuthor (fancyPrompt as) (fancyPrompt as)
 where
  shortPrompt = askUser "What is your email address? "
  longPrompt  = askUser "What is your email address (e.g. Fred Bloggs <fred@example.net>)? "
  fancyPrompt xs =
    do putDocLn $ text "" $$
                  text "You have saved the following email addresses to your global settings:"
       str <- askUserListItem "Please select an email address for this repository: " (xs ++ ["Other"])
       if str == "Other"
          then longPrompt
          else return str
  askForAuthor askfn1 askfn2 = do
      aminrepo <- doesDirectoryExist (darcsdir++"/prefs")
      if aminrepo then do
          putDocLn $
            text "Each patch is attributed to its author, usually by email address (for" $$
            text "example, `Fred Bloggs <fred@example.net>').  Darcs could not determine" $$
            text "your email address, so you will be prompted for it." $$
            text "" $$
            text ("Your address will be stored in " ++ darcsdir ++ "/prefs/author.") $$
            text "It will be used for all patches recorded in this repository." $$
            text ("If you move that file to " ++ globalPrefsDirDoc ++ "author, it will be used for patches") $$
            text "you record in ALL repositories."
          add <- askfn1
          writeLocaleFile (darcsdir ++ "/prefs/author") $
                          unlines ["# " ++ line | line <- fileHelpAuthor] ++ "\n" ++ add
          return add
        else askfn2

-- | 'getEasyAuthor' tries to get the author name first from the repository preferences,
-- then from global preferences, then from environment variables.  Returns @[]@
-- if it could not get it.  Note that it may only return multiple possibilities when
-- reading from global preferences
getEasyAuthor :: IO [String]
getEasyAuthor = fmap (map decodeString) $
  firstNotNullIO [ (take 1 . nonblank) `fmap` getPreflist "author"
                 , nonblank    `fmap` getGlobal "author"
                 , maybeToList `fmap` maybeGetEnv "DARCS_EMAIL"
                 , maybeToList `fmap` maybeGetEnv "EMAIL"
                 ]
 where
  nonblank = filter (not . null)
  -- this could perhaps be simplified with Control.Monad
  -- but note that we do NOT want to concatenate the results
  firstNotNullIO [] = return []
  firstNotNullIO (e:es) = do
    v <- e `catchall` return []
    if null v then firstNotNullIO es else return v

nocompress = DarcsMultipleChoiceOption [__compress, __dontCompress]
uncompressNocompress = DarcsMultipleChoiceOption [__compress, __dontCompress, __uncompress]

__compress, __dontCompress, __uncompress :: DarcsAtomicOption
__compress = DarcsNoArgOption [] ["compress"] Compress
            "create compressed patches"
__dontCompress = DarcsNoArgOption [] ["dont-compress","no-compress"] NoCompress
                  "don't create compressed patches"
__uncompress = DarcsNoArgOption [] ["uncompress"] UnCompress
               "uncompress patches"

summary = DarcsMultipleChoiceOption
          [DarcsNoArgOption ['s'] ["summary"] Summary "summarize changes",
           DarcsNoArgOption [] ["no-summary"] NoSummary "don't summarize changes"]
unified = DarcsMultipleChoiceOption
          [DarcsNoArgOption ['u'] ["unified"] Unified
          "output changes in a darcs-specific format similar to diff -u",
           DarcsNoArgOption  [] ["no-unified"] NonUnified
          "output changes in darcs' usual format"]

unidiff = DarcsMultipleChoiceOption
          [DarcsNoArgOption ['u'] ["unified"] Unified
          "pass -u option to diff [DEFAULT]",
           DarcsNoArgOption  [] ["no-unified"] NonUnified
          "output patch in diff's dumb format"]

diffCmdFlag = DarcsSingleOption $
  DarcsArgOption [] ["diff-command"] DiffCmd "COMMAND" "specify diff command (ignores --diff-opts)"

pauseForGui = DarcsMultipleChoiceOption
    [ DarcsNoArgOption
        []
        ["pause-for-gui"]
        PauseForGui
        "pause for an external diff or merge command to finish [DEFAULT]"
    , DarcsNoArgOption
        []
        ["no-pause-for-gui"]
        NoPauseForGui
        "return immediately after external diff or merge command finishes"
    ]

storeInMemory = DarcsMultipleChoiceOption
    [DarcsNoArgOption [] ["store-in-memory"] StoreInMemory
     "do patch application in memory rather than on disk",
     DarcsNoArgOption [] ["no-store-in-memory"] ApplyOnDisk
     "do patch application on disk [DEFAULT]"]

target = DarcsSingleOption $
  DarcsArgOption [] ["to"] Target "EMAIL" "specify destination email"
ccSend = DarcsSingleOption $
  DarcsArgOption [] ["cc"] Cc "EMAIL" "mail results to additional EMAIL(s)"
ccApply = DarcsSingleOption $
  DarcsArgOption [] ["cc"] Cc "EMAIL" "mail results to additional EMAIL(s). Requires --reply"

-- |'getCc' takes a list of flags and returns the addresses to send a copy of
-- the patch bundle to when using @darcs send@.
-- looks for a cc address specified by @Cc \"address\"@ in that list of flags.
-- Returns the addresses as a comma separated string.
getCc :: [DarcsFlag] -> String
getCc fs = lt $ mapMaybe whatcc fs
            where whatcc (Cc t) = Just t
                  whatcc _ = Nothing
                  lt [t] = t
                  lt [t,""] = t
                  lt (t:ts) = t++" , "++lt ts
                  lt [] = ""

subject = DarcsSingleOption $ DarcsArgOption [] ["subject"] Subject "SUBJECT" "specify mail subject"

-- |'getSubject' takes a list of flags and returns the subject of the mail
-- to be sent by @darcs send@. Looks for a subject specified by
-- @Subject \"subject\"@ in that list of flags, if any.
-- This flag is present if darcs was invoked with @--subject=SUBJECT@
getSubject :: [DarcsFlag] -> Maybe String
getSubject fs = listToMaybe [ s | Subject s <- fs ]

charset = DarcsSingleOption $ DarcsArgOption [] ["charset"] Charset "CHARSET" "specify mail charset"

getCharset :: [DarcsFlag] -> Maybe String
getCharset fs = listToMaybe [ s | Charset s <- fs ]

inReplyTo = DarcsSingleOption $ DarcsArgOption [] ["in-reply-to"] InReplyTo "EMAIL" "specify in-reply-to header"
getInReplyTo :: [DarcsFlag] -> Maybe String
getInReplyTo fs = listToMaybe [ s | InReplyTo s <- fs ]

output = DarcsSingleOption $ DarcsAbsPathOrStdOption ['o'] ["output"] Output "FILE"
         "specify output filename"

outputAutoName = DarcsSingleOption $
                   DarcsOptAbsPathOption ['O'] ["output-auto-name"] "." OutputAutoName "DIRECTORY"
                   "output to automatically named file in DIRECTORY, default: current directory"

getOutput :: [DarcsFlag] -> FilePath -> Maybe AbsolutePathOrStd
getOutput (Output a:_) _ = return a
getOutput (OutputAutoName a:_) f = return $ makeAbsoluteOrStd a f
getOutput (_:flags) f = getOutput flags f
getOutput [] _ = Nothing

editDescription = mkMutuallyExclusive [] yes [no]
 where
  yes = ( DarcsNoArgOption [] ["edit-description"]
        , EditDescription
        , "edit the patch bundle description" )
  no  = ( DarcsNoArgOption [] ["dont-edit-description","no-edit-description"]
        , NoEditDescription
        , "don't edit the patch bundle description" )

distnameOption = DarcsSingleOption $
  DarcsArgOption ['d'] ["dist-name"] DistName "DISTNAME" "name of version"

recursive :: String -> DarcsOption
recursive h
    = DarcsMultipleChoiceOption
      [DarcsNoArgOption ['r'] ["recursive"] Recursive h,
       DarcsNoArgOption [] ["not-recursive","no-recursive"] NoRecursive ("don't "++h)]

patchFormatChoices :: DarcsOption
patchFormatChoices =
    DarcsMultipleChoiceOption
    [DarcsNoArgOption [] ["hashed"] UseHashedInventory
                          "Some new features. Compatible with older repos",
     DarcsNoArgOption [] ["darcs-2"] UseFormat2
                          "All features. Related repos must use same format [DEFAULT]."]

useWorkingDir :: DarcsOption
useWorkingDir =
  DarcsMultipleChoiceOption
  [ DarcsNoArgOption [] ["with-working-dir"] UseWorkingDir
                         "Create a working directory (normal repository)",
    DarcsNoArgOption [] ["no-working-dir"] UseNoWorkingDir
                           "Do not create a working directory (bare repository)"]

upgradeFormat :: DarcsOption
upgradeFormat = DarcsSingleOption $
    DarcsNoArgOption [] ["upgrade"] UpgradeFormat
         "upgrade repository to latest compatible format"

xmloutput = DarcsSingleOption $
  DarcsNoArgOption [] ["xml-output"] XMLOutput "generate XML formatted output"

creatorhash = DarcsSingleOption $
  DarcsArgOption [] ["creator-hash"] CreatorHash "HASH" "specify hash of creator patch (see docs)"

sign = DarcsMultipleChoiceOption
       [DarcsNoArgOption [] ["sign"] Sign
        "sign the patch with your gpg key",
        DarcsArgOption [] ["sign-as"] SignAs "KEYID"
        "sign the patch with a given keyid",
        DarcsArgOption [] ["sign-ssl"] SignSSL "IDFILE"
        "sign the patch using openssl with a given private key",
        DarcsNoArgOption [] ["dont-sign","no-sign"] NoSign
        "don't sign the patch"]
applyas = DarcsMultipleChoiceOption
           [DarcsArgOption [] ["apply-as"] ApplyAs "USERNAME"
            "apply patch as another user using sudo",
            DarcsNoArgOption [] ["no-apply-as"] NonApply
            "don't use sudo to apply as another user [DEFAULT]"]

happyForwarding = DarcsMultipleChoiceOption
                   [DarcsNoArgOption [] ["happy-forwarding"] HappyForwarding
                   "forward unsigned messages without extra header",
                    DarcsNoArgOption [] ["no-happy-forwarding"] NoHappyForwarding
                   "don't forward unsigned messages without extra header [DEFAULT]"]

setDefault :: Bool -> DarcsOption
setDefault wantYes
  | wantYes   = mkMutuallyExclusive [yes,no] defaultyes []
  | otherwise = mkMutuallyExclusive [yes,no] defaultno  []
 where
  yes = ( DarcsNoArgOption [] ["set-default"], SetDefault True
        , "set default repository" ++ defaultText wantYes )
  no  = ( DarcsNoArgOption [] ["no-set-default"], NoSetDefault True
        , "don't set default repository" ++ defaultText (not wantYes) )
  defaultyes = ( \f _ -> DarcsInternalOption f, SetDefault False, "" )
  defaultno = ( \f _ -> DarcsInternalOption f, NoSetDefault False, "" )
  defaultText True = " [DEFAULT]"
  defaultText False = ""

verify = DarcsMultipleChoiceOption
         [DarcsAbsPathOption [] ["verify"] Verify "PUBRING"
          "verify that the patch was signed by a key in PUBRING",
          DarcsAbsPathOption [] ["verify-ssl"] VerifySSL "KEYS"
          "verify using openSSL with authorized keys from file KEYS",
          DarcsNoArgOption [] ["no-verify"] NonVerify
          "don't verify patch signature"]

reponame :: DarcsOption
reponame = DarcsSingleOption $
           DarcsArgOption [] ["repo-name","repodir"] NewRepo "DIRECTORY"
           "path of output directory" --repodir is there for compatibility
                                      --should be removed eventually
depsSel :: DarcsOption
depsSel = DarcsMultipleChoiceOption
       [DarcsNoArgOption [] ["no-deps"] DontGrabDeps
        "don't automatically fulfill dependencies",
        DarcsNoArgOption [] ["dont-prompt-for-dependencies"] DontPromptForDependencies
        "don't ask about patches that are depended on by matched patches (with --match or --patch)",
        DarcsNoArgOption [] ["prompt-for-dependencies"] PromptForDependencies
        "prompt about patches that are depended on by matched patches [DEFAULT]"]

tokens :: DarcsOption
tokens = DarcsSingleOption $
         DarcsArgOption [] ["token-chars"] Toks "\"[CHARS]\""
         "define token to contain these characters"

partial :: DarcsOption
partial       = DarcsMultipleChoiceOption [__lazy, __complete]

__lazy, __complete :: DarcsAtomicOption
__lazy = DarcsNoArgOption [] ["lazy"] Lazy
              "get patch files only as needed"
__complete = DarcsNoArgOption [] ["complete"] Complete
             "get a complete copy of the repository"

forceReplace = DarcsMultipleChoiceOption
                [DarcsNoArgOption ['f'] ["force"] ForceReplace
                 "proceed with replace even if 'new' token already exists",
                 DarcsNoArgOption [] ["no-force"]
                 NonForce "don't force the replace if it looks scary"]

reply = DarcsSingleOption $
  DarcsArgOption [] ["reply"] Reply "FROM" "reply to email-based patch using FROM address"
applyConflictOptions
    = DarcsMultipleChoiceOption
      [DarcsNoArgOption [] ["mark-conflicts"]
       MarkConflicts "mark conflicts",
       DarcsNoArgOption [] ["allow-conflicts"]
       AllowConflicts "allow conflicts, but don't mark them",
       DarcsNoArgOption [] ["no-resolve-conflicts"] NoAllowConflicts
       "equivalent to --dont-allow-conflicts, for backwards compatibility",
       DarcsNoArgOption [] ["dont-allow-conflicts","no-allow-conflicts"]
       NoAllowConflicts "fail if there are patches that would create conflicts [DEFAULT]",
       DarcsNoArgOption [] ["skip-conflicts"]
       SkipConflicts "filter out any patches that would create conflicts"
      ]
pullConflictOptions
    = DarcsMultipleChoiceOption
      [DarcsNoArgOption [] ["mark-conflicts"]
       MarkConflicts "mark conflicts [DEFAULT]",
       DarcsNoArgOption [] ["allow-conflicts"]
       AllowConflicts "allow conflicts, but don't mark them",
       DarcsNoArgOption [] ["dont-allow-conflicts","no-allow-conflicts"]
       NoAllowConflicts "fail if there are patches that would create conflicts",
       DarcsNoArgOption [] ["skip-conflicts"]
       SkipConflicts "filter out any patches that would create conflicts"
      ]
useExternalMerge = DarcsSingleOption $
  DarcsArgOption [] ["external-merge"] ExternalMerge "COMMAND"
    "use external tool to merge conflicts"

-- NOTE: I'd rather work to have no uses of dryRunNoxml, so that any time
-- --dry-run is a possibility, automated users can examine the results more
-- easily with --xml.
dryRunNoxml :: DarcsOption
dryRunNoxml = DarcsSingleOption $
  DarcsNoArgOption [] ["dry-run"] DryRun "don't actually take the action"

dryRun :: [DarcsOption]
dryRun = [dryRunNoxml, xmloutput]

noskipBoring = DarcsMultipleChoiceOption
                [DarcsNoArgOption [] ["boring"]
                 Boring "don't skip boring files",
                 DarcsNoArgOption [] ["no-boring"]
                 SkipBoring "skip boring files [DEFAULT]"]

allowProblematicFilenames = DarcsMultipleChoiceOption
                [DarcsNoArgOption [] ["case-ok"] AllowCaseOnly
                 "don't refuse to add files differing only in case"
                ,DarcsNoArgOption [] ["no-case-ok"] DontAllowCaseOnly
                 "refuse to add files whose name differ only in case [DEFAULT]"
                ,DarcsNoArgOption [] ["reserved-ok"] AllowWindowsReserved
                 "don't refuse to add files with Windows-reserved names"
                ,DarcsNoArgOption [] ["no-reserved-ok"] DontAllowWindowsReserved
                 "refuse to add files with Windows-reserved names [DEFAULT]"]

diffflags = DarcsSingleOption $
            DarcsArgOption [] ["diff-opts"]
            DiffFlags "OPTIONS" "options to pass to diff"

changesFormat = concatOptions
                 [DarcsMultipleChoiceOption [
                   DarcsNoArgOption [] ["context"]
                    GenContext "give output suitable for get --context" ],
                  xmloutput,
                  humanReadable,
                  DarcsMultipleChoiceOption [
                  DarcsNoArgOption [] ["number"] NumberPatches "number the changes",
                  DarcsNoArgOption [] ["count"] Count "output count of changes" ]
                 ]
changesReverse = DarcsMultipleChoiceOption
                  [DarcsNoArgOption [] ["reverse"] Reverse
                   "show changes in reverse order"
                  ,DarcsNoArgOption [] ["no-reverse"] Forward
                   "show changes in the usual order [DEFAULT]"]

onlyToFiles :: DarcsOption
onlyToFiles = DarcsMultipleChoiceOption
    [DarcsNoArgOption [] ["only-to-files"] OnlyChangesToFiles
     "show only changes to specified files",
     DarcsNoArgOption [] ["no-only-to-files"] ChangesToAllFiles
     "show changes to all files [DEFAULT]"]


humanReadable = DarcsSingleOption $
  DarcsNoArgOption [] ["human-readable"] HumanReadable "give human-readable output"

machineReadable = DarcsSingleOption $
  DarcsNoArgOption [] ["machine-readable"] MachineReadable "give machine-readable output"

pipe :: DarcsAtomicOption
pipe =
  DarcsNoArgOption [] ["pipe"] Pipe "ask user interactively for the patch metadata"

interactive :: DarcsAtomicOption
interactive =
    DarcsNoArgOption ['i'] ["interactive"] Interactive
                         "prompt user interactively"

allPatches :: DarcsAtomicOption
allPatches =
  DarcsNoArgOption ['a'] ["all"] All "answer yes to all patches"

allInteractive = DarcsMultipleChoiceOption [allPatches, interactive]

allPipeInteractive
    = DarcsMultipleChoiceOption [allPatches,pipe,interactive]

pipeInteractive =
    DarcsMultipleChoiceOption [pipe, interactive]

repoCombinator =
    DarcsMultipleChoiceOption
    [DarcsNoArgOption [] ["intersection"] Intersection
     "take intersection of all repositories",
     DarcsNoArgOption [] ["union"] Union
     "take union of all repositories [DEFAULT]",
     DarcsNoArgOption [] ["complement"] Complement
     "take complement of repositories (in order listed)"]

optionsLatex :: [DarcsOption] -> String
optionsLatex opts = "\\begin{tabular}{lll}\n"++
                     unlines (map optionListLatex opts)++
                     "\\end{tabular}\n"

latexHelp :: String -> String
latexHelp h
    = "\\begin{minipage}{7cm}\n\\raggedright\n" ++ h ++ "\\end{minipage}\n"

optionListLatex :: DarcsOption -> String
optionListLatex (DarcsSingleOption o) = optionLatex o
optionListLatex (DarcsMultipleChoiceOption os) = unlines (map optionLatex os)
optionListLatex (DarcsMutuallyExclusive os _) = unlines (map optionLatex os)

optionLatex :: DarcsAtomicOption -> String
optionLatex (DarcsInternalOption _) = ""
optionLatex (DarcsNoArgOption a b _ h) =
    showShortOptions a ++ showLongOptions b ++ latexHelp h ++ "\\\\"
optionLatex (DarcsArgOption a b _ arg h) =
    showShortOptions a ++
    showLongOptions (map (++(" "++arg)) b) ++ latexHelp h ++ "\\\\"
optionLatex (DarcsAbsPathOrStdOption a b _ arg h) =
    showShortOptions a ++
    showLongOptions (map (++(" "++arg)) b) ++ latexHelp h ++ "\\\\"
optionLatex (DarcsAbsPathOption a b _ arg h) =
    showShortOptions a ++
    showLongOptions (map (++(" "++arg)) b) ++ latexHelp h ++ "\\\\"
optionLatex (DarcsOptAbsPathOption a b _ _ arg h) =
    showShortOptions a ++
    showLongOptions (map (++("[="++arg++"]")) b) ++ latexHelp h ++ "\\\\"

showShortOptions :: [Char] -> String
showShortOptions [] = "&"
showShortOptions [c] = "\\verb!-"++[c]++"! &"
showShortOptions (c:cs) = "\\verb!-"++[c]++"!,"++showShortOptions cs

showLongOptions :: [String] -> String
showLongOptions [] = " &"
showLongOptions [s] = "\\verb!--" ++ s ++ "! &"
showLongOptions (s:ss)
    = "\\verb!--" ++ s ++ "!,"++ showLongOptions ss

setScriptsExecutableOption :: DarcsOption
setScriptsExecutableOption = DarcsMultipleChoiceOption
                              [DarcsNoArgOption [] ["set-scripts-executable"] SetScriptsExecutable
                               "make scripts executable",
                               DarcsNoArgOption [] ["dont-set-scripts-executable","no-set-scripts-executable"] DontSetScriptsExecutable
                               "don't make scripts executable"]

testStrategy :: DarcsOption
testStrategy = DarcsMultipleChoiceOption
    [ __once
    , __linear
    , __backoff
    , __bisect
    ]

__once, __linear, __bisect, __backoff :: DarcsAtomicOption
__once = DarcsNoArgOption [] ["once"] Once
                "run test on current version only [DEFAULT]"
__linear = DarcsNoArgOption [] ["linear"] Linear
                "locate the most recent version lacking an error"
__backoff = DarcsNoArgOption [] ["backoff"] Backoff
             "exponential backoff search"
__bisect = DarcsNoArgOption [] ["bisect"] Bisect
             "binary instead of linear search"

relink, sibling :: DarcsOption
relink = DarcsSingleOption $ DarcsNoArgOption [] ["relink"] Relink
         "relink random internal data to a sibling"

sibling = DarcsSingleOption $
  DarcsAbsPathOption [] ["sibling"] Sibling "URL"
          "specify a sibling directory"

-- | 'flagsToSiblings' collects the contents of all @Sibling@ flags in a list of flags.
flagsToSiblings :: [DarcsFlag] -> [AbsolutePath]
flagsToSiblings fs = [ s | Sibling s <- fs ]

reorderPatches :: DarcsOption
reorderPatches = DarcsSingleOption $ DarcsNoArgOption [] ["reorder-patches"] Reorder
                  "reorder the patches in the repository"

sendmailCmd = DarcsSingleOption $
  DarcsArgOption [] ["sendmail-command"] SendmailCmd "COMMAND" "specify sendmail command"

environmentHelpSendmail :: ([String], [String])
environmentHelpSendmail = (["SENDMAIL"], [
 "On Unix, the `darcs send' command relies on sendmail(8).  The",
 "`--sendmail-command' or $SENDMAIL environment variable can be used to",
 "provide an explicit path to this program; otherwise the standard",
 "locations /usr/sbin/sendmail and /usr/lib/sendmail will be tried."])
-- FIXME: mention the following also:
-- * sendmail(8) is not sendmail-specific;
-- * nowadays, desktops often have no MTA or an unconfigured MTA --
--   which is awful, because it accepts mail but doesn't relay it;
-- * in this case, can be a sendmail(8)-emulating wrapper on top of an
--   MUA that sends mail directly to a smarthost; and
-- * on a multi-user system without an MTA and on which you haven't
--   got root, can be msmtp.

-- |'getSendmailCmd' takes a list of flags and returns the sendmail command
-- to be used by @darcs send@. Looks for a command specified by
-- @SendmailCmd \"command\"@ in that list of flags, if any.
-- This flag is present if darcs was invoked with @--sendmail-command=COMMAND@
-- Alternatively the user can set @$S@@ENDMAIL@ which will be used as a fallback if present.
getSendmailCmd :: [DarcsFlag] -> IO String
getSendmailCmd (SendmailCmd a:_) = return a
getSendmailCmd (_:flags) = getSendmailCmd flags
getSendmailCmd [] =   do easy_sendmail <- firstJustIO [ maybeGetEnv "SENDMAIL" ]
                         case easy_sendmail of
                            Just a -> return a
                            Nothing -> return ""

files :: DarcsOption
files = DarcsMultipleChoiceOption
        [DarcsNoArgOption [] ["files"] Files
         "include files in output [DEFAULT]",
         DarcsNoArgOption [] ["no-files"] NoFiles
         "don't include files in output"]

directories :: DarcsOption
directories = DarcsMultipleChoiceOption
              [DarcsNoArgOption [] ["directories"] Directories
               "include directories in output [DEFAULT]",
               DarcsNoArgOption [] ["no-directories"] NoDirectories
               "don't include directories in output"]

pending :: DarcsOption
pending = DarcsMultipleChoiceOption
              [DarcsNoArgOption [] ["pending"] Pending
               "reflect pending patches in output [DEFAULT]",
               DarcsNoArgOption [] ["no-pending"] NoPending
               "only included recorded patches in output"]

nullFlag :: DarcsOption        -- "null" is already taken
nullFlag = DarcsSingleOption $ DarcsNoArgOption ['0'] ["null"] NullFlag
       "separate file names by NUL characters"

posthookCmd :: DarcsOption
posthookCmd = DarcsMultipleChoiceOption
               [DarcsArgOption [] ["posthook"] PosthookCmd
                "COMMAND" "specify command to run after this darcs command",
                DarcsNoArgOption [] ["no-posthook"] NoPosthook
                "don't run posthook command"]

posthookPrompt :: DarcsOption
posthookPrompt = DarcsMultipleChoiceOption
                  [DarcsNoArgOption [] ["prompt-posthook"] AskPosthook
                   "prompt before running posthook [DEFAULT]",
                   DarcsNoArgOption [] ["run-posthook"] RunPosthook
                   "run posthook command without prompting"]

prehookCmd :: DarcsOption
prehookCmd = DarcsMultipleChoiceOption
               [DarcsArgOption [] ["prehook"] PrehookCmd
                "COMMAND" "specify command to run before this darcs command",
                DarcsNoArgOption [] ["no-prehook"] NoPrehook
                "don't run prehook command"]

prehookPrompt :: DarcsOption
prehookPrompt = DarcsMultipleChoiceOption
                  [DarcsNoArgOption [] ["prompt-prehook"] AskPrehook
                   "prompt before running prehook [DEFAULT]",
                   DarcsNoArgOption [] ["run-prehook"] RunPrehook
                   "run prehook command without prompting"]

networkOptions :: [DarcsOption]
networkOptions =
   [ DarcsMultipleChoiceOption
       [ DarcsNoArgOption [] ["no-http-pipelining"] NoHTTPPipelining
                          "disable HTTP pipelining"
       ]
   , remoteDarcs ]

remoteDarcs :: DarcsOption
remoteDarcs = DarcsSingleOption $
  DarcsArgOption [] ["remote-darcs"] RemoteDarcsOpt "COMMAND"
                "name of the darcs executable on the remote server"

noCache :: DarcsOption
noCache = DarcsSingleOption $
  DarcsNoArgOption [] ["no-cache"] NoCache
                          "don't use patch caches"

optimizePristine :: DarcsOption
optimizePristine = DarcsSingleOption $
  DarcsNoArgOption [] ["pristine"] OptimizePristine
                          "optimize hashed pristine layout"

optimizeHTTP :: DarcsOption
optimizeHTTP = DarcsSingleOption $
  DarcsNoArgOption [] ["http"] OptimizeHTTP
                          "optimize repository for getting over network"

usePacks :: DarcsOption
usePacks = DarcsMultipleChoiceOption
  [ DarcsNoArgOption [] ["packs"] Packs "use repository packs [DEFAULT]"
  , DarcsNoArgOption [] ["no-packs"] NoPacks
      "don't use repository packs"
  ]

umaskOption :: DarcsOption
umaskOption = DarcsSingleOption $
    DarcsArgOption [] ["umask"] UMask "UMASK"
        "specify umask to use when writing"

restrictPaths :: DarcsOption
restrictPaths =
    DarcsMultipleChoiceOption
    [DarcsNoArgOption [] ["restrict-paths"] RestrictPaths
     "don't allow darcs to touch external files or repo metadata",
     DarcsNoArgOption [] ["dont-restrict-paths","no-restrict-paths"] DontRestrictPaths
     "allow darcs to modify any file or directory (unsafe)"]

allowUnrelatedRepos = DarcsSingleOption $
    DarcsNoArgOption [] ["ignore-unrelated-repos"] AllowUnrelatedRepos
                         "do not check if repositories are unrelated"

justThisRepo :: DarcsOption
justThisRepo = DarcsSingleOption $
    DarcsNoArgOption [] ["just-this-repo"] JustThisRepo
                        "Limit the check or repair to the current repo"

check, repair, checkOrRepair :: DarcsOption
check = DarcsSingleOption $
    DarcsNoArgOption [] ["check"] Check
                        "Specify checking mode"
repair = DarcsSingleOption $
    DarcsNoArgOption [] ["repair"] Repair
                        "Specify repair mode"
checkOrRepair = concatOptions [check, repair]

-- | @'patchSelectFlag' f@ holds whenever @f@ is a way of selecting
-- patches such as @PatchName n@.
patchSelectFlag :: DarcsFlag -> Bool
patchSelectFlag All = True
patchSelectFlag (PatchName _) = True
patchSelectFlag (OnePatch _) = True
patchSelectFlag (SeveralPatch _) = True
patchSelectFlag (AfterPatch _) = True
patchSelectFlag (UpToPatch _) = True
patchSelectFlag (TagName _) = True
patchSelectFlag (LastN _) = True
patchSelectFlag (OneTag _) = True
patchSelectFlag (AfterTag _) = True
patchSelectFlag (UpToTag _) = True
patchSelectFlag (OnePattern _) = True
patchSelectFlag (SeveralPattern _) = True
patchSelectFlag (AfterPattern _) = True
patchSelectFlag (UpToPattern _) = True
patchSelectFlag _ = False

-- | The integer corresponding to a string, if it's only composed of digits.
--   Otherwise, -1.
numberString :: String -> Int
numberString "" = -1
numberString s = if all isDigit s then read s else (-1)
