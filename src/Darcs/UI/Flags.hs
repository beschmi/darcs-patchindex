-- Copyright (C) 2002-2004 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Darcs.UI.Flags
    ( DarcsFlag( .. )
    , compression
    , remoteDarcs
    , diffingOpts
    , externalMerge
    , wantGuiPause
    , isInteractive
    , maxCount
    , willIgnoreTimes
    , willRemoveLogFile
    , isUnified
    , isNotUnified
    , doHappyForwarding
    , includeBoring
    , doAllowCaseOnly
    , doAllowWindowsReserved
    , doReverse
    , usePacks
    , showChangesOnlyToFiles
    , removeFromAmended
    , defaultFlag
    , getPosthookCmd
    , getPrehookCmd
    , toMatchFlags
    , verbosity
    , useCache
    , umask
    , dryRun
    , lookForAdds
    , runTest
    , setScriptsExecutable
    , leaveTestDir
    , remoteRepos
    , setDefault
    , getKind
    , workRepo
    , allowConflicts
    ) where


import Data.List ( find )
import Data.Maybe( fromMaybe )
import Darcs.Path ( AbsolutePath, AbsolutePathOrStd )
import qualified Darcs.Patch.Match as MF ( MatchFlag(..) )
import qualified Darcs.Repository.Flags as RF
    ( Compression (..)
    , RemoteDarcs (..)
    , Verbosity (..)
    , UseCache (..)
    , UMask (..)
    , DryRun (..)
    , LookForAdds (..)
    , RunTest (..)
    , SetScriptsExecutable (..)
    , LeaveTestDir (..)
    , RemoteRepos (..)
    , SetDefault (..)
    , UseIndex (..)
    , ScanKnown (..)
    , GetKind (..)
    , ExternalMerge (..)
    , WorkRepo (..)
    , AllowConflicts (..)
    , WantGuiPause (..)
    )

-- | The 'DarcsFlag' type is a list of all flags that can ever be
-- passed to darcs, or to one of its commands.
data DarcsFlag = Help | ListOptions | NoTest | Test
               | OnlyChangesToFiles | ChangesToAllFiles
               | LeaveTestDir | NoLeaveTestDir
               | Timings | Debug | DebugVerbose | DebugHTTP
               | Verbose | NormalVerbosity | Quiet
               | Target String | Cc String
               | Output AbsolutePathOrStd | OutputAutoName AbsolutePath
               | Subject String | InReplyTo String | Charset String
               | SendmailCmd String | Author String | PatchName String
               | OnePatch String | SeveralPatch String
               | AfterPatch String | UpToPatch String
               | TagName String | LastN Int | MaxCount Int | PatchIndexRange Int Int
               | NumberPatches
               | OneTag String | AfterTag String | UpToTag String
               | GenContext | Context AbsolutePath | Count
               | LogFile AbsolutePath | RmLogFile | DontRmLogFile
               | DistName String | All
               | Recursive | NoRecursive | Reorder
               | RestrictPaths | DontRestrictPaths
               | AskDeps | NoAskDeps | IgnoreTimes | DontIgnoreTimes
               | LookForAdds | NoLookForAdds
               | AnyOrder | CreatorHash String
               | Intersection | Union | Complement
               | Sign | SignAs String | NoSign | SignSSL String
               | HappyForwarding | NoHappyForwarding
               | Verify AbsolutePath | VerifySSL AbsolutePath
               | RemoteDarcsOpt String
               | EditDescription | NoEditDescription
               | Toks String
               | EditLongComment | NoEditLongComment | PromptLongComment
               | KeepDate | NoKeepDate
               | AllowConflicts | MarkConflicts | NoAllowConflicts
               | SkipConflicts
               | Boring | SkipBoring
               | AllowCaseOnly | DontAllowCaseOnly
               | AllowWindowsReserved | DontAllowWindowsReserved
               | DontGrabDeps | DontPromptForDependencies | PromptForDependencies
               | Compress | NoCompress | UnCompress
               | WorkRepoDir String | WorkRepoUrl String | RemoteRepo String
               | NewRepo String
               | Reply String | ApplyAs String
               | MachineReadable | HumanReadable
               | Pipe | Interactive
               | DiffCmd String
               | ExternalMerge String | Summary | NoSummary
               | PauseForGui | NoPauseForGui
               | Unified | NonUnified | Reverse | Forward
               | Complete | Lazy
               | FixFilePath AbsolutePath AbsolutePath | DiffFlags String
               | XMLOutput
               | ForceReplace
               | OnePattern String | SeveralPattern String
               | AfterPattern String | UpToPattern String
               | NonApply | NonVerify | NonForce
               | DryRun

               -- The Bool parameters are a bit of a hack so that we can tell
               -- whether the user explicitly set the option or not.
               -- A more general mechanism would be better.
               -- True = explicitly set by user (on command-line or in prefs/defaults),
               -- False = defaulted by darcs
               | SetDefault Bool | NoSetDefault Bool

               | Disable | SetScriptsExecutable | DontSetScriptsExecutable
               | Once | Linear | Backoff | Bisect
               | UseHashedInventory
               | UseFormat2 | UseNoWorkingDir | UseWorkingDir
               | NoUpdateWorking
               | Sibling AbsolutePath | Relink
               | OptimizePristine | OptimizeHTTP
               | UpgradeFormat
               | Files | NoFiles | Directories | NoDirectories
               | Pending | NoPending
               | PosthookCmd String | NoPosthook | AskPosthook | RunPosthook
               | PrehookCmd String  | NoPrehook  | AskPrehook  | RunPrehook
               | UMask String
               | StoreInMemory | ApplyOnDisk
               | NoHTTPPipelining
               | Packs | NoPacks
               | NoCache
               | AllowUnrelatedRepos
               | Check | Repair | JustThisRepo
               | NullFlag
               | NoAmendUnrecord | AmendUnrecord
               | PatchIndexFlag
               | NoPatchIndexFlag
                 deriving ( Eq, Show )

compression :: [DarcsFlag]
            -> RF.Compression
compression f
    | NoCompress `elem` f = RF.NoCompression
    | otherwise           = RF.GzipCompression


remoteDarcs :: [DarcsFlag]
            -> RF.RemoteDarcs
remoteDarcs f
    | (x:_) <- [ c | RemoteDarcsOpt c <- f ] = RF.RemoteDarcs x
    | otherwise = RF.DefaultRemoteDarcs


diffingOpts :: [DarcsFlag]
            -> (RF.UseIndex, RF.ScanKnown)
diffingOpts opts = (index, scan)
  where
    index = if willIgnoreTimes opts
            then RF.IgnoreIndex
            else RF.UseIndex
    scan =
        if LookForAdds `elem` opts
        then
          if Boring `elem` opts
          then RF.ScanBoring
          else RF.ScanAll
        else RF.ScanKnown


externalMerge :: [DarcsFlag] -> RF.ExternalMerge
externalMerge [] = RF.NoExternalMerge
externalMerge (ExternalMerge c:_) = RF.YesExternalMerge c
externalMerge (_:fs) = externalMerge fs


wantGuiPause :: [DarcsFlag] -> RF.WantGuiPause
wantGuiPause fs = if (hasDiffCmd || hasExternalMerge) && hasPause then RF.YesWantGuiPause else RF.NoWantGuiPause
  where
    hasDiffCmd = any isDiffCmd fs
    hasExternalMerge = externalMerge fs /= RF.NoExternalMerge
    isDiffCmd (DiffCmd _) = True
    isDiffCmd _ = False
    hasPause = maybe True (==PauseForGui) $ find isPauseFlag $ reverse fs
    isPauseFlag f = (f==PauseForGui) || (f==NoPauseForGui)


isInteractive :: [DarcsFlag] -> Bool
isInteractive = isInteractive_ True
    where
      isInteractive_ def [] = def
      isInteractive_ _ (Interactive:_) = True
      isInteractive_ _ (All:_) = False
      isInteractive_ _ (DryRun:fs) = isInteractive_ False fs
      isInteractive_ def (_:fs) = isInteractive_ def fs

maxCount :: [DarcsFlag] -> Maybe Int
maxCount (MaxCount n : _) = Just n
maxCount (_:xs) = maxCount xs
maxCount [] = Nothing

-- | @lastWord [(flag, value)] default opts@ scans @opts@ for a flag
-- in the list and returns the value of the first match, or @default@
-- if none is found.
--
-- We call this the \"last\" word because we assume that flags are
-- *prepended* in the order they arrive, so what is first internally
-- is last from the user's point of view.
lastWord :: [(DarcsFlag,a)] -> a -> [DarcsFlag] -> a
lastWord known_flags = foldr . flip $ \ def -> fromMaybe def . flip lookup known_flags

getBoolFlag :: DarcsFlag -> DarcsFlag -> [DarcsFlag] -> Bool
getBoolFlag t f = lastWord [(t, True), (f, False)] False

willIgnoreTimes :: [DarcsFlag] -> Bool
willIgnoreTimes = getBoolFlag IgnoreTimes DontIgnoreTimes

willRemoveLogFile :: [DarcsFlag] -> Bool
willRemoveLogFile = getBoolFlag RmLogFile DontRmLogFile

isUnified :: [DarcsFlag] -> Bool
isUnified = getBoolFlag Unified NonUnified

isNotUnified :: [DarcsFlag] -> Bool
isNotUnified = getBoolFlag NonUnified Unified

doHappyForwarding :: [DarcsFlag] -> Bool
doHappyForwarding = getBoolFlag HappyForwarding NoHappyForwarding

includeBoring :: [DarcsFlag] -> Bool
includeBoring = getBoolFlag Boring SkipBoring

doAllowCaseOnly :: [DarcsFlag] -> Bool
doAllowCaseOnly = getBoolFlag AllowCaseOnly DontAllowCaseOnly


doAllowWindowsReserved :: [DarcsFlag] -> Bool
doAllowWindowsReserved = getBoolFlag AllowWindowsReserved DontAllowWindowsReserved

doReverse :: [DarcsFlag] -> Bool
doReverse = getBoolFlag Reverse Forward

usePacks :: [DarcsFlag] -> Bool
usePacks = getBoolFlag Packs NoPacks

showChangesOnlyToFiles :: [DarcsFlag] -> Bool
showChangesOnlyToFiles = getBoolFlag OnlyChangesToFiles ChangesToAllFiles

-- | Set flags to a default value, but only one has not already been provided
defaultFlag :: [DarcsFlag] -- ^ distractors
            -> DarcsFlag   -- ^ default
            -> [DarcsFlag] -- ^ flags
            -> [DarcsFlag] -- ^ updated flags
defaultFlag alts def flags =
 if any (`elem` flags) alts then flags else def : flags

removeFromAmended :: [DarcsFlag] -> Bool
removeFromAmended = getBoolFlag AmendUnrecord NoAmendUnrecord

-- | 'getPosthookCmd' takes a list of flags and returns the posthook command
--  specified by @PosthookCmd a@ in that list of flags, if any.
getPosthookCmd :: [DarcsFlag] -> Maybe String
getPosthookCmd (PosthookCmd a:_) = Just a
getPosthookCmd (NoPosthook:_) = Nothing
getPosthookCmd (_:flags) = getPosthookCmd flags
getPosthookCmd [] = Nothing

-- | 'getPrehookCmd' takes a list of flags and returns the prehook command
--  specified by @PrehookCmd a@ in that list of flags, if any.
getPrehookCmd :: [DarcsFlag] -> Maybe String
getPrehookCmd (PrehookCmd a:_) = Just a
getPrehookCmd (NoPrehook:_) = Nothing
getPrehookCmd (_:flags) = getPrehookCmd flags
getPrehookCmd [] = Nothing


toMatchFlags :: [DarcsFlag] -> [MF.MatchFlag]
toMatchFlags df = concatMap toMatchFlag df
 where
  toMatchFlag (OnePattern s)        = [MF.OnePattern s]
  toMatchFlag (SeveralPattern s)    = [MF.SeveralPattern s]
  toMatchFlag (AfterPattern s)      = [MF.AfterPattern s]
  toMatchFlag (UpToPattern s)       = [MF.UpToPattern s]
  toMatchFlag (OnePatch s)          = [MF.OnePatch s]
  toMatchFlag (SeveralPatch s)      = [MF.SeveralPatch s]
  toMatchFlag (AfterPatch s)        = [MF.AfterPatch s]
  toMatchFlag (UpToPatch s)         = [MF.UpToPatch s]
  toMatchFlag (OneTag s)            = [MF.OneTag s]
  toMatchFlag (AfterTag s)          = [MF.AfterTag s]
  toMatchFlag (UpToTag s)           = [MF.UpToTag s]
  toMatchFlag (LastN i)             = [MF.LastN i]
  toMatchFlag (PatchIndexRange b e) = [MF.PatchIndexRange b e]
  toMatchFlag (Context p)           = [MF.Context p]
  toMatchFlag _                     = []


verbosity :: [DarcsFlag] -> RF.Verbosity
verbosity (Quiet:_) = RF.Quiet
verbosity (Verbose:_) = RF.Verbose
verbosity (NormalVerbosity:_) = RF.NormalVerbosity
verbosity [] = RF.NormalVerbosity
verbosity (_:fs) = verbosity fs

useCache :: [DarcsFlag] -> RF.UseCache
useCache fs | NoCache `elem` fs = RF.NoUseCache
useCache _ = RF.YesUseCache

umask :: [DarcsFlag] -> RF.UMask
umask fs = case [ s | UMask s <- fs ] of (s:_) -> RF.YesUMask s ; [] -> RF.NoUMask

dryRun :: [DarcsFlag] -> RF.DryRun
dryRun fs | DryRun `elem` fs = RF.YesDryRun
dryRun _ = RF.NoDryRun

lookForAdds :: [DarcsFlag] -> RF.LookForAdds
lookForAdds fs | LookForAdds `elem` fs = RF.YesLookForAdds
lookForAdds _ = RF.NoLookForAdds

runTest :: [DarcsFlag] -> RF.RunTest
runTest fs | Test `elem` fs = RF.YesRunTest
runTest _ = RF.NoRunTest

setScriptsExecutable :: [DarcsFlag] -> RF.SetScriptsExecutable
setScriptsExecutable fs | SetScriptsExecutable `elem` fs = RF.YesSetScriptsExecutable
setScriptsExecutable _ = RF.NoSetScriptsExecutable

leaveTestDir :: [DarcsFlag] -> RF.LeaveTestDir
leaveTestDir fs | LeaveTestDir `elem` fs = RF.YesLeaveTestDir
leaveTestDir _ = RF.NoLeaveTestDir

remoteRepos :: [DarcsFlag] -> RF.RemoteRepos
remoteRepos fs = RF.RemoteRepos [ s | RemoteRepo s <- fs]

setDefault :: [DarcsFlag] -> RF.SetDefault
setDefault fs = case ( [ b | SetDefault b <- fs], [ b | NoSetDefault b <- fs] ) of
                 ((True:_), _      )   -> RF.YesSetDefault True
                 ( _      ,(True:_))   -> RF.NoSetDefault True
                 ((False:_), _     )   -> RF.YesSetDefault False
                 _                     -> RF.NoSetDefault False

getKind :: [DarcsFlag] -> RF.GetKind
getKind fs | Lazy `elem` fs = RF.LazyGet
getKind fs | Complete `elem` fs = RF.CompleteGet
getKind _ = RF.NormalGet

workRepo :: [DarcsFlag] -> RF.WorkRepo
workRepo fs = case ([ s | WorkRepoDir s <- fs ], [ s | WorkRepoUrl  s <- fs]) of
               ((s:_) ,  _    ) -> RF.WorkRepoDir s
               ( _    , (s:_) ) -> RF.WorkRepoURL s
               _                -> RF.WorkRepoCurrentDir

allowConflicts :: [DarcsFlag] -> RF.AllowConflicts
allowConflicts fs | AllowConflicts `elem` fs = RF.YesAllowConflicts
allowConflicts fs | MarkConflicts  `elem` fs = RF.YesAllowConflictsAndMark
allowConflicts _  = RF.NoAllowConflicts

