module Darcs.Repository.Flags
    (
      Compression (..)
    , RemoteDarcs (..)
    , Verbosity (..)
    , UpdateWorking (..)
    , UseCache (..)
    , DryRun (..)
    , UMask (..)
    , LookForAdds (..)
    , RunTest (..)
    , SetScriptsExecutable (..)
    , LeaveTestDir (..)
    , RemoteRepos (..)
    , SetDefault (..)
    , UseIndex (..)
    , ScanKnown (..)
    , GetKind (..)
    , AllowConflicts (..)
    , ExternalMerge (..)
    , WorkRepo (..)
    , WantGuiPause (..)
    ) where

data Verbosity = Quiet | NormalVerbosity | Verbose
    deriving ( Eq )

data Compression = NoCompression
                 | GzipCompression
    deriving ( Eq )

data RemoteDarcs = RemoteDarcs String
                 | DefaultRemoteDarcs
    deriving ( Eq )

data UpdateWorking = YesUpdateWorking | NoUpdateWorking
    deriving ( Eq )

data UseCache = YesUseCache | NoUseCache
    deriving ( Eq )

data DryRun = YesDryRun | NoDryRun
    deriving ( Eq )

data UMask = YesUMask String | NoUMask
    deriving ( Eq )

data LookForAdds = YesLookForAdds | NoLookForAdds
    deriving ( Eq )

data RunTest = YesRunTest | NoRunTest
    deriving ( Eq )

data SetScriptsExecutable = YesSetScriptsExecutable | NoSetScriptsExecutable
    deriving ( Eq )

data LeaveTestDir = YesLeaveTestDir | NoLeaveTestDir
    deriving ( Eq )

data RemoteRepos = RemoteRepos [String]
    deriving ( Eq )

data SetDefault = YesSetDefault Bool | NoSetDefault Bool
    deriving ( Eq )

data UseIndex = UseIndex | IgnoreIndex

data ScanKnown = ScanKnown -- ^Just files already known to darcs
               | ScanAll -- ^All files, i.e. look for new ones
               | ScanBoring -- ^All files, even boring ones

-- Various kinds of getting repositories
data GetKind = LazyGet       -- ^Just copy pristine and inventories
             | NormalGet     -- ^First do a lazy get then copy everything
             | CompleteGet   -- ^Same as Normal but omit telling user they can interrumpt
    deriving ( Eq )

data AllowConflicts = NoAllowConflicts | YesAllowConflicts | YesAllowConflictsAndMark
    deriving ( Eq )

data ExternalMerge = YesExternalMerge String | NoExternalMerge
    deriving ( Eq )

data WorkRepo = WorkRepoDir String | WorkRepoURL String | WorkRepoCurrentDir
    deriving ( Eq )

data WantGuiPause = YesWantGuiPause | NoWantGuiPause
    deriving ( Eq )

