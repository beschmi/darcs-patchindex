module Darcs.UI.Commands.ShowPatchIndex ( showPatchIndexFiles, showPatchIndexAll, showPatchIndexStatus ) where
import Darcs.UI.Arguments ( DarcsFlag(..), workingRepoDir,
                            files, directories, nullFlag)
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, amInHashedRepository )
import Darcs.UI.Flags ( useCache )
import Darcs.Repository.InternalTypes ( Repository(..) )
import Darcs.Repository ( withRepository, RepoJob(..) )
import Darcs.Repository.FileMod

showPatchIndexAll :: DarcsCommand
showPatchIndexAll = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "patch-index-all",
  commandDescription = "Dump complete content of patch index.",
  commandHelp =
      "The `darcs show patch-index all' command lists all information in the patch index",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = showPatchIndexAllCmd,
  commandPrereq = amInHashedRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = [files, directories, nullFlag, workingRepoDir] }

showPatchIndexFiles :: DarcsCommand
showPatchIndexFiles = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "patch-index-files",
  commandDescription = "Dump current files registered in patch index.",
  commandHelp =
      "The `darcs show patch-index files' command lists all current files registered in the patch index",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = showPatchIndexFilesCmd,
  commandPrereq = amInHashedRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = [files, directories, nullFlag, workingRepoDir] }

showPatchIndexStatus :: DarcsCommand
showPatchIndexStatus = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "patch-index-status",
  commandDescription = " Report patch-index status",
  commandHelp =
      "The `darcs show patch-index-status' reports if the patch index is in sync, out of sync, or does not exist",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = showPatchIndexStatus',
  commandPrereq = amInHashedRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = [files, directories, nullFlag, workingRepoDir] }

showPatchIndexAllCmd :: [DarcsFlag] -> [String] -> IO ()
showPatchIndexAllCmd opts _ = do
  withRepository (useCache opts) $ RepoJob $ \(Repo repodir _ _) -> do
    dumpPatchIndex repodir


showPatchIndexFilesCmd :: [DarcsFlag] -> [String] -> IO ()
showPatchIndexFilesCmd opts _ = withRepository (useCache opts) $ RepoJob $ \(Repo repodir _ _) -> do
  dumpPatchIndexFiles repodir

showPatchIndexStatus' :: [DarcsFlag] -> [String] -> IO ()
showPatchIndexStatus' opts _ = withRepository (useCache opts) $ RepoJob $ \(repo@(Repo repodir _ _)) -> do
  ex <- doesPatchIndexExist repodir
  if ex 
   then do
          sy <- isPatchIndexInSync repo 
          if sy
            then putStrLn "Patch Index is in sync with repo."
            else putStrLn "Patch Index is outdated. Run darcs optimize --patch-index"
   else putStrLn "Patch Index is not yet created. Run darcs optimize --patch-index"


