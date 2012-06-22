--  Copyright (C) 2003 David Roundy
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

module Darcs.UI.ArgumentDefaults ( getDefaultFlags ) where
import Data.Maybe ( listToMaybe, mapMaybe )

import Darcs.UI.Arguments ( DarcsFlag,
                         atomicOptions, DarcsAtomicOption( .. ), DarcsOption ( .. ),
                         applyDefaults,
                         arein )
import Darcs.UI.Commands ( CommandControl( CommandData ),
                        commandAlloptions )
import Darcs.UI.Commands.Help ( commandControlList )
import Darcs.Repository.Prefs ( getGlobal, getPreflist )

getDefaultFlags :: String -> [DarcsOption] -> [DarcsFlag] -> IO [DarcsFlag]
getDefaultFlags com com_opts already = do
    repo_defs   <- defaultContent $ getPreflist "defaults"
    global_defs <- defaultContent $ getGlobal   "defaults"
    let repo_flags = getFlagsFrom com com_opts already repo_defs
        global_flags = getFlagsFrom com com_opts
                                          (already++repo_flags) global_defs
    return $ applyDefaults com_opts     -- hard-coded defaults (respects user preferences)
           $ repo_flags ++ global_flags -- user preferences

getFlagsFrom :: String -> [DarcsOption] -> [DarcsFlag] -> [(String,String,String)] -> [DarcsFlag]
getFlagsFrom com com_opts already defs =
    options_for com_defs com_opts com_opts ++
    options_for all_defs com_opts all_opts
    where com_defs = filter (\ (c,_,_) -> c == com) defs
          all_defs = filter (\ (c,_,_) -> c == "ALL") defs
          options_for d o ao = concatMap (findOption o ao already) d
          all_opts = concatMap get_opts commandControlList
          get_opts (CommandData c) = let (o1, o2) = commandAlloptions c
                                      in o1 ++ o2
          get_opts _                = []

findOption :: [DarcsOption] -> [DarcsOption] -> [DarcsFlag] -> (String,String,String) -> [DarcsFlag]
findOption opts all_opts already (c, f, d) =
    if null $ mapMaybe choose_option all_opts
    then error $ "Bad default option: command '"++c++"' has no option '"++f++"'."
    else concat $ mapMaybe choose_option opts
    where choose_atomic_option (DarcsNoArgOption _ fls o _)
              | f `elem` fls = if null d
                               then Just [o]
                               else error $ "Bad default option: '"++f
                                        ++"' takes no argument, but '"++d
                                        ++"' argument given."
          choose_atomic_option (DarcsArgOption _ fls o _ _)
              | f `elem` fls = if null d
                               then error $ "Bad default option: '"++f
                                        ++"' requires an argument, but no "
                                        ++"argument given."
                               else Just [o d]
          choose_atomic_option _ = Nothing
          choose_option o
              | o `arein` already = Just []
              | otherwise = listToMaybe $ mapMaybe choose_atomic_option $ atomicOptions o

defaultContent :: IO [String] -> IO [(String,String,String)]
defaultContent = fmap (mapMaybe (doline . words))
    where doline (c:a:r) = Just (c, drop_dashdash a, unwords r)
          doline _ = Nothing
          drop_dashdash ('-':'-':a) = a
          drop_dashdash a = a
