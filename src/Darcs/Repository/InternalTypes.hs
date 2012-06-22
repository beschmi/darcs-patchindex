-- Copyright (C) 2006-2007 David Roundy
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
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

{-# LANGUAGE CPP #-}


module Darcs.Repository.InternalTypes ( Repository(..), RepoType(..), Pristine(..)
                                      , extractCache, modifyCache
                                      ) where

import Data.List ( nub, sortBy )
import Darcs.Repository.Cache ( Cache (..) , compareByLocality )
import Darcs.Repository.Format ( RepoFormat )
import Darcs.Patch ( RepoPatch )

data Pristine
  = NoPristine
  | PlainPristine
  | HashedPristine
    deriving ( Show, Eq )

data Repository (p :: * -> * -> *) wRecordedstate wUnrecordedstate wTentativestate =
  Repo !String !RepoFormat !(RepoType p) deriving ( Show )

data RepoType (p :: * -> * -> *) = DarcsRepository !Pristine Cache deriving ( Show )

extractCache :: Repository p wR wU wT -> Cache
extractCache (Repo _ _ (DarcsRepository _ c)) = c

-- | 'modifyCache' @repository function@ modifies the cache of
--   @repository@ with @function@, remove duplicates and sort the results with 'compareByLocality'.
modifyCache :: forall p wR wU wT . (RepoPatch p)  => Repository p wR wU wT -> (Cache -> Cache) -> Repository p wR wU wT
modifyCache (Repo dir rf (DarcsRepository pristine cache)) f = Repo dir rf dr
  where dr            = DarcsRepository pristine . cmap ( sortBy compareByLocality . nub ) $ f cache
        cmap g (Ca c) = Ca (g c)
