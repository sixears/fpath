{-|
Module      : FPath.ToDir
Description : Convert fpath types to their directory equivalents
Copyright   : (c) Martyn J. Pearce, 2022
License     : GPL-3
Maintainer  : sample@email.com
Stability   : stable
Portability : POSIX
-}

module FPath.ToDir
  ( ToDir( toDir ) )
where

import Base1T

-- non-empty-containers ----------------

import NonEmptyContainers.SeqConversions  ( FromSeq( fromSeq ), ToSeq( toSeq ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Abs         ( Abs( AbsD, AbsF ) )
import FPath.AbsFile     ( AbsFile )
import FPath.AbsDir      ( AbsDir )
import FPath.AsFilePath  ( AsFilePath )
import FPath.Dir         ( Dir( DirA, DirR ) )
import FPath.DirType     ( DirType )
import FPath.File        ( File( FileA, FileR ) )
import FPath.FPath       ( FPath( FAbsD, FAbsF, FRelD, FRelF ) )
import FPath.Rel         ( Rel( RelD, RelF ) )
import FPath.RelDir      ( RelDir )
import FPath.RelFile     ( RelFile )

--------------------------------------------------------------------------------

{-| Convert fpath types to their directory equivalents.  Note that this is
    different from `FPath.AsDir`; that is just pulling out the `Dir` from
    things that really are files (e.g., FPath); whereas this is about
    /converting/ things; e.g., file names, to directories.
 -}
-- we don't really need the Printable, AsFilePath requirements here; rather,
-- they should be true of all fpathish things, and including them here makes
-- many function type signatures simpler
{-| Convert fpath types to their directory equivalents. -}
class (Printable ρ, AsFilePath ρ) ⇒ ToDir ρ where
  {-| Convert an fpath type to its directory equivalent. -}
  toDir ∷ ρ → DirType ρ

instance ToDir AbsFile where
  toDir = fromSeq ∘ toSeq

instance ToDir AbsDir where
  toDir = id

instance ToDir RelFile where
  toDir = fromSeq ∘ toSeq

instance ToDir RelDir where
  toDir = id

instance ToDir Abs where
  toDir (AbsF f) = toDir f
  toDir (AbsD d) = d

instance ToDir Rel where
  toDir (RelF f) = toDir f
  toDir (RelD d) = d

instance ToDir File where
  toDir (FileA f) = DirA $ toDir f
  toDir (FileR f) = DirR $ toDir f

instance ToDir Dir where
  toDir = id

instance ToDir FPath where
  toDir (FAbsD d) = DirA d
  toDir (FRelD d) = DirR d
  toDir (FAbsF f) = DirA $ toDir f
  toDir (FRelF f) = DirR $ toDir f

-- that's all, folks! ----------------------------------------------------------
