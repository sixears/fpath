{-|
Module      : FPath.ToFile
Description : Convert fpath types to their directory equivalents
Copyright   : (c) Martyn J. Pearce, 2022
License     : GPL-3
Maintainer  : sample@email.com
Stability   : stable
Portability : POSIX
-}

module FPath.ToFile
  ( ToFile( toFile ), ToFileY( toFileY ) )
where

import Base1T

-- non-empty-containers ----------------

import qualified  NonEmptyContainers.SeqNE  as  SeqNE
import NonEmptyContainers.SeqConversions    ( ToSeq( toSeq ) )
import NonEmptyContainers.SeqNEConversions  ( FromSeqNonEmpty( fromSeqNE )
                                            , ToSeqNonEmpty( toSeqNE ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Abs         ( Abs( AbsD, AbsF ) )
import FPath.AbsFile     ( AbsFile )
import FPath.AbsDir      ( AbsDir, NonRootAbsDir )
import FPath.AsFilePath  ( AsFilePath )
import FPath.Dir         ( Dir( DirA, DirR ) )
import FPath.FileTypeC   ( FileType )
import FPath.File        ( File( FileA, FileR ) )
import FPath.FPath       ( FPath( FAbsD, FAbsF, FRelD, FRelF ) )
import FPath.Rel         ( Rel( RelD, RelF ) )
import FPath.RelDir      ( NonRootRelDir, RelDir )
import FPath.RelFile     ( RelFile )

--------------------------------------------------------------------------------

{-| Convert fpath types to their file equivalents.  Note that this is different
    from `FPath.AsFile`; that is just pulling out the `File` from things that
    really are files (e.g., FPath); whereas this is about /converting/ things;
    e.g., directory names, to files.
 -}
-- we don't really need the Printable, AsFilePath requirements here; rather,
-- they should be true of all fpathish things, and including them here makes
-- many function type signatures simpler
class (Printable ρ, AsFilePath ρ) ⇒ ToFile ρ where
  {-| Convert an fpath type to its file equivalent. -}
  toFile ∷ ρ → FileType ρ

instance ToFile AbsFile where
  toFile = id

instance ToFile NonRootAbsDir where
  toFile = fromSeqNE ∘ toSeqNE

instance ToFile RelFile where
  toFile = id

instance ToFile NonRootRelDir where
  toFile = fromSeqNE ∘ toSeqNE

instance ToFile File where
  toFile = id

------------------------------------------------------------

{-| Convert fpath types to their file equivalents. -}
class ToFileY ρ where
  {-| Convert an fpath type to its file equivalent.  Not every directory
      can be converted; specifically, the root directory (/) and the empty
      relative directory (./) cannot convert.
   -}
  toFileY ∷ ρ → 𝕄 (FileType ρ)

instance ToFileY AbsDir where
  toFileY = fromSeqNE ⩺ SeqNE.fromSeq ∘ toSeq

instance ToFileY AbsFile where
  toFileY = 𝓙

instance ToFileY RelDir where
  toFileY = fromSeqNE ⩺ SeqNE.fromSeq ∘ toSeq

instance ToFileY RelFile where
  toFileY = 𝓙

instance ToFileY Abs where
  toFileY (AbsD d) = toFileY d
  toFileY (AbsF f) = 𝓙 f

instance ToFileY Rel where
  toFileY (RelD d) = toFileY d
  toFileY (RelF f) = 𝓙 f

instance ToFileY Dir where
  toFileY (DirA f) = FileA ⊳ toFileY f
  toFileY (DirR f) = FileR ⊳ toFileY f

instance ToFileY File where
  toFileY = 𝓙

instance ToFileY FPath where
  toFileY (FAbsD d) = FileA ⊳ toFileY d
  toFileY (FRelD d) = FileR ⊳ toFileY d
  toFileY (FAbsF f) = 𝓙 $ FileA f
  toFileY (FRelF f) = 𝓙 $ FileR f

-- that's all, folks! ----------------------------------------------------------
