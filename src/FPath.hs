{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE ViewPatterns     #-}

-- {-# LANGUAGE MultiParamTypeClasses #-}

-- TODO

-- separate out temp handlers -- using on fpath!
-- add (bash)completion for files+dirs
-- move tests into .Internals, so they're not publicly exposed; see
--   https://stackoverflow.com/questions/9190638/how-why-and-when-to-use-the-internal-modules-pattern
--   https://stackoverflow.com/questions/16887023/is-it-possible-to-hide-specific-functions-from-appearing-in-the-documentation-us

------------------------------------------------------------

-- parseAsAbsDir - parse a path as an AbsDir; if it's not relative, parse it relative to here; if it lacks a trailing '/', add one.
-- fns for Optparse

-- the only tests that each module should export are 'tests' (with a no-doc flag on them)
-- add toEither for {Abs,Rel}(Path?) {Dir,File}

-- add AsAbs(Path), AsRel(Path); to allow for lenses on FPath; also AsDir & AsRel
-- add toDir, toFile to convert, e.g., "files" returned by readlink to dirs

-- unify implementations to one ("Internals") impl., with newtype encapsulations
--    for each of {Abs,Rel}{Dir,File,Path}; Internals would be something like
--    `Internals Seq|SeqNE FileComponent|None Basis=/|./ AllowPathBits(.|..)`
--    NOTE: ABSDIR IS JUST A GROUNDED RELDIR; that is, AbsDir is just a RelDir,
--    grounded at /

-- try using type-level depth, to allow for non-maybe Parent whenever depth>1.

-- implement Paths

module FPath
  ( AbsDir, AbsFile, NonRootAbsDir, RelDir, RelFile
  , Abs, Rel, Dir, File
  , FPath

  , AsFilePath( filepath )

  , AppendableFPath( (⫻) ), (</>)
  , Parseable( parse, parse', __parse__, __parse'__ )

  -- file functions
  , (⊙), addExt, dir, dirfile, file, ext, splitExt

  -- quasi-quoters
  , absdir, absfile, absdirN, reldir, relfile

  , nonRootAbsDir

  , root
  , stripDir, stripDir'

  , getCwd, getCwd'

  , tests
  )
where

-- base --------------------------------

import Control.Monad    ( return )
import Data.Maybe       ( maybe )
import Data.String      ( String )
import System.Exit      ( ExitCode )
import System.IO        ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( toText )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- more-unicode-symbols ----------------

import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import qualified  NonEmptyContainers.SeqNE           as  SeqNE
import qualified  NonEmptyContainers.SeqConversions  as  SeqConversions

import NonEmptyContainers.SeqConversions   ( FromSeq( fromSeq )
                                           , IsSeq( seq ) )
import NonEmptyContainers.SeqNEConversions ( IsSeqNonEmpty( seqNE )
                                           , fromSeqNE )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Abs               ( Abs )
import FPath.AbsDir            ( AbsDir, NonRootAbsDir

                               , absdir, absdirN, absdirT, nonRootAbsDir, root
                               )
import FPath.AbsFile           ( AbsFile
                               , absfile, absfileT
                               )
import FPath.AppendableFPath   ( AppendableFPath( (⫻) ), (</>) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Dir               ( Dir )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathError  ( AsFPathNotAPrefixError, FPathNotAPrefixError
                               , __FPathNotAPrefixError__ )
import FPath.File              ( File )
import FPath.FileLike          ( FileLike( (⊙), addExt, dir, dirfile, file, ext
                                         , splitExt ) )
import FPath.FPath             ( FPath )
import FPath.IO                ( getCwd, getCwd' )
import FPath.Parseable         ( Parseable( parse, parse'
                                          , __parse__, __parse'__ ) )
import FPath.Rel               ( Rel )
import FPath.RelDir            ( RelDir, reldir, reldirT )
import FPath.RelFile           ( RelFile, relfile, relfileT )
import FPath.RelType           ( RelTypeC( RelType ) )

-------------------------------------------------------------------------------

------------------------------------------------------------

{- | type-level conversion to absolute version of a type; no-op on
     already-absolute types -}
class HasAbsify α where
  type Absify α

instance HasAbsify RelFile where
  type Absify RelFile = AbsFile

instance HasAbsify RelDir where
  type Absify RelDir = AbsDir

instance HasAbsify AbsFile where
  type Absify AbsFile = AbsFile

instance HasAbsify AbsDir where
  type Absify AbsDir = AbsDir

------------------------------------------------------------

class (DirTypeC π, RelTypeC π) ⇒ Strippable π where
  {- | "unresolve" a path; that is, if an absolute directory is a prefix of an
       absolute (file|directory), then strip off that prefix to leave a relative
       (file|directory).  Note that a file will always keep its filename; but a
       directory might result in a relative dir of './'.
   -}
  stripDir ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
             DirType π → π → η (RelType π)

  stripDir' ∷ MonadError FPathNotAPrefixError η ⇒ DirType π → π → η (RelType π)
  stripDir' = stripDir

instance Strippable AbsFile where
  stripDir ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
             AbsDir → AbsFile → η RelFile
  stripDir d'@(view seq → d) f'@(view seqNE → f) =
    maybe (__FPathNotAPrefixError__ absfileT (toText d') (toText f'))
          (return ∘ fromSeqNE)
          (SeqNE.stripProperPrefix d f)

instance Strippable AbsDir where
  stripDir ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                AbsDir → AbsDir → η RelDir
  stripDir d'@(view seq → d) f'@(view seq → f) =
    maybe (__FPathNotAPrefixError__ absdirT (toText d') (toText f'))
          (return ∘ fromSeq)
          (SeqConversions.stripPrefix d f)

instance Strippable RelFile where
  stripDir ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                RelDir → RelFile → η RelFile
  stripDir d'@(view seq → d) f'@(view seqNE → f) =
    maybe (__FPathNotAPrefixError__ relfileT (toText d') (toText f'))
          (return ∘ fromSeqNE)
          (SeqNE.stripProperPrefix d f)

instance Strippable RelDir where
  stripDir ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                RelDir → RelDir → η RelDir
  stripDir d'@(view seq → d) f'@(view seq → f) =
    maybe (__FPathNotAPrefixError__ reldirT (toText d') (toText f'))
          (return ∘ fromSeq)
          (SeqConversions.stripPrefix d f)

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "FPath" [ ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
