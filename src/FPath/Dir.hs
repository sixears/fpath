{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE ViewPatterns     #-}
module FPath.Dir
  ( Dir(..)

  , tests
  )
where

-- base --------------------------------

import Data.Bool        ( Bool( False, True ) )
import Data.Either      ( Either( Right ) )
import Data.Eq          ( Eq )
import Data.Function    ( ($) )
import Data.Maybe       ( Maybe( Just, Nothing ) )
import Data.String      ( String )
import Data.Typeable    ( Proxy( Proxy ), TypeRep, typeRep )
import System.Exit      ( ExitCode )
import System.IO        ( IO )
import Text.Show        ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism' )

-- more-unicode-symbols ----------------

import Data.MoreUnicode.Functor    ( (⊳) )
import Data.MoreUnicode.Lens       ( (⩼), (##) )
import Data.MoreUnicode.Natural    ( ℕ )
import Data.MoreUnicode.Tasty      ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( head, null )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir            ( AbsDir, AsAbsDir( _AbsDir)
                               , AsNonRootAbsDir( _NonRootAbsDir ), NonRootAbsDir
                               , ToAbsDir( toAbsDir )

                               , absdir
                               )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir, reldir )

-------------------------------------------------------------------------------

data Dir = DirA AbsDir | DirR RelDir
  deriving (Eq, Show)

instance AsAbsDir Dir where
  _AbsDir ∷ Prism' Dir AbsDir
  _AbsDir = prism' DirA (\ case (DirA d) → Just d; _ → Nothing)

instance AsNonRootAbsDir Dir where
  _NonRootAbsDir ∷ Prism' Dir NonRootAbsDir
  _NonRootAbsDir = prism' (DirA ∘ toAbsDir)
                          (\ case (DirA d) → d ⩼ _NonRootAbsDir; _ → Nothing)

instance AsRelDir Dir where
  _RelDir ∷ Prism' Dir RelDir
  _RelDir = prism' DirR (\ case (DirR d) → Just d; _ → Nothing)

----------------------------------------

dirT ∷ TypeRep
dirT = typeRep (Proxy ∷ Proxy Dir)

instance Parseable Dir where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η Dir
  parse (toText → t) =
    case null t of
      True → __FPathEmptyE__ dirT
      False → case head t of
                '/' → DirA ⊳ parse t
                _   → DirR ⊳ parse t

parseDirTests ∷ TestTree
parseDirTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parse @Dir @FPathError t
   in testGroup "parseDir"
                [ success [absdir|/|]     _AbsDir "/"
                , success [absdir|/etc/|] _AbsDir "/etc/"
                , success [reldir|./|]    _RelDir "./"
                , success [reldir|etc/|]  _RelDir "etc/"
                ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "FPath.Dir" [ parseDirTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
