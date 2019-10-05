{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE QuasiQuotes      #-}
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

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism' )

-- more-unicode-symbols ----------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⩼), (##) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

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
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir, reldir )
import FPath.T.FPath.TestData  ( etc, pamd, r0, r1, r2, r3, root, wgm )

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

instance Printable Dir where
  print (DirA f) = print f
  print (DirR f) = print f

instance Textual Dir where
  textual = DirA ⊳ textual ∤ DirR ⊳ textual

----------------------------------------

instance AsFilePath Dir where
  filepath = prism' toString fromString

--------------------

filepathTests ∷ TestTree
filepathTests =
  let nothin' = Nothing ∷ Maybe Dir
      fail s  = testCase s $ nothin' ≟ s ⩼ filepath
   in testGroup "filepath"
            [ testCase "root"  $ "/"           ≟ DirA root    ## filepath
            , testCase "etc"   $ "/etc/"       ≟ DirA etc     ## filepath
            , testCase "pam.d" $ "/etc/pam.d/" ≟ DirA pamd    ## filepath
            , testCase "wgm"   $ "/w/g/M/"     ≟ DirA wgm     ## filepath

            , testCase "/etc/" $ Just etc      ≟ "/etc/" ⩼ filepath

            , testCase "r0" $ "./"     ≟ DirR r0 ## filepath
            , testCase "r1" $ "r/"     ≟ DirR r1 ## filepath
            , testCase "r2" $ "r/p/"   ≟ DirR r2 ## filepath
            , testCase "r3" $ "p/q/r/" ≟ DirR r3 ## filepath

            , testCase "r0" $ Just (DirR r0) ≟ "./"     ⩼ filepath
            , testCase "r1" $ Just (DirR r1) ≟ "r/"     ⩼ filepath
            , testCase "r2" $ Just (DirR r2) ≟ "r/p/"   ⩼ filepath
            , testCase "r3" $ Just (DirR r3) ≟ "p/q/r/" ⩼ filepath

            , fail "/etc"
            , fail "foo/etc"
            , fail "etc"
            , fail "/etc/pam.d"
            , fail "etc/pam.d"
            , fail "/etc//pam.d/"
            , fail "e/c"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"
            ]

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
tests = testGroup "FPath.Dir" [ filepathTests, parseDirTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
