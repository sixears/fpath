{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.Abs
  ( Abs(..), AsAbs( _Abs ), absT

  , tests
  )
where

-- base --------------------------------

import Data.Bool      ( Bool( False, True ) )
import Data.Either    ( Either( Right ) )
import Data.Eq        ( Eq )
import Data.Function  ( ($), id )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import Data.Typeable  ( Proxy( Proxy ), TypeRep, typeRep )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism', prism' )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⩼), (##) )
import Data.MoreUnicode.Natural      ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsers -----------------------------

import Text.Parser.Combinators  ( try )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( last, null )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir            ( AbsDir, AsAbsDir( _AbsDir)
                               , AsNonRootAbsDir( _NonRootAbsDir )
                               , NonRootAbsDir
                               , absdir, toAbsDir
                               )
import FPath.AbsFile           ( AbsFile, AsAbsFile( _AbsFile ), absfile )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.T.FPath.TestData  ( etc, pamd, af1, af2, af3, af4, root, wgm )

--------------------------------------------------------------------------------

data Abs = AbsD AbsDir | AbsF AbsFile
  deriving (Eq, Show)

class AsAbs α where
  _Abs ∷ Prism' α Abs

instance AsAbs Abs where
  _Abs = id

----------------------------------------

instance AsAbsDir Abs where
  _AbsDir ∷ Prism' Abs AbsDir
  _AbsDir = prism' AbsD (\ case (AbsD d) → Just d; _ → Nothing)

instance AsNonRootAbsDir Abs where
  _NonRootAbsDir ∷ Prism' Abs NonRootAbsDir
  _NonRootAbsDir = prism' (AbsD ∘ toAbsDir)
                          (\ case (AbsD d) → d ⩼ _NonRootAbsDir; _ → Nothing)

instance AsAbsFile Abs where
  _AbsFile ∷ Prism' Abs AbsFile
  _AbsFile = prism' AbsF (\ case (AbsF f) → Just f; _ → Nothing)

----------------------------------------

instance Printable Abs where
  print (AbsD f) = print f
  print (AbsF f) = print f

instance Textual Abs where
  textual = try (AbsD ⊳ textual) ∤ AbsF ⊳ textual

----------------------------------------

instance AsFilePath Abs where
  filepath = prism' toString fromString

filepathTests ∷ TestTree
filepathTests =
  let nothin' = Nothing ∷ Maybe Abs
      fail s  = testCase s $ nothin' @=? s ⩼ filepath
   in testGroup "filepath"
            [ testCase "root"  $ "/"             ≟ AbsD root    ## filepath
            , testCase "etc"   $ "/etc/"         ≟ AbsD etc     ## filepath
            , testCase "pam.d" $ "/etc/pam.d/"   ≟ AbsD pamd    ## filepath
            , testCase "wgm"   $ "/w/g/M/"       ≟ AbsD wgm     ## filepath
            , testCase "/etc/" $ Just (AbsD etc) @=? "/etc/" ⩼ filepath

            , testCase "af1" $ "/r.e"       ≟ AbsF af1 ## filepath
            , testCase "af2" $ "/r/p.x"     ≟ AbsF af2 ## filepath
            , testCase "af3" $ "/p/q/r.mp3" ≟ AbsF af3 ## filepath
            , testCase "af4" $ "/.x"        ≟ AbsF af4 ## filepath

            , testCase "af1" $ Just (AbsF af1) @=? "/r.e"       ⩼ filepath
            , testCase "af2" $ Just (AbsF af2) @=? "/r/p.x"     ⩼ filepath
            , testCase "af3" $ Just (AbsF af3) @=? "/p/q/r.mp3" ⩼ filepath
            , testCase "af4" $ Just (AbsF af4) @=? "/.x"        ⩼ filepath

            , fail "etc"
            , fail "etc/pam.d/"
            , fail "etc//pam.d/"
            , fail "/\0etc"
            , fail "/etc\0"
            , fail "/e\0c"

            , fail "etc/"
            , fail "etc/pam.d"
            , fail "e/c"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"
            ]

------------------------------------------------------------

absT ∷ TypeRep
absT = typeRep (Proxy ∷ Proxy Abs)

instance Parseable Abs where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η Abs
  parse (toText → t) =
    case null t of
      True → __FPathEmptyE__ absT
      False → case last t of
                '/' → AbsD ⊳ parse t
                _   → AbsF ⊳ parse t

parseAbsTests ∷ TestTree
parseAbsTests =
  let success d f t = testCase t $ Right (d ## f) @=? parse @Abs @FPathError t
   in testGroup "parseAbs"
                [ success [absdir|/|]           _AbsDir "/"
                , success [absdir|/etc/|]       _AbsDir "/etc/"
                , success [absfile|/etc/group|] _AbsFile "/etc/group"
                ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Abs" [ filepathTests, parseAbsTests ]
                
----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
