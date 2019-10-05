{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.FPath
  ( FPath, tests )
where

-- base --------------------------------

import Data.Bool      ( Bool( False, True ) )
import Data.Either    ( Either( Right ) )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
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
import Data.MoreUnicode.Lens         ( (⩼), (⫥) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsers -----------------------------

import Text.Parser.Combinators  ( try )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( head, last, null )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Abs               ( AsAbs(_Abs ), Abs( AbsD, AbsF ) )
import FPath.AbsDir            ( AbsDir, AsAbsDir( _AbsDir)
                               , AsNonRootAbsDir( _NonRootAbsDir ), NonRootAbsDir
                               , ToAbsDir( toAbsDir )
                               , absdir
                               )
import FPath.AbsFile           ( AbsFile, AsAbsFile( _AbsFile )
                               , absfile )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.Rel               ( AsRel(_Rel ), Rel( RelD, RelF ) )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir, reldir )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile, relfile )
import FPath.T.FPath.TestData  ( af1, af2, af3, af4, etc, pamd
                               , r0, r1, r2, r3, rf1, rf2, rf3, rf4, root, wgm )

--------------------------------------------------------------------------------

data FPath = FAbsD AbsDir | FAbsF AbsFile | FRelD RelDir | FRelF RelFile
  deriving (Eq, Show)

----------------------------------------

instance AsAbsDir FPath where
  _AbsDir ∷ Prism' FPath AbsDir
  _AbsDir = prism' FAbsD (\ case (FAbsD d) → Just d; _ → Nothing)

----------------------------------------

instance AsAbsFile FPath where
  _AbsFile ∷ Prism' FPath AbsFile
  _AbsFile = prism' FAbsF (\ case (FAbsF f) → Just f; _ → Nothing)

----------------------------------------

instance AsNonRootAbsDir FPath where
  _NonRootAbsDir ∷ Prism' FPath NonRootAbsDir
  _NonRootAbsDir = prism' (FAbsD ∘ toAbsDir)
                          (\ case (FAbsD d) → d ⩼ _NonRootAbsDir; _ → Nothing)

----------------------------------------

instance AsRelDir FPath where
  _RelDir ∷ Prism' FPath RelDir
  _RelDir = prism' FRelD (\ case (FRelD d) → Just d; _ → Nothing)

----------------------------------------

instance AsRelFile FPath where
  _RelFile ∷ Prism' FPath RelFile
  _RelFile = prism' FRelF (\ case (FRelF f) → Just f; _ → Nothing)

----------------------------------------

instance AsAbs FPath where
  _Abs = prism' (\ p → case p of AbsD d → FAbsD d
                                 AbsF f → FAbsF f
                )
                (\ p → case p of FAbsD d → Just $ AbsD d
                                 FAbsF f → Just $ AbsF f
                                 _       → Nothing
                )


----------------------------------------

instance AsRel FPath where
  _Rel = prism' (\ p → case p of RelD d → FRelD d
                                 RelF f → FRelF f
                )
                (\ p → case p of FRelD d → Just $ RelD d
                                 FRelF f → Just $ RelF f
                                 _       → Nothing
                )

----------------------------------------

instance Printable FPath where
  print (FAbsD f) = print f
  print (FAbsF f) = print f
  print (FRelD f) = print f
  print (FRelF f) = print f

instance Textual FPath where
  textual = try (FAbsD ⊳ textual) ∤ try (FAbsF ⊳ textual)
          ∤ try (FRelD ⊳ textual) ∤ FRelF ⊳ textual

----------------------------------------

instance AsFilePath FPath where
  filepath = prism' toString fromString

filepathTests ∷ TestTree
filepathTests =
  let nothin' = Nothing ∷ Maybe FPath
      fail s  = testCase s $ nothin' ≟ s ⩼ filepath
   in testGroup "filepath"
            [ testCase "r0" $ "./"     ≟ FRelD r0 ⫥ filepath
            , testCase "r1" $ "r/"     ≟ FRelD r1 ⫥ filepath
            , testCase "r2" $ "r/p/"   ≟ FRelD r2 ⫥ filepath
            , testCase "r3" $ "p/q/r/" ≟ FRelD r3 ⫥ filepath

            , testCase "r0" $ Just (FRelD r0) ≟ "./"     ⩼ filepath
            , testCase "r1" $ Just (FRelD r1) ≟ "r/"     ⩼ filepath
            , testCase "r2" $ Just (FRelD r2) ≟ "r/p/"   ⩼ filepath
            , testCase "r3" $ Just (FRelD r3) ≟ "p/q/r/" ⩼ filepath

            , testCase "rf1" $ "r.e"       ≟ FRelF rf1 ⫥ filepath
            , testCase "rf2" $ "r/p.x"     ≟ FRelF rf2 ⫥ filepath
            , testCase "rf3" $ "p/q/r.mp3" ≟ FRelF rf3 ⫥ filepath
            , testCase "rf4" $ ".x"        ≟ FRelF rf4 ⫥ filepath

            , testCase "rf1" $ Just (FRelF rf1) ≟ "r.e"       ⩼ filepath
            , testCase "rf2" $ Just (FRelF rf2) ≟ "r/p.x"     ⩼ filepath
            , testCase "rf3" $ Just (FRelF rf3) ≟ "p/q/r.mp3" ⩼ filepath
            , testCase "rf4" $ Just (FRelF rf4) ≟ ".x"        ⩼ filepath

            , testCase "root"  $ "/"             ≟ FAbsD root    ⫥ filepath
            , testCase "etc"   $ "/etc/"         ≟ FAbsD etc     ⫥ filepath
            , testCase "pam.d" $ "/etc/pam.d/"   ≟ FAbsD pamd    ⫥ filepath
            , testCase "wgm"   $ "/w/g/M/"       ≟ FAbsD wgm     ⫥ filepath
            , testCase "/etc/" $ Just (FAbsD etc) ≟ "/etc/" ⩼ filepath

            , testCase "af1" $ "/r.e"       ≟ FAbsF af1 ⫥ filepath
            , testCase "af2" $ "/r/p.x"     ≟ FAbsF af2 ⫥ filepath
            , testCase "af3" $ "/p/q/r.mp3" ≟ FAbsF af3 ⫥ filepath
            , testCase "af4" $ "/.x"        ≟ FAbsF af4 ⫥ filepath

            , testCase "af1" $ Just (FAbsF af1) ≟ "/r.e"       ⩼ filepath
            , testCase "af2" $ Just (FAbsF af2) ≟ "/r/p.x"     ⩼ filepath
            , testCase "af3" $ Just (FAbsF af3) ≟ "/p/q/r.mp3" ⩼ filepath
            , testCase "af4" $ Just (FAbsF af4) ≟ "/.x"        ⩼ filepath

            , fail "/etc//pam.d/"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"
            ]

------------------------------------------------------------

fpathT ∷ TypeRep
fpathT = typeRep (Proxy ∷ Proxy FPath)

instance Parseable FPath where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η FPath
  parse (toText → t) =
    case null t of
      True → __FPathEmptyE__ fpathT
      False → case (head t, last t) of
                ('/','/') → FAbsD ⊳ parse t
                ('/',_  ) → FAbsF ⊳ parse t
                (_  ,'/') → FRelD ⊳ parse t
                (_  ,_  ) → FRelF ⊳ parse t

parseFPathTests ∷ TestTree
parseFPathTests =
  let success d f t =
        testCase t $ Right (d ⫥ f) ≟ parse @FPath @FPathError t
   in testGroup "parseFPath"
                [ success [absdir|/|]      _AbsDir  "/"
                , success [absdir|/etc/|]  _AbsDir  "/etc/"
                , success [absfile|/quux|] _AbsFile "/quux"
                , success [reldir|foo/|]   _RelDir  "foo/"
                , success [relfile|bar|]   _RelFile "bar"
                ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "FPath" [ filepathTests, parseFPathTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
