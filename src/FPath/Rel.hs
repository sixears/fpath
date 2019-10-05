{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.Rel
  ( AsRel( _Rel ), Rel(..)

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

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism', prism' )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⫥), (⩼) )
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

import Data.Text  ( last, null )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir, reldir )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile
                               , relfile )
import FPath.T.FPath.TestData  ( r0, r1, r2, r3, rf1, rf2, rf3, rf4 )

--------------------------------------------------------------------------------

data Rel = RelD RelDir | RelF RelFile
  deriving (Eq, Show)

class AsRel α where
  _Rel ∷ Prism' α Rel

instance AsRel Rel where
  _Rel = id

----------------------------------------

instance AsRelDir Rel where
  _RelDir ∷ Prism' Rel RelDir
  _RelDir = prism' RelD (\ case (RelD d) → Just d; _ → Nothing)

instance AsRelFile Rel where
  _RelFile ∷ Prism' Rel RelFile
  _RelFile = prism' RelF (\ case (RelF f) → Just f; _ → Nothing)

----------------------------------------

instance Printable Rel where
  print (RelD f) = print f
  print (RelF f) = print f

instance Textual Rel where
  textual = try (RelD ⊳ textual) ∤ RelF ⊳ textual

----------------------------------------

instance AsFilePath Rel where
  filepath = prism' toString fromString

filepathTests ∷ TestTree
filepathTests =
  let nothin' = Nothing ∷ Maybe Rel
      fail s  = testCase s $ nothin' ≟ s ⩼ filepath
   in testGroup "filepath"
            [ testCase "r0" $ "./"     ≟ RelD r0 ⫥ filepath
            , testCase "r1" $ "r/"     ≟ RelD r1 ⫥ filepath
            , testCase "r2" $ "r/p/"   ≟ RelD r2 ⫥ filepath
            , testCase "r3" $ "p/q/r/" ≟ RelD r3 ⫥ filepath

            , testCase "r0" $ Just (RelD r0) ≟ "./"     ⩼ filepath
            , testCase "r1" $ Just (RelD r1) ≟ "r/"     ⩼ filepath
            , testCase "r2" $ Just (RelD r2) ≟ "r/p/"   ⩼ filepath
            , testCase "r3" $ Just (RelD r3) ≟ "p/q/r/" ⩼ filepath

            , testCase "rf1" $ "r.e"       ≟ RelF rf1 ⫥ filepath
            , testCase "rf2" $ "r/p.x"     ≟ RelF rf2 ⫥ filepath
            , testCase "rf3" $ "p/q/r.mp3" ≟ RelF rf3 ⫥ filepath
            , testCase "rf4" $ ".x"        ≟ RelF rf4 ⫥ filepath

            , testCase "rf1" $ Just (RelF rf1) ≟ "r.e"       ⩼ filepath
            , testCase "rf2" $ Just (RelF rf2) ≟ "r/p.x"     ⩼ filepath
            , testCase "rf3" $ Just (RelF rf3) ≟ "p/q/r.mp3" ⩼ filepath
            , testCase "rf4" $ Just (RelF rf4) ≟ ".x"        ⩼ filepath

            , fail "/etc"
            , fail "/etc/pam.d/"
            , fail "/etc//pam.d/"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"

            , fail "/etc/pam.d"
            ]

----------------------------------------

relpathT ∷ TypeRep
relpathT = typeRep (Proxy ∷ Proxy Rel)

instance Parseable Rel where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η Rel
  parse (toText → t) =
    case null t of
      True → __FPathEmptyE__ relpathT
      False → case last t of
                '/' → RelD ⊳ parse  t
                _   → RelF ⊳ parse t

parseRelTests ∷ TestTree
parseRelTests =
  let success d f t = testCase t $ Right (d ⫥ f) ≟ parse @Rel @FPathError t
   in testGroup "parseRel"
                [ success [reldir|./|]         _RelDir "./"
                , success [reldir|etc/|]       _RelDir "etc/"
                , success [relfile|etc/group|] _RelFile "etc/group"
                ]


--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Rel" [ filepathTests, parseRelTests ]
                

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
