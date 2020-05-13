{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath.AbsDir
  ( tests )
where

-- base --------------------------------

import Data.Function  ( ($) )
import Data.String    ( String )
import System.Exit    ( ExitCode )
import System.IO      ( IO )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )

-- tasty -------------------------------

import Test.Tasty           ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  FPath.AbsDir

import FPath.AbsDir            ( absdir )
import FPath.T.FPath.TestData  ( etc, pamd, root, wgm )

--------------------------------------------------------------------------------

absdirQQTests ∷ TestTree
absdirQQTests =
  testGroup "absdir (quasi-quoting)"
            [ testCase "root" $ root ≟ [absdir|/|]
            , testCase "/etc" $ etc ≟ [absdir|/etc/|]
            , testCase "/etc/pam.d" $ pamd ≟ [absdir|/etc/pam.d/|]
            , testCase "/w/g/M" $ wgm ≟ [absdir|/w/g/M/|]
            ]
----------------------------------------

tests ∷ TestTree
tests =
  testGroup "AbsDir" [ absdirQQTests, FPath.AbsDir.tests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
