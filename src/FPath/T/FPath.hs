{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-| more unit tests for `FPath.FPath` -}
module FPath.T.FPath
  ( tests )
where

-- base --------------------------------

import Data.Either    ( Either( Left, Right ) )
import Data.Function  ( ($) )
import Data.String    ( String )
import System.Exit    ( ExitCode )
import System.IO      ( IO )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  FPath
import qualified  FPath.Abs
import qualified  FPath.Dir
import qualified  FPath.FPath
import qualified  FPath.Rel
import qualified  FPath.T.FPath.AbsDir
import qualified  FPath.T.FPath.AbsFile
import qualified  FPath.T.FPath.NonRootAbsDir
import qualified  FPath.T.FPath.PathComponent
import qualified  FPath.T.FPath.RelDir
import qualified  FPath.T.FPath.RelFile

import FPath           ( Strippable, (⫻), stripDir )
import FPath.AbsDir    ( AbsDir, absdir, absdirT, root )
import FPath.AbsFile   ( AbsFile, absfile, absfileT )
import FPath.DirType   ( DirTypeC( DirType ) )
import FPath.RelDir    ( RelDir, reldir, reldirT )
import FPath.RelFile   ( RelFile, relfile, relfileT )
import FPath.RelType   ( RelTypeC( RelType ) )

import FPath.Error.FPathError  ( FPathNotAPrefixError, fPathNotAPrefixError )

--------------------------------------------------------------------------------

catenationTests ∷ TestTree
catenationTests =
  testGroup "catenation"
            [ testGroup "absfile"
                [ testCase "/etc/bar" $
                    [absfile|/etc/bar|] ≟ [absdir|/etc/|] ⫻ [relfile|bar|]
                , testCase "/bar" $
                    [absfile|/bar|]     ≟ [absdir|/|] ⫻ [relfile|bar|]
                , testCase "/etc/udev/bar/baz" $
                      [absfile|/etc/udev/bar/baz|]
                    ≟ [absdir|/etc/udev/|] ⫻ [relfile|bar/baz|]
                ]
            , testGroup "absdir"
                [ testCase "/etc/bar/" $
                    [absdir|/etc/bar/|] ≟ [absdir|/etc/|] ⫻ [reldir|bar/|]
                , testCase "/bar/" $
                    [absdir|/bar/|]     ≟ [absdir|/|] ⫻ [reldir|bar/|]
                , testCase "/etc/udev/bar/baz/" $
                      [absdir|/etc/udev/bar/baz/|]
                    ≟ [absdir|/etc/udev/|] ⫻ [reldir|bar/baz/|]
                ]
            , testGroup "reldir"
                [ testCase "etc/bar/" $
                    [reldir|etc/bar/|] ≟ [reldir|etc/|] ⫻ [reldir|bar/|]
                , testCase "bar/" $
                    [reldir|bar/|]     ≟ [reldir|./|] ⫻ [reldir|bar/|]
                , testCase "etc/udev/bar/baz/" $
                      [reldir|etc/udev/bar/baz/|]
                    ≟ [reldir|etc/udev/|] ⫻ [reldir|bar/baz/|]
                ]
            , testGroup "relfile"
                [ testCase "etc/bar" $
                    [relfile|etc/bar|] ≟ [reldir|etc/|] ⫻ [relfile|bar|]
                , testCase "bar" $
                    [relfile|bar|]     ≟ [reldir|./|] ⫻ [relfile|bar|]
                , testCase "etc/udev/bar/baz" $
                      [relfile|etc/udev/bar/baz|]
                    ≟ [reldir|etc/udev/|] ⫻ [relfile|bar/baz|]
                ]
            ]

----------------------------------------

stripDirAbsFileTests ∷ TestTree
stripDirAbsFileTests =
  let
    stripDir' ∷ ∀ π η . (MonadError FPathNotAPrefixError η, Strippable π) ⇒
                DirType π → π → η (RelType π)
    stripDir' = stripDir
  in
    testGroup "absfile"
      [ testCase "/"   $ Right [relfile|foo|] @=? stripDir' root [absfile|/foo|]
      , testCase "pfx" $
          Right [relfile|bar|] @=? stripDir' [absdir|/etc/|] [absfile|/etc/bar|]
      , testCase "no pfx" $
              Left (fPathNotAPrefixError absfileT "/dev/" "/etc/bar")
          @=? stripDir' @AbsFile [absdir|/dev/|] [absfile|/etc/bar|]
      , testCase "equal" $
              Left (fPathNotAPrefixError absfileT "/etc/bar/" "/etc/bar")
          @=? stripDir' @AbsFile [absdir|/etc/bar/|] [absfile|/etc/bar|]
      , testCase "longer 1" $
              Left (fPathNotAPrefixError absfileT "/etc/bar/" "/etc"
                                         )
          @=? stripDir' @AbsFile [absdir|/etc/bar/|] [absfile|/etc|]
      , testCase "longer 2" $
              Left (fPathNotAPrefixError absfileT "/etc/udev/bar/" "/etc/udev")
          @=? stripDir' @AbsFile [absdir|/etc/udev/bar/|] [absfile|/etc/udev|]
      ]

stripDirAbsDirTests ∷ TestTree
stripDirAbsDirTests =
  let
    stripDir' ∷ ∀ π η . (MonadError FPathNotAPrefixError η, Strippable π) ⇒
                DirType π → π → η (RelType π)
    stripDir' = stripDir
  in
    testGroup "absfile"
     [ testCase "/"   $ Right [reldir|foo/|] @=? stripDir' root [absdir|/foo/|]
     , testCase "pfx" $
         Right [reldir|bar/|] @=? stripDir' [absdir|/etc/|] [absdir|/etc/bar/|]
     , testCase "no pfx" $
             Left (fPathNotAPrefixError absdirT "/dev/" "/etc/bar/")
         @=? stripDir' @AbsDir [absdir|/dev/|] [absdir|/etc/bar/|]
     , testCase "equal" $
             Right [reldir|./|]
         @=? stripDir' @AbsDir [absdir|/etc/bar/|] [absdir|/etc/bar/|]
     , testCase "longer 1" $
             Left (fPathNotAPrefixError absdirT "/etc/bar/" "/etc/")
         @=? stripDir' @AbsDir [absdir|/etc/bar/|] [absdir|/etc/|]
     , testCase "longer 2" $
             Left (fPathNotAPrefixError absdirT "/etc/udev/bar/" "/etc/udev/")
         @=? stripDir' @AbsDir [absdir|/etc/udev/bar/|] [absdir|/etc/udev/|]
     ]

stripDirRelFileTests ∷ TestTree
stripDirRelFileTests =
  let
    stripDir' ∷ ∀ π η . (MonadError FPathNotAPrefixError η, Strippable π) ⇒
                DirType π → π → η (RelType π)
    stripDir' = stripDir
  in
   testGroup "relfile"
    [ testCase "/"   $
        Right [relfile|foo|] @=? stripDir' [reldir|./|] [relfile|foo|]
    , testCase "pfx" $
        Right [relfile|bar|] @=? stripDir' [reldir|etc/|] [relfile|etc/bar|]
    , testCase "no pfx" $
            Left (fPathNotAPrefixError relfileT "dev/" "etc/bar")
        @=? stripDir' @RelFile [reldir|dev/|] [relfile|etc/bar|]
    , testCase "equal" $
            Left (fPathNotAPrefixError relfileT "etc/bar/" "etc/bar")
        @=? stripDir' @RelFile [reldir|etc/bar/|] [relfile|etc/bar|]
    , testCase "longer 1" $
            Left (fPathNotAPrefixError relfileT "etc/bar/" "etc")
        @=? stripDir' @RelFile [reldir|etc/bar/|] [relfile|etc|]
    , testCase "longer 2" $
            Left (fPathNotAPrefixError relfileT "etc/udev/bar/" "etc/udev")
        @=? stripDir' @RelFile [reldir|etc/udev/bar/|] [relfile|etc/udev|]
    ]

stripDirRelDirTests ∷ TestTree
stripDirRelDirTests =
  let
    stripDir' ∷ ∀ π η . (Strippable π, MonadError FPathNotAPrefixError η) ⇒
                DirType π → π → η (RelType π)
    stripDir' = stripDir
  in
   testGroup "reldir"
    [ testCase "/"   $
        Right [reldir|foo/|] @=? stripDir' [reldir|./|] [reldir|foo/|]
    , testCase "pfx" $
        Right [reldir|bar/|] @=? stripDir' [reldir|etc/|] [reldir|etc/bar/|]
    , testCase "no pfx" $
            Left (fPathNotAPrefixError reldirT "dev/" "etc/bar/")
        @=? stripDir' @RelDir [reldir|dev/|] [reldir|etc/bar/|]
    , testCase "equal" $
            Right [reldir|./|]
        @=? stripDir' [reldir|etc/bar/|] [reldir|etc/bar/|]
    , testCase "longer 1" $
            Left (fPathNotAPrefixError reldirT "etc/bar/" "etc/")
        @=? stripDir' @RelDir [reldir|etc/bar/|] [reldir|etc/|]
    , testCase "longer 2" $
            Left (fPathNotAPrefixError reldirT "etc/udev/bar/" "etc/udev/")
        @=? stripDir' @RelDir [reldir|etc/udev/bar/|] [reldir|etc/udev/|]
    ]

stripDirTests ∷ TestTree
stripDirTests =
  testGroup "stripDir" [ stripDirAbsFileTests, stripDirAbsDirTests
                       , stripDirRelFileTests, stripDirRelDirTests ]

----------------------------------------

----------------------------------------

fpathTests ∷ TestTree
fpathTests = testGroup "FPath" [ catenationTests
                               , stripDirTests
                               , FPath.tests
                               , FPath.Abs.tests
                               , FPath.Dir.tests
                               , FPath.FPath.tests
                               , FPath.Rel.tests
                               ]

{-| unit tests for all of FPath -}
tests ∷ TestTree
tests = testGroup "fpath" [ FPath.T.FPath.PathComponent.tests
                          , FPath.T.FPath.AbsDir.tests
                          , FPath.T.FPath.NonRootAbsDir.tests
                          , FPath.T.FPath.RelDir.tests
                          , FPath.T.FPath.RelFile.tests
                          , FPath.T.FPath.AbsFile.tests
                          , fpathTests
                          ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
