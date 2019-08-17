{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath
  ( tests )
where

-- base --------------------------------

import Data.Either      ( Either( Left, Right ) )
import Data.Function    ( ($) )
import Data.String      ( String )
import Numeric.Natural  ( Natural )
import System.IO        ( IO )

-- more-unicode ------------------------

import Data.MoreUnicode.Tasty  ( (≟) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  FPath.T.FPath.AbsDir
import qualified  FPath.T.FPath.AbsFile
import qualified  FPath.T.FPath.NonRootAbsDir
import qualified  FPath.T.FPath.PathComponent
import qualified  FPath.T.FPath.RelDir
import qualified  FPath.T.FPath.RelFile

import FPath           ( resolve, stripPrefix', toDir )
import FPath.AbsDir    ( AbsDir, absdir, absdirT, root )
import FPath.AbsFile   ( AbsFile, absfile, absfileT )
import FPath.RelDir    ( reldir )
import FPath.RelFile   ( relfile )
import FPath.T.Common  ( doTest, doTestR, doTestS )

import FPath.Error.FPathError  ( FPathNotAPrefixError( FPathNotAPrefixError ) )

--------------------------------------------------------------------------------

toDirTests ∷ TestTree
toDirTests =
  testGroup "toDir"
            [ testCase "absfile" $
                [absdir|/foo/bar/|] ≟ toDir [absfile|/foo/bar|]
            , testCase "relfile" $
                [reldir|foo/bar/|]  ≟ toDir [relfile|foo/bar|]
            ]

----------------------------------------

resolveTests ∷ TestTree
resolveTests =
  testGroup "resolve"
            [ testCase "/etc/bar" $
                [absfile|/etc/bar|] ≟ resolve [absdir|/etc/|] [relfile|bar|]
            , testCase "/bar" $
                [absfile|/bar|]     ≟ resolve [absdir|/|] [relfile|bar|]
            , testCase "/etc/udev/bar/baz" $
                  [absfile|/etc/udev/bar/baz|]
                ≟ resolve [absdir|/etc/udev/|] [relfile|bar/baz|]
            , testCase "/etc/bar" $
                [absdir|/etc/bar/|] ≟ resolve [absdir|/etc/|] [reldir|bar/|]
            , testCase "/bar" $
                [absdir|/bar/|]     ≟ resolve [absdir|/|] [reldir|bar/|]
            , testCase "/etc/udev/bar/baz" $
                  [absdir|/etc/udev/bar/baz/|]
                ≟ resolve [absdir|/etc/udev/|] [reldir|bar/baz/|]
            ]

----------------------------------------

stripProperPrefixAbsFileTests ∷ TestTree
stripProperPrefixAbsFileTests =
  testGroup "absfile"
   [ testCase "/"   $ Right [relfile|foo|] ≟ stripPrefix' root [absfile|/foo|]
   , testCase "pfx" $
       Right [relfile|bar|] ≟ stripPrefix' [absdir|/etc/|] [absfile|/etc/bar|]
   , testCase "no pfx" $
         Left (FPathNotAPrefixError absfileT "/dev/" "/etc/bar")
       ≟ stripPrefix' @AbsFile [absdir|/dev/|] [absfile|/etc/bar|]
   , testCase "equal" $
         Left (FPathNotAPrefixError absfileT "/etc/bar/" "/etc/bar")
       ≟ stripPrefix' @AbsFile [absdir|/etc/bar/|] [absfile|/etc/bar|]
   , testCase "longer 1" $
         Left (FPathNotAPrefixError absfileT "/etc/bar/" "/etc")
       ≟ stripPrefix' @AbsFile [absdir|/etc/bar/|] [absfile|/etc|]
   , testCase "longer 2" $
         Left (FPathNotAPrefixError absfileT "/etc/udev/bar/" "/etc/udev")
       ≟ stripPrefix' @AbsFile [absdir|/etc/udev/bar/|] [absfile|/etc/udev|]
   ]

stripProperPrefixAbsDirTests ∷ TestTree
stripProperPrefixAbsDirTests =
  testGroup "absfile"
   [ testCase "/"   $ Right [reldir|foo/|] ≟ stripPrefix' root [absdir|/foo/|]
   , testCase "pfx" $
       Right [reldir|bar/|] ≟ stripPrefix' [absdir|/etc/|] [absdir|/etc/bar/|]
   , testCase "no pfx" $
         Left (FPathNotAPrefixError absdirT "/dev/" "/etc/bar/")
       ≟ stripPrefix' @AbsDir [absdir|/dev/|] [absdir|/etc/bar/|]
   , testCase "equal" $
         Right [reldir|./|]
       ≟ stripPrefix' @AbsDir [absdir|/etc/bar/|] [absdir|/etc/bar/|]
   , testCase "longer 1" $
         Left (FPathNotAPrefixError absdirT "/etc/bar/" "/etc/")
       ≟ stripPrefix' @AbsDir [absdir|/etc/bar/|] [absdir|/etc/|]
   , testCase "longer 2" $
         Left (FPathNotAPrefixError absdirT "/etc/udev/bar/" "/etc/udev/")
       ≟ stripPrefix' @AbsDir [absdir|/etc/udev/bar/|] [absdir|/etc/udev/|]
   ]

stripProperPrefixTests ∷ TestTree
stripProperPrefixTests =
  testGroup "stripProperPrefix" [ stripProperPrefixAbsFileTests
                                , stripProperPrefixAbsDirTests ]

----------------------------------------

fpathTests ∷ TestTree
fpathTests = testGroup "FPath" [ toDirTests, resolveTests
                               , stripProperPrefixTests ]

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

-- Cannot use Fluffy.Tasty here, as we will be a dependency of Fluffy...

_test ∷ IO ()
_test = doTest tests

--------------------

_tests ∷ String → IO ()
_tests = doTestS tests

_testr ∷ String → Natural → IO ()
_testr = doTestR tests

-- that's all, folks! ----------------------------------------------------------
