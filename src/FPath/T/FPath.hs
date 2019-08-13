{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath
  ( tests )
where

-- base --------------------------------

import Data.String      ( String )
import Numeric.Natural  ( Natural )
import System.IO        ( IO )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  FPath.T.FPath.AbsDir
import qualified  FPath.T.FPath.AbsFile
import qualified  FPath.T.FPath.NonRootAbsDir
import qualified  FPath.T.FPath.PathComponent
import qualified  FPath.T.FPath.RelDir
import qualified  FPath.T.FPath.RelFile

import FPath.T.Common  ( doTest, doTestR, doTestS )

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "FPath" [ FPath.T.FPath.PathComponent.tests
                          , FPath.T.FPath.AbsDir.tests
                          , FPath.T.FPath.NonRootAbsDir.tests
                          , FPath.T.FPath.RelDir.tests
                          , FPath.T.FPath.RelFile.tests
                          , FPath.T.FPath.AbsFile.tests
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
