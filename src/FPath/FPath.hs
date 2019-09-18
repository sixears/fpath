{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.FPath
  ( FPath, parseFPath, parseFPath', __parseFPath__ , __parseFPath'__, tests )
where

-- base --------------------------------

import Data.Bool      ( Bool( False, True ) )
import Data.Either    ( Either( Right ), either )
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

import Data.Textual  ( Printable, toText )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism', prism' )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⩼), (##) )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.Tasty    ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

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
                               , absdir, parseAbsDir
                               )
import FPath.AbsFile           ( AbsFile, AsAbsFile( _AbsFile )
                               , absfile )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parseable         ( parse )
import FPath.Rel               ( AsRel(_Rel ), Rel( RelD, RelF ) )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir
                               , parseRelDir, reldir
                               )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile, relfile )
import FPath.Util              ( __ERROR'__ )

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

------------------------------------------------------------

fpathT ∷ TypeRep
fpathT = typeRep (Proxy ∷ Proxy FPath)

parseFPath ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η FPath
parseFPath (toText → t) =
  case null t of
    True → __FPathEmptyE__ fpathT
    False → case (head t, last t) of
              ('/','/') → FAbsD ⊳ parseAbsDir  t
              ('/',_  ) → FAbsF ⊳ parse        t
              (_  ,'/') → FRelD ⊳ parseRelDir  t
              (_  ,_  ) → FRelF ⊳ parse        t

parseFPath' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η FPath
parseFPath' = parseFPath

__parseFPath__ ∷ Printable τ ⇒ τ → FPath
__parseFPath__ = either __ERROR'__ id ∘ parseFPath'

__parseFPath'__ ∷ String → FPath
__parseFPath'__ = __parseFPath__

parseFPathTests ∷ TestTree
parseFPathTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parseFPath' t
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
tests = testGroup "FPath" [ parseFPathTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
