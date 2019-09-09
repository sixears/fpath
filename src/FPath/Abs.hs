{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.Abs
  ( Abs(..), AsAbs( _Abs )
  , absT, parseAbs, parseAbs', __parseAbs__, __parseAbs'__

  , tests
  )
where

-- base --------------------------------

import Data.Bool      ( Bool( False, True ) )
import Data.Either    ( Either( Right ), either )
import Data.Eq        ( Eq )
import Data.Function  ( ($), id )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import Data.Typeable  ( Proxy( Proxy ), TypeRep, typeRep )
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
import Data.MoreUnicode.Tasty    ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- text --------------------------------

import Data.Text  ( last, null )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir            ( AbsDir, AsAbsDir( _AbsDir)
                               , AsNonRootAbsDir( _NonRootAbsDir )
                               , NonRootAbsDir
                               , absdir, parseAbsDir, toAbsDir
                               )
import FPath.AbsFile           ( AbsFile, AsAbsFile( _AbsFile )
                               , absfile, parseAbsFile )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Util              ( __ERROR'__ )

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

------------------------------------------------------------

absT ∷ TypeRep
absT = typeRep (Proxy ∷ Proxy Abs)

parseAbs ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η Abs
parseAbs (toText → t) =
  case null t of
    True → __FPathEmptyE__ absT
    False → case last t of
              '/' → AbsD ⊳ parseAbsDir  t
              _   → AbsF ⊳ parseAbsFile t

parseAbsTests ∷ TestTree
parseAbsTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parseAbs' t
   in testGroup "parseAbs"
                [ success [absdir|/|]           _AbsDir "/"
                , success [absdir|/etc/|]       _AbsDir "/etc/"
                , success [absfile|/etc/group|] _AbsFile "/etc/group"
                ]

parseAbs' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η Abs
parseAbs' = parseAbs

__parseAbs__ ∷ Printable τ ⇒ τ → Abs
__parseAbs__ = either __ERROR'__ id ∘ parseAbs'

__parseAbs'__ ∷ String → Abs
__parseAbs'__ = __parseAbs__

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Abs" [ parseAbsTests ]
                
-- that's all, folks! ----------------------------------------------------------
