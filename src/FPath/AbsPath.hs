{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.AbsPath
  ( AbsPath(..), AsAbsPath( _AbsPath )
  , abspathT, parseAbsPath, parseAbsPath', __parseAbsPath__, __parseAbsPath'__

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

data AbsPath = AbsD AbsDir | AbsF AbsFile
  deriving (Eq, Show)

class AsAbsPath α where
  _AbsPath ∷ Prism' α AbsPath

instance AsAbsPath AbsPath where
  _AbsPath = id

----------------------------------------

instance AsAbsDir AbsPath where
  _AbsDir ∷ Prism' AbsPath AbsDir
  _AbsDir = prism' AbsD (\ case (AbsD d) → Just d; _ → Nothing)

instance AsNonRootAbsDir AbsPath where
  _NonRootAbsDir ∷ Prism' AbsPath NonRootAbsDir
  _NonRootAbsDir = prism' (AbsD ∘ toAbsDir)
                          (\ case (AbsD d) → d ⩼ _NonRootAbsDir; _ → Nothing)

instance AsAbsFile AbsPath where
  _AbsFile ∷ Prism' AbsPath AbsFile
  _AbsFile = prism' AbsF (\ case (AbsF f) → Just f; _ → Nothing)

------------------------------------------------------------

abspathT ∷ TypeRep
abspathT = typeRep (Proxy ∷ Proxy AbsPath)

parseAbsPath ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η AbsPath
parseAbsPath (toText → t) =
  case null t of
    True → __FPathEmptyE__ abspathT
    False → case last t of
              '/' → AbsD ⊳ parseAbsDir  t
              _   → AbsF ⊳ parseAbsFile t

parseAbsPathTests ∷ TestTree
parseAbsPathTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parseAbsPath' t
   in testGroup "parseAbsPath"
                [ success [absdir|/|]           _AbsDir "/"
                , success [absdir|/etc/|]       _AbsDir "/etc/"
                , success [absfile|/etc/group|] _AbsFile "/etc/group"
                ]

parseAbsPath' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η AbsPath
parseAbsPath' = parseAbsPath

__parseAbsPath__ ∷ Printable τ ⇒ τ → AbsPath
__parseAbsPath__ = either __ERROR'__ id ∘ parseAbsPath'

__parseAbsPath'__ ∷ String → AbsPath
__parseAbsPath'__ = __parseAbsPath__

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "AbsPath" [ parseAbsPathTests ]
                
-- that's all, folks! ----------------------------------------------------------
