{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.Rel
  ( AsRel( _Rel ), Rel(..)
  , parseRel, parseRel', __parseRel__, __parseRel'__

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
import Data.MoreUnicode.Lens     ( (##) )
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

import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir
                               , parseRelDir, reldir )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile
                               , parseRelFile, relfile )
import FPath.Util              ( __ERROR'__ )

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

relpathT ∷ TypeRep
relpathT = typeRep (Proxy ∷ Proxy Rel)

parseRel ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η Rel
parseRel (toText → t) =
  case null t of
    True → __FPathEmptyE__ relpathT
    False → case last t of
              '/' → RelD ⊳ parseRelDir  t
              _   → RelF ⊳ parseRelFile t

parseRel' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η Rel
parseRel' = parseRel

__parseRel__ ∷ Printable τ ⇒ τ → Rel
__parseRel__ = either __ERROR'__ id ∘ parseRel'

__parseRel'__ ∷ String → Rel
__parseRel'__ = __parseRel__

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

parseRelTests ∷ TestTree
parseRelTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parseRel' t
   in testGroup "parseRel"
                [ success [reldir|./|]         _RelDir "./"
                , success [reldir|etc/|]       _RelDir "etc/"
                , success [relfile|etc/group|] _RelFile "etc/group"
                ]


tests ∷ TestTree
tests = testGroup "Rel" [ parseRelTests ]
                
-- that's all, folks! ----------------------------------------------------------
