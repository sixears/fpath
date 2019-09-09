{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.RelPath
  ( AsRelPath( _RelPath ), RelPath(..)
  , parseRelPath, parseRelPath', __parseRelPath__, __parseRelPath'__

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

data RelPath = RelD RelDir | RelF RelFile
  deriving (Eq, Show)

class AsRelPath α where
  _RelPath ∷ Prism' α RelPath

instance AsRelPath RelPath where
  _RelPath = id

----------------------------------------

instance AsRelDir RelPath where
  _RelDir ∷ Prism' RelPath RelDir
  _RelDir = prism' RelD (\ case (RelD d) → Just d; _ → Nothing)

instance AsRelFile RelPath where
  _RelFile ∷ Prism' RelPath RelFile
  _RelFile = prism' RelF (\ case (RelF f) → Just f; _ → Nothing)

----------------------------------------

relpathT ∷ TypeRep
relpathT = typeRep (Proxy ∷ Proxy RelPath)

parseRelPath ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η RelPath
parseRelPath (toText → t) =
  case null t of
    True → __FPathEmptyE__ relpathT
    False → case last t of
              '/' → RelD ⊳ parseRelDir  t
              _   → RelF ⊳ parseRelFile t

parseRelPath' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η RelPath
parseRelPath' = parseRelPath

__parseRelPath__ ∷ Printable τ ⇒ τ → RelPath
__parseRelPath__ = either __ERROR'__ id ∘ parseRelPath'

__parseRelPath'__ ∷ String → RelPath
__parseRelPath'__ = __parseRelPath__

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

parseRelPathTests ∷ TestTree
parseRelPathTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parseRelPath' t
   in testGroup "parseRelPath"
                [ success [reldir|./|]         _RelDir "./"
                , success [reldir|etc/|]       _RelDir "etc/"
                , success [relfile|etc/group|] _RelFile "etc/group"
                ]


tests ∷ TestTree
tests = testGroup "RelPath" [ parseRelPathTests ]
                
-- that's all, folks! ----------------------------------------------------------
