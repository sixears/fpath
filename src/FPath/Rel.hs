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
import Data.Typeable  ( Proxy( Proxy ), TypeRep, typeRep )
import Text.Show      ( Show )

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

import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir, reldir )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile
                               , relfile )

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

instance Parseable Rel where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η Rel
  parse (toText → t) =
    case null t of
      True → __FPathEmptyE__ relpathT
      False → case last t of
                '/' → RelD ⊳ parse  t
                _   → RelF ⊳ parse t

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

parseRelTests ∷ TestTree
parseRelTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parse @Rel @FPathError t
   in testGroup "parseRel"
                [ success [reldir|./|]         _RelDir "./"
                , success [reldir|etc/|]       _RelDir "etc/"
                , success [relfile|etc/group|] _RelFile "etc/group"
                ]


tests ∷ TestTree
tests = testGroup "Rel" [ parseRelTests ]
                
-- that's all, folks! ----------------------------------------------------------
