{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.PathComponent
  ( PathComponent, parsePathC, pathComponent, pc )
where

import Prelude  ( (!!), mod )

-- base --------------------------------

import qualified  Data.List  as  List

import Control.Applicative  ( some )
import Control.Monad        ( return )
import Data.Bool            ( not, otherwise )
import Data.Char            ( isAlphaNum, ord )
import Data.Either          ( either )
import Data.Eq              ( Eq )
import Data.Foldable        ( length )
import Data.Function        ( ($), id )
import Data.Functor         ( (<$>), fmap )
import Data.List            ( filter, notElem, subsequences )
import Data.String          ( String )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡), (≢) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , toString, toText )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsers -----------------------------

import Text.Parser.Char  ( noneOf )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- text --------------------------------

import Data.Text  ( Text, any, pack, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Error.FPathComponentError
           ( AsFPathComponentError, FPathComponentError
           , __FPathCEmptyE__, __FPathCIllegalCharE__ )
import FPath.Util  ( QuasiQuoter, __ERROR'__, mkQuasiQuoterExp )

--------------------------------------------------------------------------------

{- | A single component in a Path, that is, a directory or file ---
     notably no slashes are allowed (or nul chars); must not be empty
 -}
newtype PathComponent = PathComponent Text
  deriving Eq

instance Show PathComponent where
  show (PathComponent t) = "[pathComponent|" ⊕ unpack t ⊕ "|]"

instance Printable PathComponent where
  print (PathComponent t) = P.text t

instance Textual PathComponent where
  textual = PathComponent ∘ pack <$> some (noneOf "/\0")

instance Arbitrary PathComponent where
  arbitrary = let filtBadChars = filter (`notElem` ['/','\0'])
               in PathComponent <$> (pack ∘ filtBadChars ) <$> arbitrary

  -- shrink by -) trying proper substrings
  --           -) replacing non-alphanumeric characters with alphanums
  -- per the QuickCheck doc, try the more aggressive efforts first
  shrink p = let properSubs x = filter (≢ x) (subsequences x)
                 alphaNums = "abcdefghijklmnopqrstuvwxyz01234567890"
                 trChar c = if isAlphaNum c
                            then c
                            else alphaNums !! (ord c `mod` length alphaNums)
                 tr s = fmap trChar <$> s
                 subs = properSubs (toString p)
                 subsToTr = filter (List.any $ not ∘ isAlphaNum)
                                   (toString p : subs)
                 
              in (PathComponent ∘ toText) <$> ((tr subsToTr) ⊕ subs)

----------------------------------------

parsePathC ∷ (Printable ρ, AsFPathComponentError ε, MonadError ε η) ⇒
             ρ → η PathComponent
parsePathC (toText → "")                 = __FPathCEmptyE__
parsePathC (toText → t) | any (≡ '\0') t = __FPathCIllegalCharE__ '\0' t
                        | any (≡ '/')  t = __FPathCIllegalCharE__ '/' t
                        | otherwise      = return $ PathComponent t

parsePathC' ∷ (Printable ρ, MonadError FPathComponentError η) ⇒
              ρ → η PathComponent
parsePathC' = parsePathC

__parsePathC__ ∷ Printable τ ⇒ τ → PathComponent
__parsePathC__ = either __ERROR'__ id ∘ parsePathC'

__parsePathC'__ ∷ String → PathComponent
__parsePathC'__ = __parsePathC__

{- | quasi-quoter for PathComponent -}
pathComponent ∷ QuasiQuoter
pathComponent = mkQuasiQuoterExp "pathComponent" $ \ s → ⟦ __parsePathC'__ s ⟧

{- | abbreviation for `pathComponent` -}
pc ∷ QuasiQuoter
pc = pathComponent

-- that's all, folks! ----------------------------------------------------------
