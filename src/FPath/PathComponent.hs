{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module FPath.PathComponent
  -- XXX REMOVE PathComponent DATA CTOR
  ( PathComponent( PathComponent ), parsePathC, pathComponent, pc )
where

-- base --------------------------------

import Control.Applicative  ( some )
import Control.Monad        ( return )
import Data.Bool            ( otherwise )
import Data.Either          ( either )
import Data.Eq              ( Eq )
import Data.Function        ( ($), id )
import Data.Functor         ( (<$>), fmap )
import Data.List            ( any, elem, filter, find, nub, subsequences )
import Data.Maybe           ( Maybe( Nothing ) )
import Data.Monoid          ( mconcat )
import Data.String          ( String )
import GHC.Generics         ( Generic )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡), (≢) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual ), toString )

-- genvalidity -------------------------

import Data.GenValidity  ( GenUnchecked, GenValid( genValid ) )

-- genvalidity-bytestring --------------

import Data.GenValidity.ByteString  ( )

-- genvalidity-text --------------------

import Data.GenValidity.Text  ( )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsers -----------------------------

import Text.Parser.Char  ( noneOf )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Error.FPathComponentError
           ( AsFPathComponentError, FPathComponentError
           , __FPathCEmptyE__, __FPathCIllegalCharE__ )
-- import FPath.PathCTypes.ByteString     ( PathCChar, PathCInner, pathCChar
-- import FPath.PathCTypes.ByteStringUtf8 ( PathCChar, PathCInner, pathCChar
import FPath.PathCTypes.String         ( PathCChar, PathCInner, pathCChar
-- import FPath.PathCTypes.Text           ( PathCChar, PathCInner, pathCChar
                                       , to_inner, to_print, to_string )
import FPath.Util  ( QuasiQuoter, __ERROR'__, mkQuasiQuoterExp, mkVisS )

--------------------------------------------------------------------------------

{-| file name components may contain any character 'cept NUL and SLASH -}
badChars :: [PathCChar]
badChars = pathCChar <$> [0,47] -- '\0', '/'


{- | A single component in a Path, that is, a directory or file ---
     notably no slashes are allowed (or nul chars); must not be empty
 -}
newtype PathComponent = PathComponent PathCInner
  deriving (Eq, Generic, GenUnchecked)

instance Show PathComponent where
  show (PathComponent t) = "[pathComponent|" ⊕ to_string t ⊕ "|]"

instance Printable PathComponent where
  print (PathComponent t) = to_print t

instance Validity PathComponent where
  validate p =
    let badCs = declare "has neither NUL nor /" $
                    Nothing ≡ find (`elem` badChars) (toString p)
        nonEmpty = declare "is not empty" $ "" ≢ toString p
     in mconcat [ badCs, nonEmpty ]

instance GenValid PathComponent where

instance Textual PathComponent where
  textual = PathComponent ∘ to_inner <$> some (noneOf badChars)

instance Arbitrary PathComponent where
  arbitrary = genValid

  -- shrink by -) trying proper substrings
  --           -) replacing non-alphanumeric characters with alphanums
  -- per the QuickCheck doc, try the more aggressive efforts first
  shrink (PathComponent p) =
    let subs ∷ [String]
        subs = subsequences (to_string p)
        all_subs ∷ [String]
        all_subs = nub $ (mkVisS <$> subs) ⊕ subs
        filt_subs ∷ [String] → [String]
        filt_subs = filter $ \ t → t ≢ to_string p ∧ t ≢ ""
     in fmap (PathComponent ∘ to_inner) $ filt_subs all_subs

----------------------------------------

parsePathC ∷ (Printable ρ, AsFPathComponentError ε, MonadError ε η) ⇒
             ρ → η PathComponent
parsePathC (toString → "")                 = __FPathCEmptyE__
parsePathC (toString → t) | any (≡ '\0') t = __FPathCIllegalCharE__ '\0' t
                          | any (≡ '/')  t = __FPathCIllegalCharE__ '/' t
                          | otherwise      = return $ PathComponent (to_inner t)

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
