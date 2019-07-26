{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

{- | A permissable file name (excluding any directory part), thus a sequence of
     characters excluding the null character (`\0`) or the slash character (`/`)
     and also explicitly excluding the special components `.` and `..` .
 -}
module FPath.PathComponent
  ( PathComponent, parsePathC, pathComponent, pc, toLower, toUpper )
where

-- base --------------------------------

import Control.Applicative  ( many, some )
import Control.Monad        ( return )
import Data.Bool            ( otherwise )
import Data.Either          ( either )
import Data.Eq              ( Eq )
import Data.Function        ( ($), id )
import Data.Functor         ( (<$>), fmap )
import Data.List            ( any, elem, filter, find, nub, subsequences )
import Data.Maybe           ( Maybe( Nothing ) )
import Data.Monoid          ( mconcat )
import Data.Semigroup       ( Semigroup )
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

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsers -----------------------------

import Text.Parser.Char  ( char, noneOf, string )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  FPath.PathCTypes.String  as  PathCTypes

import FPath.Error.FPathComponentError
           ( AsFPathComponentError, FPathComponentError
           , __FPathCEmptyE__, __FPathCIllegalE__, __FPathCIllegalCharE__ )
import FPath.PathCTypes.String  ( PathCChar, PathCInner, pathCChar
                                , to_inner, to_print, to_string )
import FPath.Util               ( QuasiQuoter
                                , __ERROR'__, mkQuasiQuoterExp, mkVisS )

--------------------------------------------------------------------------------

{-| file name components may contain any character 'cept NUL and SLASH -}
badChars :: [PathCChar]
badChars = pathCChar <$> [0,47] -- '\0', '/'


{- | A single component in a Path, that is, a directory or file ---
     notably no slashes are allowed (or nul chars); must not be empty
 -}
newtype PathComponent = PathComponent PathCInner
  deriving (Eq, Generic, GenUnchecked, Semigroup)

instance Show PathComponent where
  show (PathComponent t) = "[pathComponent|" ⊕ to_string t ⊕ "|]"

instance Printable PathComponent where
  print (PathComponent t) = to_print t

instance Validity PathComponent where
  validate p =
    let badCs = declare "has neither NUL nor /" $
                  Nothing ≡ find (`elem` badChars) (toString p)
        nonEmpty = declare "is not empty" $ "" ≢ toString p
        nonDotties = declare "is not \".\" or \"..\"" $
                       "." ≢ toString p ∧ ".." ≢ toString p
          
     in mconcat [ badCs, nonEmpty, nonDotties ]

instance GenValid PathComponent where

instance Textual PathComponent where
  textual =
    let parseChars = noneOf badChars
        parseNoDotsNorBads = noneOf (badChars ⊕ ['.'])
        jjoin a b ys = a : b : ys
        matches =   ((:) ⊳ parseNoDotsNorBads ⊵ many parseChars)
                  ∤ ((⊕) ⊳ string ".." ⊵ some parseChars)
                  ∤ (jjoin ⊳ char '.' ⊵ parseNoDotsNorBads ⊵ many parseChars)
                  
     in PathComponent ∘ to_inner ⊳ matches

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
parsePathC (toString → ".")                = __FPathCIllegalE__ "."
parsePathC (toString → "..")               = __FPathCIllegalE__ ".."
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

----------------------------------------

-- don't do this!  In general, omaps over monofunctor aren't safe, as
-- they may map the "inner" "string" to something invalid (containing /, or \0,
-- or empty).  And if we declare this instance here, then it will get exported -
-- no way to stop that :-(.
{-
type instance Element PathComponent = PathCInner

instance MonoFunctor PathComponent where
  omap = _  
-}

-- don't export this, it wouldn't be safe... it must only be used with functions
-- that map a Valid PathComponent to a Valid PathComponent
pcmap ∷ (PathCInner → PathCInner) → PathComponent → PathComponent
pcmap f (PathComponent p) = PathComponent (f p)

toUpper ∷ PathComponent → PathComponent
toUpper = pcmap PathCTypes.to_upper
  
toLower ∷ PathComponent → PathComponent
toLower = pcmap PathCTypes.to_lower

-- that's all, folks! ----------------------------------------------------------
