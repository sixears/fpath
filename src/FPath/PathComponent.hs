{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

{- | A permissable file name (excluding any directory part), thus a sequence of
     characters excluding the null character (`\0`) or the slash character (`/`)
     and also explicitly excluding the special components `.` and `..` .
 -}
module FPath.PathComponent
  ( PathComponent
  , (⊙)
  , addExt, ext, parsePathC, pathComponent, pc, splitExt, stub, toLower, toUpper
  , updateExt

  , tests
  )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Control.Applicative  ( many, some )
import Control.Monad        ( return )
import Data.Bool            ( otherwise )
import Data.Char            ( Char, chr )
import Data.Data            ( Data )
import Data.Either          ( either )
import Data.Eq              ( Eq )
import Data.Function        ( ($), (&), id )
import Data.Functor         ( (<$>), fmap )
import Data.List            ( any, elem, filter, find, nub, subsequences )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Monoid          ( mconcat )
import Data.Semigroup       ( Semigroup )
import Data.String          ( String )
import Data.Tuple           ( fst, snd )
import Data.Word            ( Word8 )
import GHC.Generics         ( Generic )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡), (≢) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-default ------------------------

import Data.Default  ( def )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual ), toString )

-- genvalidity -------------------------

import Data.GenValidity  ( GenUnchecked, GenValid( genValid, shrinkValid )
                         , isValid )

-- genvalidity-bytestring --------------

import Data.GenValidity.ByteString  ( )

-- genvalidity-text --------------------

import Data.GenValidity.Text  ( )

-- monaderror-io -----------------------

import MonadError  ( ѭ )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (∤) )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊩) )
import Data.MoreUnicode.Natural      ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsers -----------------------------

import Text.Parser.Char  ( char, noneOf, string )

-- quasiquoting ------------------------

import QuasiQuoting  ( QuasiQuoter, mkQQ, exp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( suchThat )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), runTestsP, runTestsReplay, runTestTree )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import qualified  Data.Text  as  Text
import Data.Text  ( Text, break, init, pack, reverse, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- th-lift-instances -------------------

import Instances.TH.Lift ()

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.DirType            ( DirTypeC( DirType ) )
import FPath.Error.FPathComponentError
                                ( AsFPathComponentError, FPathComponentError
                                , __FPathCEmptyE__, __FPathCIllegalE__
                                , __FPathCIllegalCharE__
                                )
import FPath.Util               ( __ERROR'__, mkVisS )

--------------------------------------------------------------------------------

{-| file name components may contain any character 'cept NUL and SLASH -}
badChars :: [Char]
badChars = (chr ∘ fromIntegral @Word8) ⊳ [0,47] -- '\0', '/'

{- | A single component in a Path, that is, a directory or file ---
     notably no slashes are allowed (or nul chars); must not be empty
 -}
newtype PathComponent = PathComponent Text
  deriving (Eq, Data, Generic, GenUnchecked, Lift, Semigroup, Show)

instance Printable PathComponent where
  print (PathComponent t) = P.text t

instance Validity PathComponent where
  validate p =
    let badCs = declare "has neither NUL nor /" $
                  Nothing ≡ find (`elem` badChars) (toString p)
        nonEmpty = declare "is not empty" $ "" ≢ toString p
        nonDotties = declare "is not \".\" or \"..\"" $
                       "." ≢ toString p ∧ ".." ≢ toString p
          
     in mconcat [ badCs, nonEmpty, nonDotties ]

instance GenValid PathComponent where
  genValid = (PathComponent ⊳ genValid) `suchThat` isValid
  shrinkValid (PathComponent t) = PathComponent ⊳ shrinkValid t

instance Textual PathComponent where
  textual =
    let parseChars = noneOf badChars
        parseNoDotsNorBads = noneOf (badChars ⊕ ['.'])
        jjoin a b ys = a : b : ys
        matches =   ((:) ⊳ parseNoDotsNorBads ⊵ many parseChars)
                  ∤ ((⊕) ⊳ string ".." ⊵ some parseChars)
                  ∤ (jjoin ⊳ char '.' ⊵ parseNoDotsNorBads ⊵ many parseChars)
                  
     in PathComponent ∘ pack ⊳ matches

instance DirTypeC PathComponent where
  -- needed for `FileLike`
  type DirType PathComponent = ()

instance Arbitrary PathComponent where
  arbitrary = genValid

  -- shrink by -) trying proper substrings
  --           -) replacing non-alphanumeric characters with alphanums
  -- per the QuickCheck doc, try the more aggressive efforts first
  shrink (PathComponent p) =
    let subs ∷ [String]
        subs = subsequences (unpack p)
        all_subs ∷ [String]
        all_subs = nub $ (mkVisS <$> subs) ⊕ subs
        filt_subs ∷ [String] → [String]
        filt_subs = filter $ \ t → t ≢ unpack p ∧ t ≢ ""
     in fmap (PathComponent ∘ pack) $ filt_subs all_subs

----------------------------------------

parsePathC ∷ (Printable τ, AsFPathComponentError ε, MonadError ε η) ⇒
             τ → η PathComponent
parsePathC (toString → "")                 = __FPathCEmptyE__
parsePathC (toString → ".")                = __FPathCIllegalE__ "."
parsePathC (toString → "..")               = __FPathCIllegalE__ ".."
parsePathC (toString → t) | any (≡ '\0') t = __FPathCIllegalCharE__ '\0' t
                          | any (≡ '/')  t = __FPathCIllegalCharE__ '/' t
                          | otherwise      = return $ PathComponent (pack t)

parsePathC' ∷ (Printable τ, MonadError FPathComponentError η) ⇒
              τ → η PathComponent
parsePathC' = parsePathC

__parsePathC__ ∷ Printable τ ⇒ τ → PathComponent
__parsePathC__ = either __ERROR'__ id ∘ parsePathC'

__parsePathC'__ ∷ String → PathComponent
__parsePathC'__ = __parsePathC__

{- | quasi-quoter for PathComponent -}
pathComponentQQ ∷ String → Maybe ExpQ
pathComponentQQ = (\ p → ⟦p⟧) ⩺ (ѭ ∘ parsePathC')

pathComponent ∷ QuasiQuoter
pathComponent = mkQQ "PathComponent" $ def & exp ⊩ pathComponentQQ

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
pcmap ∷ (Text → Text) → PathComponent → PathComponent
pcmap f (PathComponent p) = PathComponent (f p)

toUpper ∷ PathComponent → PathComponent
toUpper = pcmap Text.toUpper
  
toLower ∷ PathComponent → PathComponent
toLower = pcmap Text.toLower

----------------------------------------

{- | Add an "extension", that is, join two `PathComponent`s with a '.'
     character
 -}
addExt ∷ PathComponent → PathComponent → PathComponent
addExt (PathComponent pfx) (PathComponent sfx) = PathComponent $ pfx ⊕ "." ⊕ sfx

{- | operator alias for `addExt` -}
infixr 6 ⊙ -- same as for ⊕
(⊙) ∷ PathComponent → PathComponent → PathComponent
(⊙) = addExt

splitExt ∷ PathComponent → (PathComponent, Maybe PathComponent)
splitExt p@(PathComponent xs) =
  case break (≡ '.') (reverse xs) of
    (_,"")    → (p, Nothing)
    ("",_)    → (p, Nothing)
    (sfx,pfx) → (PathComponent $ init (reverse pfx),
                 Just $ PathComponent (reverse sfx))

stub ∷ PathComponent → PathComponent
stub = fst ∘ splitExt

stubTests ∷ TestTree
stubTests =
  let foo    = PathComponent "foo"
      fooBar = PathComponent "foo.bar"
      fooDot = PathComponent "foo."
   in testGroup "stub" [ testCase "foo.bar" $ foo    ≟ stub fooBar
                       , testCase "foo"     $ foo    ≟ stub foo
                       , testCase "foo."    $ fooDot ≟ stub fooDot
                       ]

ext ∷ PathComponent → Maybe PathComponent
ext = snd ∘ splitExt

updateExt ∷ (PathComponent → PathComponent) → PathComponent → PathComponent
updateExt f p@(splitExt → (s, x)) = case x of
                                       Just e  → s ⊙ f e
                                       Nothing → p

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "PathComponent" [ stubTests ]
                
--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
