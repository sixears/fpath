{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.AbsFile
  ( AbsDir, AbsFile, AsAbsFile( _AbsFile )

  , absfileT
  -- quasi-quoters
  , absfile

  , parseAbsFile , parseAbsFile' , __parseAbsFile__ , __parseAbsFile'__

  , tests
  )
where

import Prelude  ( error )

-- base --------------------------------

import qualified  Data.List.NonEmpty  as  NonEmpty

import Control.Applicative  ( pure )
import Control.Monad        ( return )
import Data.Either          ( Either, either )
import Data.Eq              ( Eq )
import Data.Foldable        ( foldMap, foldl1, foldl', foldr, foldr1 )
import Data.Function        ( ($), const, id )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Monoid          ( Monoid )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Exts             ( IsList( fromList, toList ) )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

-- lens --------------------------------

import Control.Lens.Cons   ( unsnoc )
import Control.Lens.Iso    ( iso )
import Control.Lens.Lens   ( lens )
import Control.Lens.Prism  ( Prism', prism' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFoldable( ofoldl', ofoldl1Ex'
                                                    , ofoldMap, ofoldr
                                                    , ofoldr1Ex, otoList )
                             , MonoFunctor( omap )
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋫) )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Monoid       ( ф )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------


import NonEmptyContainers.IsNonEmpty        ( FromNonEmpty( fromNonEmpty )
                                            , IsNonEmpty( nonEmpty )
                                            , ToNonEmpty( toNonEmpty ) )
import NonEmptyContainers.SeqConversions    ( FromMonoSeq( fromSeq )
                                            , ToMonoSeq( toSeq )
                                            )
import NonEmptyContainers.SeqNE             ( pattern (:⪭), (⪭), (⋖) )
import NonEmptyContainers.SeqNEConversions  ( FromMonoSeqNonEmpty( fromSeqNE )
                                            , IsMonoSeqNonEmpty( seqNE )
                                            , ToMonoSeqNonEmpty( toSeqNE
                                                               , toSeq_ )
                                            )

-- parsers -----------------------------

import Text.Parser.Char         ( char )
import Text.Parser.Combinators  ( sepByNonEmpty )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( Text, dropEnd, intercalate, length, splitOn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir            ( AbsDir, parseAbsDir, root )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Basename          ( Basename( basename, updateBasename ) )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathComponentError
                               ( FPathComponentError )
import FPath.Error.FPathError  ( AsFPathError, FPathError
                               , __FPathComponentE__, __FPathEmptyE__
                               , __FPathNonAbsE__, __FPathNotAFileE__
                               , mapTypeRepE, mapTextE
                               )
import FPath.FileLike          ( FileLike( dirfile ) )
import FPath.Parent            ( HasParent( parent ),HasParentMay( parentMay ) )
import FPath.PathComponent     ( PathComponent, parsePathC, pc )
import FPath.RelFile           ( RelFile, relfile )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.Util              ( QuasiQuoter, __ERROR'__, mkQuasiQuoterExp )

-------------------------------------------------------------------------------

{- | an absolute file -}
data AbsFile = AbsFile AbsDir PathComponent
  deriving (Eq, Show)

type instance Element AbsFile = PathComponent

--------------------

class AsAbsFile α where
  _AbsFile ∷ Prism' α AbsFile

instance AsAbsFile AbsFile where
  _AbsFile = id

--------------------

instance DirTypeC AbsFile where
  type DirType AbsFile = AbsDir

--------------------

instance RelTypeC AbsFile where
  type RelType AbsFile = RelFile

----------------------------------------

instance FileLike AbsFile where
  dirfile = iso (\ (AbsFile d f) → (d,f)) (\ (d,f) → AbsFile d f)

----------------------------------------

instance MonoFunctor AbsFile where
  omap ∷ (PathComponent → PathComponent) → AbsFile → AbsFile
  omap f (AbsFile ps b) = AbsFile (omap f ps) (f b)

----------------------------------------

instance MonoFoldable AbsFile where
  otoList ∷ AbsFile → [PathComponent]
  otoList (AbsFile ps f) = otoList ps ⊕ [f]

  ofoldl' ∷ (α → PathComponent → α) → α → AbsFile → α 
  ofoldl' f x r = foldl' f x (toNonEmpty r)

  ofoldr ∷ (PathComponent → α → α) → α → AbsFile → α
  ofoldr f x r = foldr f x (toNonEmpty r)

  ofoldMap ∷ Monoid ν => (PathComponent → ν) → AbsFile → ν
  ofoldMap f r = foldMap f (toNonEmpty r)

  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → AbsFile
            → PathComponent
  ofoldr1Ex f r = foldr1 f (toNonEmpty r)

  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → AbsFile
             → PathComponent
  ofoldl1Ex' f r = foldl1 f (toNonEmpty r)

----------------------------------------

instance FromMonoSeqNonEmpty AbsFile where
  fromSeqNE (ps :⪭ f) = AbsFile (fromSeq ps) f
  fromSeqNE _         = error "AbsFile.fromSeqNE pattern match can't get here"

----------------------------------------

instance ToMonoSeqNonEmpty AbsFile where
  toSeqNE (AbsFile ps f) = toSeq ps ⪭ f

instance ToMonoSeq AbsFile where
  toSeq = toSeq_

----------------------------------------

instance IsMonoSeqNonEmpty AbsFile where
  seqNE = iso toSeqNE fromSeqNE

----------------------------------------

instance FromNonEmpty AbsFile where
  fromNonEmpty (x :| xs) = case NonEmpty.nonEmpty xs of
                             Nothing  → AbsFile (fromList ф) x
                             Just xs' → let pcs = x : NonEmpty.init xs'
                                            f   = NonEmpty.last xs'
                                         in AbsFile (fromList pcs) f

instance ToNonEmpty AbsFile where
  toNonEmpty (AbsFile ps f) = NonEmpty.fromList $ toList ps ⊕ [f]

instance IsNonEmpty AbsFile where
  nonEmpty = iso toNonEmpty fromNonEmpty

----------------------------------------

instance Printable AbsFile where
  print r = P.text $ "/" ⊕ intercalate "/" (toText ⊳ otoList r)

----------------------------------------

instance AsFilePath AbsFile where
  filepath = prism' toString fromString

----------------------------------------

instance Textual AbsFile where
  textual = fromNonEmpty ⊳ (char '/' ⋫ sepByNonEmpty textual (char '/'))

----------------------------------------

instance Arbitrary AbsFile where
  arbitrary = fromSeqNE ⊳ arbitrary
  shrink = fromSeqNE ⩺ shrink ∘ toSeqNE

----------------------------------------

instance HasParent AbsFile where
  parent = lens (\ (AbsFile p _) → p) (\ (AbsFile _ f) p → AbsFile p f)

----------------------------------------

instance HasParentMay AbsFile where
  parentMay = lens (\ (AbsFile p _) → Just p)
                   (\ (AbsFile _ f) md → case md of
                                           Just  d → AbsFile d f
                                           Nothing → AbsFile root f
                   )


----------------------------------------
  
instance Basename AbsFile where
  basename ∷ AbsFile → RelFile
  basename (AbsFile _ n) = fromNonEmpty (pure n)

  updateBasename ∷ (PathComponent → PathComponent) → AbsFile → AbsFile
  updateBasename f (AbsFile d n) = AbsFile d (f n)

absFileBasenameTests ∷ TestTree
absFileBasenameTests =
  testGroup "basename"
            [ testCase "af1" $ [relfile|r.e|]  ≟ basename af1

            , testCase "af1 -> af4"    $
                af4 ≟ updateBasename (const [pc|.x|]) af1
            , testCase "af2 -> x.p"     $
                  fromSeqNE ([pc|r|] ⋖ [[pc|x.p|]])
                ≟ updateBasename (const [pc|x.p|]) af2
            , testCase "pam.d -> pam.d"   $
                  fromSeqNE ([pc|p|] ⋖ [[pc|q|], [pc|r|]])
                ≟ updateBasename (const [pc|r|]) af3
            , testCase "af4 -> af1"   $
                af1 ≟ updateBasename (const [pc|r.e|]) af4
            ]

------------------------------------------------------------
--                     Quasi-Quoting                      --
------------------------------------------------------------

absfileT ∷ TypeRep
absfileT = typeRep (Proxy ∷ Proxy AbsFile)

{- | try to parse a `Textual` as an absolute file -}
parseAbsFile ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η AbsFile
parseAbsFile (toText → t) =
  let mkCompE    ∷ (AsFPathError ε', MonadError ε' η') ⇒
                   FPathComponentError → η' α
      mkCompE ce = __FPathComponentE__ ce absfileT t
      eCompE     ∷ (AsFPathError ε'', MonadError ε'' η'') ⇒
                   Either FPathComponentError α → η'' α
      eCompE = either mkCompE return
      -- map the type marker on FPathError
      mapFPCE ∷ (AsFPathError ε', MonadError ε' η') ⇒
                (Text → Text) → Either FPathError α → η' α
      mapFPCE f = mapTextE f ∘ mapTypeRepE (const absfileT)
   in case unsnoc $ splitOn "/" t of
        Nothing           → error "error: splitOn always returns something"
        Just ([],"")      → __FPathEmptyE__ absfileT
        Just (_, "")      → __FPathNotAFileE__ absfileT t
        Just ([""], f)    → do f' ← eCompE $ parsePathC f
                               return $ AbsFile (fromSeq ф) f'
        Just (("":_), f)  → do f' ← eCompE $ parsePathC f
                               ps' ← mapFPCE (⊕ f) $
                                       parseAbsDir (dropEnd (length f) t)
                               return $ AbsFile ps' f'
        Just (_, _)      → __FPathNonAbsE__ absfileT t

parseAbsFile' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η AbsFile
parseAbsFile' = parseAbsFile

__parseAbsFile__ ∷ Printable τ ⇒ τ → AbsFile
__parseAbsFile__ = either __ERROR'__ id ∘ parseAbsFile'

__parseAbsFile'__ ∷ String → AbsFile
__parseAbsFile'__ = __parseAbsFile__

{- | quasi-quotation -}
absfile ∷ QuasiQuoter
absfile = mkQuasiQuoterExp "absfile" (\ s → ⟦ __parseAbsFile'__ s ⟧)

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------


af1 ∷ AbsFile
af1 = fromSeqNE $ pure [pc|r.e|]

af2 ∷ AbsFile
af2 = fromSeqNE $ [pc|r|] ⋖ [[pc|p.x|]]

af3 ∷ AbsFile
af3 = fromSeqNE $ [pc|p|] ⋖ [[pc|q|], [pc|r.mp3|]]

af4 ∷ AbsFile
af4 = fromSeqNE $ pure [pc|.x|]

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath.AbsFile" [ absFileBasenameTests ]
                
--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
