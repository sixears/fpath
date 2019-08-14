{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns        #-}

module FPath.AbsDir
  ( AbsDir, NonRootAbsDir

  -- quasi-quoters
  , absdir, absdirN

  , nonRootAbsDir
  , root

  , parseAbsDir , parseAbsDir' , __parseAbsDir__ , __parseAbsDir'__
  , parseAbsDirN, parseAbsDirN', __parseAbsDirN__, __parseAbsDirN'__
  )
where

import Prelude  ( error )

-- base --------------------------------

import qualified  Data.List.NonEmpty  as  NonEmpty

import Control.Monad  ( mapM, return )
import Data.Either    ( Either( Left, Right ), either )
import Data.Eq        ( Eq )
import Data.Foldable  ( concat, foldl', foldl1, foldMap, foldr, foldr1 )
import Data.Function  ( ($), id )
import Data.Functor   ( fmap )
import Data.Maybe     ( Maybe( Just, Nothing ), maybe )
import Data.Monoid    ( Monoid )
import Data.String    ( String )
import Data.Typeable  ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Exts       ( IsList( fromList, toList ), Item )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (∅), (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

-- lens --------------------------------

import Control.Lens.Cons    ( unsnoc )
import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( iso )
import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFoldable( ofoldl', ofoldl1Ex'
                                                    , ofoldMap, ofoldr
                                                    , ofoldr1Ex, otoList )
                             , MonoFunctor( omap )
                             )
import Data.NonNull          ( fromNullable )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋫) )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ф )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import qualified  NonEmptyContainers.SeqConversions  as  SeqConversions
import qualified  NonEmptyContainers.SeqNE           as  SeqNE

import NonEmptyContainers.IsNonEmpty  ( FromNonEmpty( fromNonEmpty )
                                      , IsNonEmpty( nonEmpty )
                                      , ToNonEmpty( toNonEmpty )
                                      , defaultNonEmpty
                                      )
import NonEmptyContainers.SeqConversions
                                    ( FromMonoSeq( fromSeq ), IsMonoSeq( seq )
                                    , ToMonoSeq( toSeq ) )
import NonEmptyContainers.SeqNE     ( SeqNE, (⫸) )
import NonEmptyContainers.SeqNEConversions
                                    ( FromMonoSeqNonEmpty( fromSeqNE )
                                    , IsMonoSeqNonEmpty( seqNE )
                                    , ToMonoSeqNonEmpty( toSeqNE, toSeq_ ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char )
import Text.Parser.Combinators  ( endBy, endByNonEmpty )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- text --------------------------------

import Data.Text  ( splitOn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.DirType           ( DirType )
import FPath.Error.FPathComponentError
                               ( FPathComponentError )
import FPath.Error.FPathError  ( AsFPathError, FPathError( FPathRootDirE )
                               , __FPathComponentE__, __FPathEmptyE__
                               , __FPathAbsE__, __FPathNonAbsE__
                               , __FPathNotADirE__
                               )
import FPath.HasAbsOrRel       ( Abs, HasAbsOrRel( AbsOrRel ) )
import FPath.HasParent         ( HasParent( parent ),HasParentMay( parentMay ) )
import FPath.PathComponent     ( PathComponent, parsePathC )
import FPath.Util              ( QuasiQuoter
                               , __ERROR'__, mkQuasiQuoterExp )

-------------------------------------------------------------------------------

{- | The root directory, i.e., '/' -}
data RootDir = RootDir
  deriving (Eq, Show)

{- | A non-root absolute directory, e.g., /etc. -}
-- a non-root dir is a path component appended to a (possibly-root) absolute
-- directory
newtype NonRootAbsDir = NonRootAbsDir (SeqNE PathComponent)
-- data NonRootAbsDir = NonRootAbsDir PathComponent AbsDir
  deriving (Eq, Show)

{- | An absolute directory is either the root directory, or a non-root absolute
     directory -}
data AbsDir = AbsRootDir | AbsNonRootDir NonRootAbsDir
  deriving (Eq, Show)

absNonRootDir ∷ SeqNE PathComponent → AbsDir
absNonRootDir = AbsNonRootDir ∘ NonRootAbsDir

----------------------------------------

type instance Element NonRootAbsDir = PathComponent
type instance Element AbsDir = PathComponent

----------------------------------------

instance MonoFunctor NonRootAbsDir where
  omap ∷ (PathComponent → PathComponent) → NonRootAbsDir → NonRootAbsDir
--  omap f (NonRootAbsDir p a) = NonRootAbsDir (f p) (omap f a)
  omap f (NonRootAbsDir ps) = NonRootAbsDir (omap f ps)

instance MonoFunctor AbsDir where
  omap ∷ (PathComponent → PathComponent) → AbsDir → AbsDir
  omap _ AbsRootDir = AbsRootDir
  omap f (AbsNonRootDir d) = AbsNonRootDir (omap f d)

nonRootAbsDir ∷ Prism' AbsDir NonRootAbsDir
nonRootAbsDir = prism' AbsNonRootDir go
                where go AbsRootDir        = Nothing
                      go (AbsNonRootDir d) = Just d

----------------------------------------

instance MonoFoldable AbsDir where
  otoList ∷ AbsDir → [PathComponent]
  otoList = toList
  ofoldl' ∷ (α → PathComponent → α) → α → AbsDir → α 
  ofoldl' f x r = foldl' f x (toList r)

  ofoldr ∷ (PathComponent → α → α) → α → AbsDir → α
  ofoldr f x r = foldr f x (toList r)
  ofoldMap ∷ Monoid ν => (PathComponent → ν) → AbsDir → ν
  ofoldMap f r = foldMap f (toList r)
  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → AbsDir
            → PathComponent
  ofoldr1Ex f r = foldr1 f (toList r)
  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → AbsDir
             → PathComponent
  ofoldl1Ex' f r = foldl1 f (toList r)

----------------------------------------

instance MonoFoldable NonRootAbsDir where
  otoList ∷ NonRootAbsDir → [PathComponent]
  otoList = NonEmpty.toList ∘ toNonEmpty
  ofoldl' ∷ (α → PathComponent → α) → α → NonRootAbsDir → α 
  ofoldl' f x r = foldl' f x (toNonEmpty r)

  ofoldr ∷ (PathComponent → α → α) → α → NonRootAbsDir → α
  ofoldr f x r = foldr f x (toNonEmpty r)
  ofoldMap ∷ Monoid ν => (PathComponent → ν) → NonRootAbsDir → ν
  ofoldMap f r = foldMap f (toNonEmpty r)
  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → NonRootAbsDir
            → PathComponent
  ofoldr1Ex f r = foldr1 f (toNonEmpty r)
  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → NonRootAbsDir
             → PathComponent
  ofoldl1Ex' f r = foldl1 f (toNonEmpty r)

----------------------------------------

instance FromMonoSeqNonEmpty NonRootAbsDir where
  fromSeqNE ∷ SeqNE PathComponent → NonRootAbsDir
  fromSeqNE = NonRootAbsDir

instance FromMonoSeqNonEmpty AbsDir where
  fromSeqNE = AbsNonRootDir ∘ fromSeqNE

----------------------------------------

instance ToMonoSeqNonEmpty NonRootAbsDir where
  toSeqNE (NonRootAbsDir ps) = ps

----------------------------------------

instance IsMonoSeqNonEmpty NonRootAbsDir where
  seqNE = iso toSeqNE fromSeqNE

----------------------------------------

instance FromMonoSeq AbsDir where
  fromSeq xs = {-
               go (Seq.reverse $ xs)
               where go Seq.Empty = AbsRootDir
                     go (y :<| ys) = AbsNonRootDir (NonRootAbsDir y (go ys))
               -}
               case fromNullable xs of
                 Nothing → AbsRootDir
                 Just ps → fromSeqNE (toSeqNE ps)

----------------------------------------

instance ToMonoSeq AbsDir where
  toSeq AbsRootDir = (∅)
  toSeq (AbsNonRootDir n) = toSeq n

instance ToMonoSeq NonRootAbsDir where
  toSeq = toSeq_

----------------------------------------

instance IsMonoSeq AbsDir where
  seq = iso toSeq fromSeq

----------------------------------------

instance IsList AbsDir where
  type instance Item AbsDir = PathComponent
  fromList = fromSeq ∘ Seq.fromList
  toList   = toList ∘ toSeq

----------------------------------------

instance FromNonEmpty NonRootAbsDir where
  fromNonEmpty = fromSeqNE ∘ fromNonEmpty

----------------------------------------

instance ToNonEmpty NonRootAbsDir where
  toNonEmpty   = toNonEmpty ∘ toSeqNE

----------------------------------------

instance IsNonEmpty NonRootAbsDir where
  nonEmpty = defaultNonEmpty

----------------------------------------

{- | Convert a sequence of printable elements to strings by intercalating '/'
     characters; with a post-fact string transformation for extra bells (e.g.,
     a prefix '/' character -}
pDir ∷ (P.Printer ρ, ToMonoSeq α, Printable (Element α)) ⇒
       (String → String) → α → ρ
pDir f =  P.string ∘ f ∘ concat ∘ fmap ((⊕ "/") ∘ toString) ∘ toSeq

instance Printable AbsDir where
  print = pDir ("/" ⊕)

instance Printable NonRootAbsDir where
  print = pDir ("/" ⊕)

----------------------------------------

instance AsFilePath AbsDir where
  filepath = prism' toString fromString

instance AsFilePath NonRootAbsDir where
  filepath = prism' toString fromString

----------------------------------------

instance Textual AbsDir where
  textual = SeqConversions.fromList ⊳ (char '/' ⋫ endBy textual (char '/'))

instance Textual NonRootAbsDir where
  textual =
    fromSeqNE ∘ fromNonEmpty ⊳ (char '/' ⋫ endByNonEmpty textual (char '/'))

----------------------------------------

instance Arbitrary AbsDir where
  arbitrary = fromSeq ⊳ arbitrary
  shrink = fromSeq ⩺ shrink ∘ toSeq

instance Arbitrary NonRootAbsDir where
  arbitrary = fromSeqNE ⊳ arbitrary
  shrink = fromSeqNE ⩺ shrink ∘ toSeqNE

----------------------------------------

type instance DirType Abs = AbsDir

instance HasAbsOrRel AbsDir where
  type AbsOrRel AbsDir = Abs

instance HasAbsOrRel NonRootAbsDir where
  type AbsOrRel NonRootAbsDir = Abs

----------------------------------------

instance HasParent NonRootAbsDir where
  parent = lens getParent setParent
           where getParent ∷ NonRootAbsDir → AbsDir
                 getParent (SeqNE.unsnoc ∘ toSeqNE → (ps,_)) = fromSeq ps
                 setParent ∷ NonRootAbsDir → AbsDir → NonRootAbsDir
                 setParent (SeqNE.unsnoc ∘ toSeqNE → (_, p)) d = fromSeqNE (toSeq d ⫸ p)

----------------------------------------

nonAbsRootGetParentMay ∷ NonRootAbsDir → Maybe AbsDir
nonAbsRootGetParentMay = Just ∘ view parent


nonAbsRootSetParentMay  ∷ NonRootAbsDir → Maybe AbsDir → NonRootAbsDir
nonAbsRootSetParentMay (SeqNE.unsnoc ∘ toSeqNE → (_,p)) d =
  fromSeqNE $ (maybe ф toSeq d) ⫸ p

instance HasParentMay AbsDir where
  parentMay = lens getParentMay setParentMay
              where getParentMay ∷ AbsDir → Maybe AbsDir
                    getParentMay AbsRootDir                          = Nothing
                    getParentMay (AbsNonRootDir n) = nonAbsRootGetParentMay n

                    setParentMay ∷ AbsDir → Maybe AbsDir → AbsDir
                    setParentMay AbsRootDir (Just d) = d
                    setParentMay AbsRootDir Nothing  = AbsRootDir
                    setParentMay (AbsNonRootDir n) d =
                      AbsNonRootDir $ nonAbsRootSetParentMay n d

----------------------------------------

instance HasParentMay NonRootAbsDir where
  parentMay = lens nonAbsRootGetParentMay nonAbsRootSetParentMay

------------------------------------------------------------
--                     Quasi-Quoting                      --
------------------------------------------------------------

absdirT ∷ TypeRep
absdirT = typeRep (Proxy ∷ Proxy AbsDir)

nrabsdirT ∷ TypeRep
nrabsdirT = typeRep (Proxy ∷ Proxy NonRootAbsDir)

{- | try to parse a `Textual` as an absolute directory -}
parseAbsDir ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η AbsDir
parseAbsDir (toText → t) =
  let mkCompE ∷ (AsFPathError ε', MonadError ε' η') ⇒ FPathComponentError → η' α
      mkCompE ce = __FPathComponentE__ ce absdirT t
      eCompE ∷ (AsFPathError ε', MonadError ε' η') ⇒
               Either FPathComponentError α → η' α
      eCompE = either mkCompE return
   in case unsnoc $ splitOn "/" t of
        Nothing            → error "no happen: splitOn always returns something"
        Just (("":xs), "") → case NonEmpty.nonEmpty xs of
                               Nothing → return $ root
                               Just ys → do
                                 ps ← eCompE $ mapM parsePathC ys
                                 return $ absNonRootDir (fromNonEmpty ps)
          
        Just ([],"")      → __FPathEmptyE__ absdirT
        Just (("":_), _)  → __FPathNotADirE__ absdirT t

        _                 → __FPathNonAbsE__ absdirT t

parseAbsDir' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η AbsDir
parseAbsDir' = parseAbsDir

__parseAbsDir__ ∷ Printable τ ⇒ τ → AbsDir
__parseAbsDir__ = either __ERROR'__ id ∘ parseAbsDir'

__parseAbsDir'__ ∷ String → AbsDir
__parseAbsDir'__ = __parseAbsDir__

parseAbsDirN ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒
               τ → η NonRootAbsDir
parseAbsDirN t = do
--  d ← parseAbsDir t
  parseAbsDir t ≫ \ case
    AbsRootDir → __FPathAbsE__ nrabsdirT (toText t)
    AbsNonRootDir d' → return d'

parseAbsDirN' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η NonRootAbsDir
parseAbsDirN' = parseAbsDirN

__parseAbsDirN__ ∷ String → NonRootAbsDir
__parseAbsDirN__ s = case parseAbsDir' s of
                       Left e → __ERROR'__ e
                       Right AbsRootDir → __ERROR'__ $ FPathRootDirE nrabsdirT
                       Right (AbsNonRootDir nr) → nr

__parseAbsDirN'__ ∷ String → NonRootAbsDir
__parseAbsDirN'__ = __parseAbsDirN__

{- | quasi-quoter for AbsDir -}
absdir ∷ QuasiQuoter
absdir = mkQuasiQuoterExp "absdir" (\ s → ⟦ __parseAbsDir'__ s ⟧)

{- | quasi-quoter for NonRootAbsDir -}

absdirN ∷ QuasiQuoter
absdirN = mkQuasiQuoterExp "absdirN" (\ s → ⟦ __parseAbsDirN__ s ⟧)

----------------------------------------
--             constants              --
----------------------------------------

root ∷ AbsDir
root = AbsRootDir

-- that's all, folks! ----------------------------------------------------------
