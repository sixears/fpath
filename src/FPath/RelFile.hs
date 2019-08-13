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

module FPath.RelFile
  ( RelDir, RelFile

  -- quasi-quoters
  , relfile

  , parseRelFile , parseRelFile' , __parseRelFile__ , __parseRelFile'__
  )
where

import Prelude  ( error )

-- base --------------------------------

import qualified  Data.List.NonEmpty  as  NonEmpty

import Control.Applicative  ( pure )
import Control.Monad        ( mapM, return )
import Data.Bifunctor       ( first )
import Data.Bool            ( otherwise )
import Data.Either          ( Either, either )
import Data.Eq              ( Eq )
import Data.Foldable        ( concat, foldMap, foldl1, foldl', foldr, foldr1 )
import Data.Function        ( ($), const, id )
import Data.Functor         ( fmap )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Monoid          ( Monoid )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Exts             ( IsList( fromList, toList ), Item )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

import Data.Sequence  ( Seq, (<|) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

-- lens --------------------------------

import Control.Lens.Cons   ( unsnoc )
import Control.Lens.Iso    ( iso )
import Control.Lens.Lens   ( lens )
import Control.Lens.Prism  ( prism' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFoldable( ofoldl', ofoldl1Ex'
                                                    , ofoldMap, ofoldr
                                                    , ofoldr1Ex, otoList )
                             , MonoFunctor( omap )
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤), (⋪) )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Monoid       ( ф )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import qualified  NonEmptyContainers.SeqNE  as  SeqNE

import NonEmptyContainers.IsNonEmpty        ( FromNonEmpty( fromNonEmpty )
                                            , IsNonEmpty( nonEmpty )
                                            , ToNonEmpty( toNonEmpty ) )
import NonEmptyContainers.SeqConversions    ( FromMonoSeq( fromSeq )
                                            , IsMonoSeq( seq )
                                            , ToMonoSeq( toSeq )
                                            )
import NonEmptyContainers.SeqNE             ( pattern (:⪭), (⪫), (⪭) )
import NonEmptyContainers.SeqNEConversions  ( FromMonoSeqNonEmpty( fromSeqNE )
                                            , IsMonoSeqNonEmpty( seqNE )
                                            , ToMonoSeqNonEmpty( toSeqNE
                                                               , toSeq_ )
                                            )

-- parsers -----------------------------

import Text.Parser.Char         ( char, string )
import Text.Parser.Combinators  ( endBy, sepByNonEmpty )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- text --------------------------------

import Data.Text  ( Text, dropEnd, intercalate, length, splitOn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath   ( AsFilePath( filepath ) )
import FPath.HasAbsOrRel  ( HasAbsOrRel( AbsOrRel ), Rel )
import FPath.HasParent    ( HasParent( parent ), HasParentMay( parentMay ) )

import FPath.Error.FPathComponentError  ( FPathComponentError )
import FPath.Error.FPathError           ( AsFPathError, FPathError
                                        , __FPathComponentE__, __FPathEmptyE__
                                        , __FPathAbsE__, __FPathNotAFileE__
                                        , mapTypeRepE, mapTextE
                                        )
import FPath.PathComponent              ( PathComponent, parsePathC )
import FPath.RelDir                     ( RelDir, parseRelDir )
import FPath.Util                       ( QuasiQuoter
                                        , __ERROR'__, mkQuasiQuoterExp )

-------------------------------------------------------------------------------

{- | a relative directory -}
data RelFile = RelFile RelDir PathComponent
  deriving (Eq, Show)

type instance Element RelFile = PathComponent

----------------------------------------

instance MonoFunctor RelFile where
  omap ∷ (PathComponent → PathComponent) → RelFile → RelFile
  omap f (RelFile ps b) = RelFile (omap f ps) (f b)

----------------------------------------

instance MonoFoldable RelFile where
  otoList ∷ RelFile → [PathComponent]
  otoList (RelFile ps f) = otoList ps ⊕ [f]

  ofoldl' ∷ (α → PathComponent → α) → α → RelFile → α 
  ofoldl' f x r = foldl' f x (toNonEmpty r)

  ofoldr ∷ (PathComponent → α → α) → α → RelFile → α
  ofoldr f x r = foldr f x (toNonEmpty r)

  ofoldMap ∷ Monoid ν => (PathComponent → ν) → RelFile → ν
  ofoldMap f r = foldMap f (toNonEmpty r)

  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → RelFile
            → PathComponent
  ofoldr1Ex f r = foldr1 f (toNonEmpty r)

  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → RelFile
             → PathComponent
  ofoldl1Ex' f r = foldl1 f (toNonEmpty r)

----------------------------------------

instance FromMonoSeqNonEmpty RelFile where
  fromSeqNE (ps :⪭ f) = RelFile (fromSeq ps) f

----------------------------------------

-- instance FromMonoSeq RelFile where
--  fromSeq = RelFile

----------------------------------------

instance ToMonoSeqNonEmpty RelFile where
  toSeqNE (RelFile ps f) = toSeq ps ⪭ f

instance ToMonoSeq RelFile where
  toSeq = toSeq_

----------------------------------------

instance IsMonoSeqNonEmpty RelFile where
  seqNE = iso toSeqNE fromSeqNE

----------------------------------------

instance FromNonEmpty RelFile where
  fromNonEmpty (x :| xs) = case NonEmpty.nonEmpty xs of
                             Nothing  → RelFile (fromList ф) x
                             Just xs' → let pcs = x : NonEmpty.init xs'
                                            f   = NonEmpty.last xs'
                                         in RelFile (fromList pcs) f

instance ToNonEmpty RelFile where
  toNonEmpty (RelFile ps f) = NonEmpty.fromList $ toList ps ⊕ [f]

instance IsNonEmpty RelFile where
  nonEmpty = iso toNonEmpty fromNonEmpty

----------------------------------------

instance Printable RelFile where
  print r = P.text $ intercalate "/" (toText ⊳ otoList r)

----------------------------------------

instance AsFilePath RelFile where
  filepath = prism' toString fromString

----------------------------------------

instance Textual RelFile where
  textual = fromNonEmpty ⊳ (sepByNonEmpty textual (char '/'))

----------------------------------------

instance Arbitrary RelFile where
  arbitrary = fromSeqNE ⊳ arbitrary
  shrink = fromSeqNE ⩺ shrink ∘ toSeqNE

----------------------------------------

instance HasAbsOrRel RelFile where
  type AbsOrRel RelFile = Rel

----------------------------------------

instance HasParent RelFile where
  parent = lens (\ (RelFile p _) → p) (\ (RelFile _ f) p → RelFile p f)

----------------------------------------

instance HasParentMay RelFile where
  parentMay = lens (\ (RelFile p _) → Just p)
                   (\ (RelFile _ f) md → case md of
                                           Just  d → RelFile d f
                                           Nothing → RelFile ф f
                   )

------------------------------------------------------------
--                     Quasi-Quoting                      --
------------------------------------------------------------

relfileT ∷ TypeRep
relfileT = typeRep (Proxy ∷ Proxy RelFile)

{- | try to parse a `Textual` as a relative directory -}
parseRelFile ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η RelFile
parseRelFile (toText → t) =
  let mkCompE    ∷ (AsFPathError ε', MonadError ε' η') ⇒
                   FPathComponentError → η' α
      mkCompE ce = __FPathComponentE__ ce relfileT t
      eCompE     ∷ (AsFPathError ε'', MonadError ε'' η'') ⇒
                   Either FPathComponentError α → η'' α
      eCompE = either mkCompE return
      -- map the type marker on FPathError
      mapFPCE ∷ (AsFPathError ε', MonadError ε' η') ⇒
                (Text → Text) → Either FPathError α → η' α
      mapFPCE f = mapTextE f ∘ mapTypeRepE (const relfileT)
   in case unsnoc $ splitOn "/" t of
        Nothing           → error "error: splitOn always returns something"
        Just ([],"")      → __FPathEmptyE__ relfileT
        Just (_, "")      → __FPathNotAFileE__ relfileT t
        Just (("":_), _)  → __FPathAbsE__ relfileT t
        Just ([], f)      → do f' ← eCompE $ parsePathC f
                               return $ RelFile (fromSeq ф) f'
        Just (_, f)       → do f' ← eCompE $ parsePathC f
                               ps' ← mapFPCE (⊕ f) $ parseRelDir (dropEnd (length f) t)
                               return $ RelFile ps' f'
{-
    Just (["."],"")   → return $ RelFile ф
    Just ((x:xs), "") → do let mkCompE ∷ (AsFPathError ε', MonadError ε' η') ⇒
                                       FPathComponentError → η' α
                               mkCompE ce = __FPathComponentE__ ce relfileT t
                               eCompE ∷ (AsFPathError ε'', MonadError ε'' η'') ⇒
                                        Either FPathComponentError α → η'' α
                               eCompE = either mkCompE return

                           p  ← eCompE $ parsePathC x
                           ps ← eCompE $ mapM parsePathC xs
                           return $ RelFile (p <| Seq.fromList ps)
    _                 → __FPathNotADirE__ relfileT t
-}

parseRelFile' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η RelFile
parseRelFile' = parseRelFile

__parseRelFile__ ∷ Printable τ ⇒ τ → RelFile
__parseRelFile__ = either __ERROR'__ id ∘ parseRelFile'

__parseRelFile'__ ∷ String → RelFile
__parseRelFile'__ = __parseRelFile__

{- | quasi-quotation -}
relfile ∷ QuasiQuoter
relfile = mkQuasiQuoterExp "relfile" (\ s → ⟦ __parseRelFile'__ s ⟧)

-- that's all, folks! ----------------------------------------------------------
