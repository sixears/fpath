{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving        #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.RelDir
  ( RelDir

  -- quasi-quoters
  , reldir

  , parseRelDir , parseRelDir' , __parseRelDir__ , __parseRelDir'__
  )
where

import Prelude  ( error, undefined )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( mapM, return )
import Data.Bool            ( otherwise )
import Data.Either          ( Either, either )
import Data.Eq              ( Eq )
import Data.Foldable        ( concat )
import Data.Function        ( ($), id )
import Data.Functor         ( fmap )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Monoid          ( Monoid )
import Data.Semigroup       ( Semigroup )
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

import NonEmptyContainers.SeqConversions    ( FromMonoSeq( fromSeq )
                                            , IsMonoSeq( seq )
                                            , ToMonoSeq( toSeq )
                                            )
import NonEmptyContainers.SeqNE             ( pattern (:⪭), (⪫) )
import NonEmptyContainers.SeqNEConversions  ( FromMonoSeqNonEmpty( fromSeqNE ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char, string )
import Text.Parser.Combinators  ( endBy )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- text --------------------------------

import Data.Text  ( splitOn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath   ( AsFilePath( filepath ) )
import FPath.DirType      ( DirType )
import FPath.HasAbsOrRel  ( HasAbsOrRel( AbsOrRel ), Rel )
import FPath.HasParent    ( HasParentMay( parentMay ) )

import FPath.Error.FPathComponentError  ( FPathComponentError )
import FPath.Error.FPathError           ( AsFPathError, FPathError
                                        , __FPathComponentE__, __FPathEmptyE__
                                        , __FPathAbsE__, __FPathNotADirE__
                                        )
import FPath.PathComponent              ( PathComponent, parsePathC )
import FPath.Util                       ( QuasiQuoter
                                        , __ERROR'__, mkQuasiQuoterExp )

-------------------------------------------------------------------------------

{- | a relative directory -}
newtype RelDir = RelDir (Seq PathComponent)
  deriving (Eq, Monoid, Semigroup, Show)

type instance Element RelDir = PathComponent

----------------------------------------

instance MonoFunctor RelDir where
  omap ∷ (PathComponent → PathComponent) → RelDir → RelDir
  omap f (RelDir ps) = RelDir (omap f ps)

----------------------------------------

instance MonoFoldable RelDir where
  otoList ∷ RelDir → [PathComponent]
  otoList (RelDir ps) = toList ps
  ofoldl' ∷ (α → PathComponent → α) → α → RelDir → α 
  ofoldl' = undefined

  ofoldr ∷ (PathComponent → α → α) → α → RelDir → α
  ofoldr = undefined
  ofoldMap ∷ Monoid ν => (PathComponent → ν) → RelDir → ν
  ofoldMap = undefined
  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → RelDir
            → PathComponent
  ofoldr1Ex = undefined
  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → RelDir
             → PathComponent
  ofoldl1Ex' = undefined

----------------------------------------

instance FromMonoSeqNonEmpty RelDir where
  fromSeqNE = RelDir ∘ SeqNE.toSeq

----------------------------------------

instance FromMonoSeq RelDir where
  fromSeq = RelDir

----------------------------------------

instance ToMonoSeq RelDir where
  toSeq (RelDir ps) = ps

----------------------------------------

instance IsMonoSeq RelDir where
  seq = iso toSeq fromSeq

----------------------------------------

instance IsList RelDir where
  type instance Item RelDir = PathComponent
  fromList = RelDir ∘ Seq.fromList
  toList   = toList ∘ toSeq

----------------------------------------

{- | Convert a sequence of printable elements to strings by intercalating '/'
     characters; with a post-fact string transformation for extra bells (e.g.,
     a prefix '/' character -}
pDir ∷ (P.Printer ρ, ToMonoSeq α, Printable (Element α)) ⇒
       (String → String) → α → ρ
pDir f =  P.string ∘ f ∘ concat ∘ fmap ((⊕ "/") ∘ toString) ∘ toSeq

instance Printable RelDir where
  print (RelDir ps) | ps ≡ ф = "./"
                    | otherwise = pDir id ps

----------------------------------------

instance AsFilePath RelDir where
  filepath = prism' toString fromString

----------------------------------------

instance Textual RelDir where
  textual = return (fromList []) ⋪ (string "./")
          ∤ fromList ⊳ (endBy textual (char '/'))

----------------------------------------

instance Arbitrary RelDir where
  arbitrary = fromSeq ⊳ arbitrary
  shrink = fromSeq ⩺ shrink ∘ toSeq

----------------------------------------

type instance DirType Rel = RelDir

instance HasAbsOrRel RelDir where
  type AbsOrRel RelDir = Rel

----------------------------------------

instance HasParentMay RelDir where
  parentMay = lens getParentMay setParentMay
              where getParentMay (RelDir (p :⪭ _)) = Just $ RelDir p
                    getParentMay (RelDir _)        =  Nothing

                    setParentMay orig par =
                      case orig of
                        RelDir (_ :⪭ d) → case par of
                                            Just (RelDir p) → RelDir $ p ⪫ d
                                            Nothing         → RelDir $ pure d
                        RelDir _ → case par of
                                            Just r → r
                                            Nothing → RelDir Seq.Empty

------------------------------------------------------------
--                     Quasi-Quoting                      --
------------------------------------------------------------

reldirT ∷ TypeRep
reldirT = typeRep (Proxy ∷ Proxy RelDir)

{- | try to parse a `Textual` as a relative directory -}
parseRelDir ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η RelDir
parseRelDir (toText → t) =
  case unsnoc $ splitOn "/" t of
    Nothing           → error "cannot happen: splitOn always returns something"
    Just (("":_), _)  → __FPathAbsE__ reldirT t
    Just ([],"")      → __FPathEmptyE__ reldirT
    Just (["."],"")   → return $ RelDir ф
    Just ((x:xs), "") → do let mkCompE ∷ (AsFPathError ε', MonadError ε' η') ⇒
                                       FPathComponentError → η' α
                               mkCompE ce = __FPathComponentE__ ce reldirT t
                               eCompE ∷ (AsFPathError ε'', MonadError ε'' η'') ⇒
                                        Either FPathComponentError α → η'' α
                               eCompE = either mkCompE return

                           p  ← eCompE $ parsePathC x
                           ps ← eCompE $ mapM parsePathC xs
                           return $ RelDir (p <| Seq.fromList ps)
    _                 → __FPathNotADirE__ reldirT t


parseRelDir' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η RelDir
parseRelDir' = parseRelDir

__parseRelDir__ ∷ Printable τ ⇒ τ → RelDir
__parseRelDir__ = either __ERROR'__ id ∘ parseRelDir'

__parseRelDir'__ ∷ String → RelDir
__parseRelDir'__ = __parseRelDir__

{- | quasi-quotation -}
reldir ∷ QuasiQuoter
reldir = mkQuasiQuoterExp "reldir" (\ s → ⟦ __parseRelDir'__ s ⟧)

-- that's all, folks! ----------------------------------------------------------
