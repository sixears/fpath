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

import Prelude  ( error, undefined )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( mapM, return )
import Data.Bifunctor       ( first )
import Data.Bool            ( otherwise )
import Data.Either          ( Either, either )
import Data.Eq              ( Eq )
import Data.Foldable        ( concat )
import Data.Function        ( ($), const, id )
import Data.Functor         ( fmap )
import Data.Maybe           ( Maybe( Just, Nothing ) )
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

import Data.MonoTraversable  ( Element, MonoFunctor( omap ) )

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

import Data.Text  ( Text, dropEnd, length, splitOn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath   ( AsFilePath( filepath ) )
import FPath.HasAbsOrRel  ( HasAbsOrRel( AbsOrRel ), Rel )
import FPath.HasParent    ( HasParentMay( parentMay ) )

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
--  omap f (RelFile ps) = RelFile (omap f ps)
  omap = undefined

----------------------------------------

instance FromMonoSeqNonEmpty RelFile where
  fromSeqNE (ps :⪭ f) = RelFile (fromSeq ps) f

----------------------------------------

instance FromMonoSeq RelFile where
--  fromSeq = RelFile

----------------------------------------

instance ToMonoSeq RelFile where
--  toSeq (RelFile ps) = ps

----------------------------------------

instance IsMonoSeq RelFile where
  seq = iso toSeq fromSeq

----------------------------------------

instance IsList RelFile where
  type instance Item RelFile = PathComponent
{-
  fromList = RelFile ∘ Seq.fromList
  toList   = toList ∘ toSeq
-}

----------------------------------------

{- | Convert a sequence of printable elements to strings by intercalating '/'
     characters; with a post-fact string transformation for extra bells (e.g.,
     a prefix '/' character -}
pDir ∷ (P.Printer ρ, ToMonoSeq α, Printable (Element α)) ⇒
       (String → String) → α → ρ
pDir f =  P.string ∘ f ∘ concat ∘ fmap ((⊕ "/") ∘ toString) ∘ toSeq

instance Printable RelFile where
{-
  print (RelFile ps) | ps ≡ ф = "./"
                     | otherwise = pDir id ps
-}

----------------------------------------

instance AsFilePath RelFile where
  filepath = prism' toString fromString

----------------------------------------

instance Textual RelFile where
  textual = return (fromList []) ⋪ (string "./")
          ∤ fromList ⊳ (endBy textual (char '/'))

----------------------------------------

instance Arbitrary RelFile where
  arbitrary = fromSeq ⊳ arbitrary
  shrink = fromSeq ⩺ shrink ∘ toSeq

----------------------------------------

instance HasAbsOrRel RelFile where
  type AbsOrRel RelFile = Rel

----------------------------------------

instance HasParentMay RelFile where
  parentMay = undefined
          {- lens getParentMay setParentMay
              where getParentMay (RelFile (p :⪭ _)) = Just $ RelFile p
                    getParentMay (RelFile _)        =  Nothing

                    setParentMay orig par =
                      case orig of
                        RelFile (_ :⪭ d) → case par of
                                            Just (RelFile p) → RelFile $ p ⪫ d
                                            Nothing         → RelFile $ pure d
                        RelFile _ → case par of
                                            Just r → r
                                            Nothing → RelFile Seq.Empty
            -}

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
