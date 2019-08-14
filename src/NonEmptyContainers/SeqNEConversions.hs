{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module NonEmptyContainers.SeqNEConversions
  ( AsMonoSeqNonEmpty( seqNE' ), FromMonoSeqNonEmpty( fromSeqNE )
  , IsMonoSeqNonEmpty( seqNE ), ToMonoSeqNonEmpty( toSeqNE, toSeq_ ) )
where

-- base --------------------------------

import Data.Function  ( id )
import Data.Maybe     ( Maybe( Just ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- containers --------------------------

import Data.Sequence  ( Seq )

-- lens --------------------------------

import Control.Lens.Iso    ( Iso' )
import Control.Lens.Prism  ( Prism', prism' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )
import Data.NonNull          ( NonNull, toNullable )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  NonEmptyContainers.SeqNE  as  SeqNE

import NonEmptyContainers.SeqNE  ( SeqNE )

--------------------------------------------------------------------------------

----------------------------------------
--        FromMonoSeqNonEmpty         --
----------------------------------------

{- | α that may be constructed from a non-empty Sequence of `Element α` -}
class FromMonoSeqNonEmpty α where
  fromSeqNE ∷ SeqNE (Element α) → α

instance α ~ Element (SeqNE α) ⇒ FromMonoSeqNonEmpty (SeqNE α) where
  fromSeqNE = id

instance α ~ Element (SeqNE α) ⇒ FromMonoSeqNonEmpty (Seq α) where
  fromSeqNE = SeqNE.toSeq

----------------------------------------
--         ToMonoSeqNonEmpty          --
----------------------------------------

{- | α that may be converted to a non-empty Sequence of `Element α` -}
class ToMonoSeqNonEmpty α where
  toSeqNE ∷ α → SeqNE (Element α)
  {- | I'd really like to say "instance ToMonoSeqNonEmpty α ⇒ ToMonoSeq α",
       but this falls foul of the "constraint is no smaller than the instance
       head" issue.  So we provide this fn, in the expectation that instances of
       `ToMonoSeqNonEmpty` will also define instances as of `ToMonoSeq` with
       @toSeq = toSeq_@
   -}
  toSeq_ ∷ α → Seq (Element α)
  toSeq_ = SeqNE.toSeq ∘ toSeqNE

instance α ~ Element (SeqNE α) ⇒ ToMonoSeqNonEmpty (SeqNE α) where
  toSeqNE = id
  toSeq_  = SeqNE.toSeq

instance α ~ Element (SeqNE α) ⇒ ToMonoSeqNonEmpty (NonNull (Seq α)) where
  toSeqNE = SeqNE.fromNonNullSeq
  toSeq_  = toNullable



----------------------------------------
--         IsMonoSeqNonEmpty          --
----------------------------------------

{- | α that are isomorphic to a non-empty Sequence of `Element α` -}
class IsMonoSeqNonEmpty α where
  seqNE ∷ Iso' α (SeqNE (Element α))

----------------------------------------
--         AsMonoSeqNonEmpty          --
----------------------------------------

{- | α that may be representable by a non-empty Sequence of `Element α` -}
class AsMonoSeqNonEmpty α where
  seqNE' ∷ Prism' α (SeqNE (Element α))

instance α ~ Element (SeqNE α) ⇒ AsMonoSeqNonEmpty (SeqNE α) where
  seqNE' = prism' id Just

-- that's all, folks! ----------------------------------------------------------
