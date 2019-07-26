{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module NonEmptyContainers.SeqConversions
  ( AsMonoSeq( seq' ), FromMonoSeq( fromList, fromSeq ), IsMonoSeq( seq )
  , ToMonoSeq( toSeq ) )
where


-- base --------------------------------

import Data.Function  ( id )
import Data.Maybe     ( Maybe( Just ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

import Data.Sequence  ( Seq )

-- lens --------------------------------

import Control.Lens.Iso    ( Iso' )
import Control.Lens.Prism  ( Prism', prism' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  NonEmptyContainers.SeqNE  as  SeqNE

import NonEmptyContainers.SeqNE  ( SeqNE )

--------------------------------------------------------------------------------

----------------------------------------
--            FromMonoSeq             --
----------------------------------------

{- | α that may be constructed from a (empty?) Sequence of `Element α` -}
class FromMonoSeq α where
  fromSeq ∷ Seq (Element α) → α
  fromList ∷ [Element α] → α
  fromList = fromSeq ∘ Seq.fromList

----------------------------------------
--             ToMonoSeq              --
----------------------------------------

{- | α that may be converted to a (empty?) Sequence of `Element α` -}
class ToMonoSeq α where
  toSeq ∷ α → Seq (Element α)

instance α ~ Element (SeqNE α) ⇒ ToMonoSeq (SeqNE α) where
  toSeq = SeqNE.toSeq

instance α ~ Element (Seq α) ⇒ ToMonoSeq (Seq α) where
  toSeq = id

----------------------------------------
--             IsMonoSeq              --
----------------------------------------

{- | α that are isomorphic to a (empty?) Sequence of `Element α` -}
class IsMonoSeq α where
  seq ∷ Iso' α (Seq (Element α))

----------------------------------------
--             AsMonoSeq              --
----------------------------------------

{- | α that may be representable by a non-empty Sequence of `Element α` -}
class AsMonoSeq α where
  seq' ∷ Prism' α (Seq (Element α))

instance α ~ Element (Seq α) ⇒ AsMonoSeq (Seq α) where
  seq' = prism' id Just

-- that's all, folks! ----------------------------------------------------------
