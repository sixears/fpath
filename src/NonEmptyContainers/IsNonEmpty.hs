{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module NonEmptyContainers.IsNonEmpty
  ( FromNonEmpty(..), IsNonEmpty(..), ToNonEmpty(..), defaultNonEmpty )
where

-- base --------------------------------

import Data.Function       ( id )
import Data.List.NonEmpty  ( NonEmpty )

-- lens --------------------------------

import Control.Lens.Iso   ( Iso', iso )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

class FromNonEmpty α where
  fromNonEmpty ∷ NonEmpty (Element α) → α

instance FromNonEmpty (NonEmpty α) where
  fromNonEmpty = id

------------------------------------------------------------

class ToNonEmpty α where
  toNonEmpty ∷ α → NonEmpty (Element α)

instance ToNonEmpty (NonEmpty α) where
  toNonEmpty = id

------------------------------------------------------------

class IsNonEmpty α where
  nonEmpty ∷ Iso' α (NonEmpty (Element α))

{- | pre-made "instance" of `IsNonEmpty` for instances of
    `FromNonEmpty` & `ToNonEmpty` -}
defaultNonEmpty ∷ (FromNonEmpty α, ToNonEmpty α) ⇒ Iso' α (NonEmpty (Element α))
defaultNonEmpty = iso toNonEmpty fromNonEmpty

instance IsNonEmpty (NonEmpty α) where
  nonEmpty = defaultNonEmpty

-- that's all, folks! ----------------------------------------------------------
