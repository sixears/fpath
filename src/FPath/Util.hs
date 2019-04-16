{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.Util
  ( QuasiQuoter, (⋕), __ERROR__, __ERROR'__, mkQuasiQuoterExp, mkVisS, mkVisT )
where

import Prelude  ( error, mod )

-- base --------------------------------

import Data.Char      ( Char, ord )
import Data.Foldable  ( length )
import Data.Function  ( ($) )
import Data.Functor   ( fmap )
import Data.List      ( (!!), elem )
import Data.String    ( String )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.Review  ( AReview, re )

-- template-haskell --------------------

import Language.Haskell.TH        ( ExpQ )
import Language.Haskell.TH.Quote  ( QuasiQuoter( QuasiQuoter, quoteDec
                                               , quoteExp, quotePat, quoteType )
                                  )

-- text --------------------------------

import Data.Text  ( Text, pack, unpack )

--------------------------------------------------------------------------------

__ERROR__ ∷ Text → α
__ERROR__ = error ∘ toString

__ERROR'__ ∷ Printable ρ ⇒ ρ → α
__ERROR'__ = error ∘ toString

----------------------------------------

mkQuasiQuoterExp ∷ Text → (String → ExpQ) → QuasiQuoter
mkQuasiQuoterExp (toText → n) f = let notImpl u = __ERROR__ $ n ⊕ " " ⊕ u ⊕ " not implemented"
                        in QuasiQuoter { quoteDec  = notImpl "quoteDec"
                                       , quoteType = notImpl "quoteType"
                                       , quotePat  = notImpl "quotePat"
                                       , quoteExp = f
                                       }

----------------------------------------

(⋕) ∷ AReview t s → s → t
x ⋕ y = y ^. re x

----------------------------------------

{-| a list of 'visible' (easily distinguishable, and typed, characters,
    available in any font) -}
visChars ∷ [Char]
visChars = "abcdefghijklmnopqrstuvwxyz01234567890-"

{-| replace a character with one from a 'visible' list, if necessary -}
mkVisC ∷ Char → Char
mkVisC c = if c `elem` visChars
           then c
           else visChars !! (ord c `mod` length visChars)

mkVisS ∷ String → String
mkVisS = fmap mkVisC

mkVisT ∷ Text → Text
mkVisT = pack ∘ mkVisS ∘ unpack

-- that's all, folks! ----------------------------------------------------------
