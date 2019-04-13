{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.Util
  ( QuasiQuoter, (⋕), __ERROR__, __ERROR'__, mkQuasiQuoterExp )
where

import Prelude  ( error )

-- base --------------------------------

import Data.Function  ( ($) )
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

import Data.Text  ( Text )

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

-- that's all, folks! ----------------------------------------------------------
