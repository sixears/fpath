{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.AbsOrRel
  ( AbsOrRel( Abs, Rel ), HasAbsOrRel( absOrRel ), isAbs, isRel )
where

-- base --------------------------------

import Data.Bool  ( Bool )
import Data.Eq    ( Eq )
import Text.Show  ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

data AbsOrRel = Abs | Rel
  deriving (Eq,Show)

class HasAbsOrRel α where
  absOrRel ∷ α → AbsOrRel
  isAbs ∷ α → Bool
  isAbs = (≡ Abs) ∘ absOrRel
  isRel ∷ α → Bool
  isRel = (≡ Rel) ∘ absOrRel

-- that's all, folks! ----------------------------------------------------------
