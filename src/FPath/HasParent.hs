{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.HasParent
  ( HasMaybeParent(..) )
where

-- base --------------------------------

import Data.Maybe  ( Maybe )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.DirType      ( DirType )
import FPath.HasAbsOrRel  ( HasAbsOrRel( AbsOrRel ) )

--------------------------------------------------------------------------------

class HasAbsOrRel α ⇒ HasMaybeParent α where
  getParentMay ∷ α → Maybe (DirType (AbsOrRel α))
  setParentMay ∷ α → Maybe (DirType (AbsOrRel α)) → α
  parentMay ∷ Lens' α (Maybe (DirType (AbsOrRel α)))
  parentMay = lens getParentMay setParentMay

-- that's all, folks! ----------------------------------------------------------
