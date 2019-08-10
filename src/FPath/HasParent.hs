{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.HasParent
  ( HasParentMay(..), HasParent(..) )
where

-- base --------------------------------

import Data.Maybe  ( Maybe )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.DirType      ( DirType )
import FPath.HasAbsOrRel  ( HasAbsOrRel( AbsOrRel ) )

--------------------------------------------------------------------------------

------------------------------------------------------------
--                       HasParent                        --
------------------------------------------------------------

class HasAbsOrRel α ⇒ HasParent α where
  parent ∷ Lens' α (DirType (AbsOrRel α))

------------------------------------------------------------
--                    HasMaybeParent                      --
------------------------------------------------------------

class HasAbsOrRel α ⇒ HasParentMay α where
  parentMay ∷ Lens' α (Maybe (DirType (AbsOrRel α)))

-- that's all, folks! ----------------------------------------------------------
