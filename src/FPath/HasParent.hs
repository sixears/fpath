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

import FPath.DirType      ( HasDirType( DirType ) )

--------------------------------------------------------------------------------

------------------------------------------------------------
--                       HasParent                        --
------------------------------------------------------------

class HasDirType α ⇒ HasParent α where
  parent ∷ Lens' α (DirType α)

------------------------------------------------------------
--                    HasMaybeParent                      --
------------------------------------------------------------

class HasDirType α ⇒ HasParentMay α where
  parentMay ∷ Lens' α (Maybe (DirType α))

-- that's all, folks! ----------------------------------------------------------
