{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | Classes for methods common to most all FPath types , such as `dirname` -}

module FPath.Parent
  ( HasParent( parent ), HasParentMay( parentMay ) )
where

-- base --------------------------------

import Data.Maybe  ( Maybe )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.DirType        ( DirTypeC( DirType ) )

--------------------------------------------------------------------------------

class DirTypeC α ⇒ HasParent α where
  {- | The parent directory of a thing.  Note that this is stricter than
       `dirname`; e.g., `root` or `"./"` don't have a `parent` (at the type
       level), though they are their own `dirname`s.
   -}
  parent ∷ Lens' α (DirType α)

class DirTypeC α ⇒ HasParentMay α where
  {- | Like `parent`, but applicable to types (e.g., `AbsDir`, `RelDir`) where
       the value might have no parent (e.g., root or `"./"`).
   -}
  parentMay ∷ Lens' α (Maybe (DirType α))

-- that's all, folks! ----------------------------------------------------------
