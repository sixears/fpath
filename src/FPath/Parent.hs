{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UnicodeSyntax           #-}

{- | Classes for methods common to most all FPath types , such as `dirname` -}

module FPath.Parent
  ( HasParent( parent ), HasParentMay( parentMay, parents, parents' ) )
where

-- base --------------------------------

import Data.Function  ( ($) )
import Data.List      ( reverse )
import Data.Maybe     ( Maybe( Nothing, Just ) )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens' )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

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
  {- | Akin to `Data.List.inits`, this returns all the parent directories of
       a value.  Note that unlike `Data.List.inits`, the value itself is not
       returned.
   -}
  parents ∷ (DirType α ~ DirType (DirType α), HasParentMay (DirType α)) ⇒
            α → [DirType α]
  parents d = reverse $ _parents d

  {- | Like `parents`, but adds this dir to the end (thus more like
       `Data.List.inits`; at the expense of a more complex type-signature; and
       of course only works for directories.
   -}
  parents' ∷ (DirType α ~ α, DirType α ~ DirType (DirType α),
              HasParentMay α, HasParentMay (DirType α)) ⇒
             α → [DirType α]
  parents' d = reverse $ d : _parents d

{- | Implementation for `parents`, `parents'`. -}
_parents ∷ (HasParentMay α, HasParentMay (DirType α),
            DirType α ~ DirType (DirType α)) ⇒
           α → [DirType α]
_parents d = case d ⊣ parentMay of
                Just p → p : _parents p
                Nothing → []


-- that's all, folks! ----------------------------------------------------------
