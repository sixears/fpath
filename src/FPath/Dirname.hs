{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | Classes for methods common to most all FPath types , such as `dirname` -}

module FPath.Dirname
  ( Ancestors(..), HasDirname(..) )
where

-- base --------------------------------

import Data.List.NonEmpty  ( NonEmpty )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.DirType  ( DirTypeC( DirType ) )

--------------------------------------------------------------------------------

class DirTypeC α ⇒ HasDirname α where
  {- | Well-typed version of `dirname` utility; note that `dirname` of "/" is
       "/", and `dirname` of "./" is "./". -}
  dirname ∷ Lens' α (DirType α)

  {- | The dirname of this thing, and it's dirname, and… up to the root (for
       abs things) or `./` (for relative things).  Note that this does *not*
       include the thing itself - so ancestors of `/etc` is `[/]`; and ancestors
       of `/` is `[]`.
   -}
  ancestors' ∷ α → [DirType α]

class DirTypeC α ⇒ Ancestors α where
  {- | The dirname of this thing, and it's dirname, and… up to the root (for
       abs things) or `./` (for relative things).  Note that this does *not*
       include the thing itself - so ancestors of `/etc` is `[/]`.  The type of
       the return is `NonEmpty (DirType α)` - so this is only applicable to
       things that have at least one ancestor (thus, files & NonRootAbsDir; dirs
       in general may lack an ancestor, `/` and `./` being cases in point.
   -}
  ancestors ∷ α → NonEmpty (DirType α)

-- that's all, folks! ----------------------------------------------------------
