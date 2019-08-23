{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.Fileish
  ( Fileish( FDirType, (⊙), (<.>), addExt, dir, dirfile, ext, file, splitExt ) )
where

-- base --------------------------------

import Data.Function  ( (&) )
import Data.Maybe     ( Maybe( Just, Nothing ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- lens --------------------------------

import Control.Lens.Iso    ( Iso', iso )
import Control.Lens.Lens   ( Lens', lens )
import Control.Lens.Tuple  ( _1, _2 )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens       ( (⊣), (⫣), (⊢) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified FPath.PathComponent  as  PathComponent

import FPath.PathComponent  ( PathComponent )

--------------------------------------------------------------------------------

{- | A 'file-like' data-type, e.g., `RelFile` or `AbsFile` - or even
     `PathComponent`.  Importantly, a `Fileish` is isomorphic with a File
     (`PathComponent`) and some other opaque thing (its Directory).
 -}
class Fileish α where
  type FDirType α

  {- | Split/Reform a `Fileish` into its directory & file components -}
  dirfile ∷ Iso' α (FDirType α, PathComponent)

  file ∷ Lens' α PathComponent
  file = lens (⊣ (dirfile ∘ _2)) (\ a c → (a ⊣ (dirfile ∘ _1),c) ⫣ dirfile)

  dir ∷ Lens' α (FDirType α)
  dir = lens (⊣ (dirfile ∘ _1)) (\ a d → (d, a ⊣ (dirfile ∘ _2)) ⫣ dirfile)

  {- | Add an "extension", that is, join two `PathComponent`s with a '.'
       character
   -}
  addExt ∷ α → PathComponent → α
  addExt a c = let (d,f) = a ⊣ dirfile
                in (d,f `addExt` c) ⫣ dirfile


  {- | operator alias for `addExt` -}
  infixr 6 <.> -- same as for ⊕
  (<.>) ∷ α → PathComponent → α
  (<.>) = addExt

  {- | operator alias for `addExt` -}
  infixr 6 ⊙ -- same as for ⊕
  (⊙) ∷ α → PathComponent → α
  (⊙) = (<.>)

  {- | Split an "extension" - the maximal non-empty sequence of characters,
       excluding '.' - from the end of a `PathComponent`; if there is one.
  -}
  splitExt ∷ α → Maybe (α, PathComponent)
  splitExt a = let (d,f) = a ⊣ dirfile
                in case splitExt f of
                     Just (b,e) → Just ((d,b) ⫣ dirfile, e)
                     Nothing    → Nothing

  {- | A badly-behaved lens onto "file extension"; being a sequence of 1 or more
       non-'.' characters at the end of a filename, after a '.' character.

       This is badly-behaved because for some `PathComponent` `f`, `f'` being `f`
       with `ext` set to `Nothing`, may yield a non-nothing extension (if f had
       two "extensions"; e.g., `"foo.bar.baz"`).
   -}
  ext ∷ Lens' α (Maybe PathComponent)
  ext = lens getter setter
             where getter a   = let (_,f) = a ⊣ dirfile
                                 in f ⊣ ext
                   setter a c = let (d,f) = a ⊣ dirfile
                                 in ((d,f & ext ⊢ c) ⫣ dirfile)
                     
instance Fileish PathComponent where
  type FDirType PathComponent = ()

  dirfile ∷ Iso' PathComponent ((), PathComponent)
  dirfile = iso ((),) (\ ((),p) → p)

  addExt ∷ PathComponent → PathComponent → PathComponent
  addExt = PathComponent.addExt

  splitExt ∷ PathComponent → Maybe (PathComponent, PathComponent)
  splitExt = PathComponent.splitExt

  ext ∷ Lens' PathComponent (Maybe PathComponent)
  ext = PathComponent.ext

-- that's all, folks! ----------------------------------------------------------
