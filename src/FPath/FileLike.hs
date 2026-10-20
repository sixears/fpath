{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-| Things that act like files; specifically can be split into a dir, a file
    part and maybe an extension. Examples are `PathComponent` (dir part is
    `()`), `AbsFile`, `RelFile` and `File`.
 -}
module FPath.FileLike
  ( FileLike( (⊙), (<.>)
            , addExt, dir, dirfile, ext, file, setExt, split, splitExt
            , updateExt )
  )
where

import Base1T

-- base-plus ---------------------------

import BasePlus.New  ( new )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( Iso', iso )
import Control.Lens.Review  ( re )
import Control.Lens.Tuple   ( _1, _2 )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⫣) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified FPath.PathComponent  as  PathComponent

import FPath.DirType            ( DirTypeC( DirType ) )
import FPath.FilenameExtension  ( FilenameExtension
                                , HasFilenameExtension( filenameExtensionPC ) )
import FPath.PathComponent      ( PathComponent, pathComponent' )

--------------------------------------------------------------------------------

{- | A 'file-like' data-type, e.g., `RelFile` or `AbsFile` - or even
     `PathComponent`.  Importantly, a `FileLike` is isomorphic with a File
     (`PathComponent`) and some other opaque thing (its Directory).
 -}
class (Printable α, DirTypeC α) => FileLike α where
  {- | Split/Reform a `FileLike` into its directory & file components -}
  dirfile ∷ Iso' α (DirType α, PathComponent)

  file ∷ Lens' α PathComponent
  file = lens (⊣ (dirfile ∘ _2)) (\ a c → (a ⊣ (dirfile ∘ _1),c) ⫣ dirfile)

  dir ∷ Lens' α (DirType α)
  dir = lens (⊣ (dirfile ∘ _1)) (\ a d → (d, a ⊣ (dirfile ∘ _2)) ⫣ dirfile)

  {- | Add an "extension", that is, join two `PathComponent`s with a '.'
       character
   -}
  addExt ∷ HasFilenameExtension χ => α → χ → α
  addExt a c = let (d,f) = a ⊣ dirfile
                in (d,f `addExt` c) ⫣ dirfile

  {- | operator alias for `addExt` -}
  infixr 6 <.> -- same as for ⊕
  (<.>) ∷ HasFilenameExtension χ => α → χ → α
  (<.>) = addExt

  {- | operator alias for `addExt` -}
  infixr 6 ⊙ -- same as for ⊕
  (⊙) ∷ HasFilenameExtension χ => α → χ → α
  (⊙) = (<.>)

  {- | Split an "extension" - the maximal non-empty sequence of characters,
       excluding '.' - from the end of a `PathComponent`; if there is one.
  -}
  splitExt ∷ α → (α, 𝕄 FilenameExtension)
  splitExt a = let (d,f) = a ⊣ dirfile
                in case splitExt f of
                     (b, 𝓙 e)  → ((d,b) ⫣ dirfile, 𝓙 e)
                     (_, 𝓝) → (a, 𝓝)

  ext ∷ α → 𝕄 FilenameExtension
  ext = snd ∘ splitExt

  {-| update an existing extension; this is a no-op on values with no extension
   -}
  updateExt ∷ (FilenameExtension → FilenameExtension) → α → α
  updateExt f x = case splitExt x of
                    (x', 𝓙 e) → x' ⊙ f e
                    (_, 𝓝) → x

  setExt ∷ FilenameExtension → α → α
  setExt = updateExt ∘ const

  {-| split into dir, base filename, maybe extension -}
  split ∷ α → (DirType α, PathComponent, 𝕄 FilenameExtension)
  split f = let (d,b) = f ⊣ dirfile
                (s,e) = splitExt b
             in (d,s,e)

instance FileLike PathComponent where
  dirfile ∷ Iso' PathComponent ((), PathComponent)
  dirfile = iso ((),) (\ ((),p) → p)

  addExt ∷ HasFilenameExtension χ => PathComponent → χ → PathComponent
  addExt p = PathComponent.addExt p ∘ view filenameExtensionPC

  splitExt ∷ PathComponent → (PathComponent, 𝕄 FilenameExtension)
  splitExt = second (fmap new) ∘ PathComponent.splitExt

  updateExt ∷ (FilenameExtension → FilenameExtension) → PathComponent
            → PathComponent
  updateExt f =
    PathComponent.updateExt (view pathComponent' ∘ f ∘ view (re pathComponent'))

-- that's all, folks! ----------------------------------------------------------
