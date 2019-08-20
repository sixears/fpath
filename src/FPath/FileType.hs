{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.FileType
  ( Filename, HasFile( file ), HasFileType( FileType ), IsFile )
where

-- lens --------------------------------

import Control.Lens.Lens  ( Lens' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNEConversions  ( IsMonoSeqNonEmpty )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.PathComponent  ( PathComponent )

--------------------------------------------------------------------------------

type Filename = PathComponent

class HasFile α where
--  {- | the file "version" of a type; e.g., `FileType RelDir = RelFile` -}
--  type FileType α
  file ∷ Lens' α Filename

class HasFileType α where
  type FileType α

{- | Just a marker class for types that represent a file -}
class (Element α ~ PathComponent, IsMonoSeqNonEmpty α) ⇒ IsFile α

-- that's all, folks! ----------------------------------------------------------
