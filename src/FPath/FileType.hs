{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.FileType
  ( Filename, HasFileType( FileType ), IsFile )
where

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

class HasFileType α where
  type FileType α

{- | Just a marker class for types that represent a file -}
class (Element α ~ PathComponent, IsMonoSeqNonEmpty α) ⇒ IsFile α

-- that's all, folks! ----------------------------------------------------------
