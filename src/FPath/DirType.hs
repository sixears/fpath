{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.DirType
  ( HasDirType( DirType ), IsDir )
where

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqConversions  ( IsMonoSeq )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.PathComponent  ( PathComponent )

--------------------------------------------------------------------------------

class HasDirType α where
  {- | the directory "version" of a type; e.g., `DirType RelFile = RelDir` -}
  type DirType α

{- | Just a marker class for types that represent a directory -}
class (Element α ~ PathComponent, IsMonoSeq α) ⇒ IsDir α

-- that's all, folks! ----------------------------------------------------------
