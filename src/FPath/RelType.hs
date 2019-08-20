{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.RelType
  ( HasRelType( RelType ), IsRel )
where

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqConversions  ( IsMonoSeq )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.PathComponent  ( PathComponent )
import FPath.AbsDir         ( AbsDir )
import FPath.AbsFile        ( AbsFile )
import FPath.RelDir         ( RelDir )
import FPath.RelFile        ( RelFile )

--------------------------------------------------------------------------------

class HasRelType α where
  {- | the relative "version" of a type; e.g., `RelType RelFile = AbsFile` -}
  type RelType α

instance HasRelType AbsFile where
  type RelType AbsFile = RelFile

instance HasRelType AbsDir where
  type RelType AbsDir = RelDir

instance HasRelType RelFile where
  type RelType RelFile = RelFile

instance HasRelType RelDir where
  type RelType RelDir = RelDir

{- | Just a marker class for types that represent a relative path -}
class (Element α ~ PathComponent, IsMonoSeq α) ⇒ IsRel α

-- that's all, folks! ----------------------------------------------------------
