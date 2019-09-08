{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | Classes for methods common to most all FPath types , such as `dirname` -}

module FPath.DirType
  ( DirTypeC( DirType ) )
where

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

class DirTypeC α where
  {- | the directory "version" of a type; e.g., `DirType RelFile = RelDir` -}
  type DirType α
-- type family DirType α

{- | Just a marker class for types that represent a directory -}
-- class (Element α ~ PathComponent, IsMonoSeq α) ⇒ IsDir α

-- that's all, folks! ----------------------------------------------------------
