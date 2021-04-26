{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | Classes for methods common to most all FPath types , such as `dirname` -}

module FPath.RelType
  ( RelTypeC( RelType ) )
where

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

class RelTypeC α where
  {- | the relative "version" of a type; e.g., `RelType RelFile = AbsFile` -}
  type RelType α

-- that's all, folks! ----------------------------------------------------------
