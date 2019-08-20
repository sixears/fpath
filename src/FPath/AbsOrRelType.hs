{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.AbsOrRelType
  ( Abs( Abs ), HasAbsOrRel(..), Rel( Rel ) )
where

-- base --------------------------------

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

class HasAbsOrRel α where
  type AbsOrRel α

data Rel = Rel
data Abs = Abs

-- that's all, folks! ----------------------------------------------------------
