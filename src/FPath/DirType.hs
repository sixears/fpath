{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.DirType
  ( HasDirType( DirType ) )
where

-- base --------------------------------

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

{- | the directory "version" of a type; e.g., `DirType RelFile = RelDir` -}
class HasDirType α where
  type DirType α

-- that's all, folks! ----------------------------------------------------------
