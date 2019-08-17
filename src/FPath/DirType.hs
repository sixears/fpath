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

class HasDirType α where
  {- | the directory "version" of a type; e.g., `DirType RelFile = RelDir` -}
  type DirType α

-- that's all, folks! ----------------------------------------------------------
