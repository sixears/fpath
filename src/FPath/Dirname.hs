{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | Classes for methods common to most all FPath types , such as `dirname` -}

module FPath.Dirname
  ( HasDirname( dirname ) )
where

-- lens --------------------------------

import Control.Lens.Lens  ( Lens' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.DirType  ( DirTypeC( DirType ) )

--------------------------------------------------------------------------------

class DirTypeC α ⇒ HasDirname α where
  {- | Well-typed version of `dirname` utility; note that `dirname` of "/" is
       "/", and `dirname` of "./" is "./".
   -}
  dirname ∷ Lens' α (DirType α)

-- that's all, folks! ----------------------------------------------------------
