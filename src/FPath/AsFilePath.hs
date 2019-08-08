{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.AsFilePath
  ( AsFilePath(..) )
where

-- base --------------------------------

import System.IO  ( FilePath )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

class AsFilePath α where
  filepath ∷ Prism' FilePath α

-- that's all, folks! ----------------------------------------------------------
