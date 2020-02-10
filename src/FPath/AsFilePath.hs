{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}

module FPath.AsFilePath
  ( AsFilePath(..) )
where

-- base --------------------------------

import Data.Function  ( id )
import System.IO      ( FilePath )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

class AsFilePath α where
  filepath ∷ Prism' FilePath α

instance AsFilePath FilePath where
  filepath = id

-- that's all, folks! ----------------------------------------------------------
