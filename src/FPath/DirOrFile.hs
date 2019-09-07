{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.DirOrFile
  ( DirOrFile(..), HasDirOrFile( dirOrFile, isDir, isFile ) )
where

-- base --------------------------------

import Data.Bool  ( Bool )
import Data.Eq    ( Eq )
import Text.Show  ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

data DirOrFile = Dir | File
  deriving (Eq,Show)

class HasDirOrFile α where
  dirOrFile ∷ α → DirOrFile
  isDir ∷ α → Bool
  isDir = (≡ Dir) ∘ dirOrFile
  isFile ∷ α → Bool
  isFile = (≡ File) ∘ dirOrFile

-- that's all, folks! ----------------------------------------------------------
