{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}

module FPath.AsFilePath'
  ( AsFilePath'( filepath' ), exterminate, terminate )
where

-- base --------------------------------

import Data.List   ( dropWhileEnd )
import System.IO   ( FilePath )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode      ( (≡) )
import Data.Monoid.Unicode  ( (⊕) )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism' )

-- safe --------------------------------

import Safe  ( lastDef )

--------------------------------------------------------------------------------

{- Like `AsFilePath`, except that directories when converted to a filepath do
   not have a trailing slash - except for the root directory, '/'.  When
   parsing directories from a filepath, a trailing slash is not required.
   Consequently, this cannot work with any type that comprises both files & dirs
   (since there is no way to distinguish between them, when parsing).
-}
-- make it a lens
-- test this for stat
class AsFilePath' α where
  filepath' ∷ Prism' FilePath α

{- | Ensure a filepath ends with a trailing slash: add one iff it does not
     already end with one. -}
terminate ∷ FilePath → FilePath
terminate s = case lastDef '/' s of
                '/' → s
                _   → s ⊕ "/"

{- | Ensure a filepath does not end with a trailing slash; strip off any
     trailing slash(es) from a filepath, unless that filepath is "/". -}
exterminate ∷ FilePath → FilePath
exterminate "/" = "/"
exterminate s   = dropWhileEnd (≡ '/') s

-- that's all, folks! ----------------------------------------------------------
