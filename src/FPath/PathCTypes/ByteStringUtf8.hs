{-# LANGUAGE UnicodeSyntax #-}

{-| Types for use in PathComponent, to allow easy exchange of
    Text / ByteString /String
-}
module FPath.PathCTypes.ByteStringUtf8
  ( {- PathCChar, PathCInner, pathCChar, to_inner, to_print, to_string -} )
where

{-

import Prelude  ( fromIntegral )

-- base --------------------------------

import Data.Char      ( Char, chr )
import Data.Function  ( id )
import Data.String    ( String )
import Data.Word      ( Word8 )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- bytestring --------------------------

import Data.ByteString.Char8  ( ByteString, pack, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

type PathCChar  = Char
type PathCInner = ByteString

pathCChar ∷ Word8 → PathCChar
pathCChar = chr ∘ fromIntegral

to_print ∷ P.Printer ρ ⇒ PathCInner → ρ
to_print = P.utf8

to_string ∷ PathCInner → String
to_string = unpack

to_inner ∷ String → PathCInner
to_inner = pack

-}

-- that's all, folks! ----------------------------------------------------------
