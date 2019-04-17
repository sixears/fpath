{-# LANGUAGE UnicodeSyntax #-}

{-| Types for use in PathComponent, to allow easy exchange of
    Text / ByteString /String
-}
module FPath.PathCTypes.ByteString
  ( {- PathCChar, PathCInner, pathCChar, to_inner, to_print, to_string -} )
where

{- 
import Prelude  ( fromEnum, fromIntegral )

-- base --------------------------------

import Data.Char      ( Char, chr )
import Data.Function  ( id )
import Data.Functor   ( fmap )
import Data.String    ( String )
import Data.Word      ( Word8 )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- bytestring --------------------------

import Data.ByteString  ( ByteString, pack, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

import Data.ByteString as S (ByteString, unpack)
import Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)

strToBS :: String -> S.ByteString
strToBS = C8.pack

bsToStr :: S.ByteString -> String
bsToStr = fmap (chr ∘ fromEnum) ∘ S.unpack

type PathCChar  = Char
type PathCInner = ByteString

pathCChar ∷ Word8 → PathCChar
pathCChar = chr ∘ fromEnum

to_print ∷ P.Printer ρ ⇒ PathCInner → ρ
to_print = P.utf8

to_string ∷ PathCInner → String
to_string = bsToStr

to_inner ∷ String → PathCInner
to_inner = strToBS

-}

-- that's all, folks! ----------------------------------------------------------
