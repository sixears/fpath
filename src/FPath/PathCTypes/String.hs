{-# LANGUAGE UnicodeSyntax #-}

{-| Types for use in PathComponent, to allow easy exchange of
    Text / ByteString /String
-}
module FPath.PathCTypes.String
  ( PathCChar, PathCInner, pathCChar, to_inner, to_print, to_string )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Data.Char      ( Char, chr )
import Data.Function  ( id )
import Data.String    ( String )
import Data.Word      ( Word8 )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

type PathCChar  = Char
type PathCInner = String

pathCChar ∷ Word8 → PathCChar
pathCChar = chr ∘ fromIntegral

to_print ∷ P.Printer ρ ⇒ PathCInner → ρ
to_print = P.string

to_string ∷ PathCInner → String
to_string = id

to_inner ∷ String → PathCInner
to_inner = id

-- that's all, folks! ----------------------------------------------------------
