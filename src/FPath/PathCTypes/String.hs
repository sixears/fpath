{-# LANGUAGE UnicodeSyntax #-}

{-| Types for use in PathComponent, to allow easy exchange of
    Text / ByteString /String
-}
module FPath.PathCTypes.String
  ( PathCChar, PathCInner
  , pathCChar, to_inner, to_lower, to_print, to_string, to_upper )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Data.Char      ( Char, chr, toLower, toUpper )
import Data.Function  ( id )
import Data.Functor   ( fmap )
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

to_lower ∷ PathCInner → PathCInner
to_lower = fmap toLower

to_upper ∷ PathCInner → PathCInner
to_upper = fmap toUpper

-- that's all, folks! ----------------------------------------------------------
