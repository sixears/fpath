{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE UnicodeSyntax    #-}

{- | A Parsecable class, plus some extra helpful utilities.  Base version, so
     fpath can use it, and ParsecPlus proper can use that (for file parsing). -}
module FPath.Parseable
  ( Parseable(..), parseT )
where

import Base1T
import Prelude  ( error )

-- base --------------------------------

import Data.Typeable  ( Proxy( Proxy ), TypeRep, typeRep )

-- optparse-applicative ----------------

import Options.Applicative  ( ReadM, eitherReader )

-- text --------------------------------

import Data.Text  ( unsnoc )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )

--------------------------------------------------------------------------------

data Parse -- uninhabited
parseT ∷ TypeRep
parseT = typeRep (Proxy ∷ Proxy Parse)

__right__ ∷ Printable ε ⇒ Either ε β → β
__right__ x = either (error ∘ toString) id x

class Parseable χ where
  {- | Parse a value -}
  parse ∷ ∀ ε τ η . (AsFPathError ε, MonadError ε η, HasCallStack, Printable τ)⇒
          τ → η χ

  ------------------

  {- | Like `parse`, with error type reified to `FPathError`. -}
  parse' ∷ ∀ τ η . (MonadError FPathError η, HasCallStack, Printable τ) ⇒
           τ → η χ
  parse' = parse

  ------------------

  {- | Parse a value; the value will have a "/" appended to it if it does not
       already have a trailing "/". -}
  parseDir ∷ ∀ ε τ η .
             (AsFPathError ε, MonadError ε η, HasCallStack, Printable τ) ⇒
             τ → η χ
  parseDir (toText → t) =
    case unsnoc t of
      𝕹          → __FPathEmptyE__ parseT
      𝕵 (_, '/') → parse t
      𝕵 _        → parse (t ⊕ "/")

  ------------------

  {- | *PARTIAL*: Like `parse`, but will error on failure to parse -}
  __parse__ ∷ (Printable τ) ⇒ τ → χ
  __parse__ = __right__ ∘ parse'

  ------------------

  {- | *PARTIAL*: Like `__parse__`, specialized to `String` -}
  __parse'__ ∷ String → χ
  __parse'__ = __parse__

  --------------------

  {- | `ReadM` producer, for use with `Options.Applicative`. -}
  readM ∷ ReadM χ
  readM = eitherReader (first toString ∘ parse')

-- that's all, folks! ----------------------------------------------------------
