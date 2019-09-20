{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax    #-}

{- | A Parsecable class, plus some extra helpful utilities.  Base version, so
     fpath can use it, and ParsecPlus proper can use that (for file parsing). -}
module FPath.Parseable
  ( Parseable(..) )
where

import Prelude  ( error )

-- base --------------------------------

import Data.Bifunctor  ( first )
import Data.Either     ( Either, either )
import Data.Function   ( id )
import Data.String     ( String )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import Options.Applicative  ( ReadM, eitherReader )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Error.FPathError  ( AsFPathError, FPathError )

--------------------------------------------------------------------------------

__right__ ∷ Printable ε ⇒ Either ε β → β
__right__ x = either (error ∘ toString) id x

class Parseable χ where
  {- | Parse a value -}
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η χ

  ------------------

  {- | Like `parse`, with error type reified to `FPathError`. -}
  parse' ∷ (MonadError FPathError μ, Printable τ) ⇒ τ → μ χ
  parse' = parse

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
