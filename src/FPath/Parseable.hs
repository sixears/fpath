{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE UnicodeSyntax    #-}

{-| A Parsecable class, plus some extra helpful utilities.  Base version, so
    fpath can use it, and ParsecPlus proper can use that (for file parsing). -}
module FPath.Parseable
  ( Parseable(..), parseT, parse' )
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

{-| things that we can parse from stringlike things -}
class Parseable χ where
  {-| parse a value -}
  parse ∷ ∀ ε τ η . (AsFPathError ε, MonadError ε η, HasCallStack, Printable τ)⇒
          τ → η χ

  ------------------

  {-| *PARTIAL*: like `parse`, but will error on failure to parse -}
  __parse__ ∷ (Printable τ) ⇒ τ → χ
  __parse__ = __right__ ∘ parse'

  ------------------

  {-| *PARTIAL*: like `__parse__`, specialized to `String` -}
  __parseS__ ∷ String → χ
  __parseS__ = __parse__

  --------------------

  {-| *PARTIAL*: like `__parse__`, specialized to `String` -}
  __parseT__ ∷ 𝕋 → χ
  __parseT__ = __parse__

  --------------------

  {-| like `parse`, with error type reified to `FPathError` -}
  parseFPE ∷ ∀ τ η . (MonadError FPathError η, HasCallStack, Printable τ) ⇒
             τ → η χ
  parseFPE = parse

  ------------------

  {-| parse a value; the value will have a "/" appended to it if it does not
      already have a trailing "/" -}
  parseDir ∷ ∀ ε τ η .
             (AsFPathError ε, MonadError ε η, HasCallStack, Printable τ) ⇒
             τ → η χ
  parseDir (toText → t) =
    case unsnoc t of
      𝓝          → __FPathEmptyE__ parseT
      𝓙 (_, '/') → parse t
      𝓙 _        → parse (t ⊕ "/")

  --------

  __parseDir__ ∷ (Printable τ, Parseable χ, HasCallStack) => τ → χ
  __parseDir__ =  let __right__ ∷ Printable ε ⇒ Either ε β → β
                      __right__ x = either (error ∘ toString) id x
                  in  __right__ @FPathError ∘ parseDir

  --------

  {-| *PARTIAL*: like `__parseDir__`, specialized to `String` -}
  __parseDirS__ ∷ HasCallStack => 𝕊 → χ
  __parseDirS__ = __parseDir__

  ------------------

  {-| `ReadM` producer, for use with `Options.Applicative` -}
  readM ∷ ReadM χ
  readM = eitherReader (first toString ∘ parse')

  ----------------------------------------------------------

{-# DEPRECATED __parse'__ "use __parseS__ instead" #-}
__parse'__ ∷ Parseable χ => 𝕊 → χ
__parse'__ = __parse__

----------------------------------------

{-# DEPRECATED parse' "use parseFPE instead" #-}
parse' ∷ ∀ χ τ η .
         (Parseable χ, MonadError FPathError η, HasCallStack, Printable τ) ⇒
         τ → η χ
parse' = parse

-- that's all, folks! ----------------------------------------------------------
