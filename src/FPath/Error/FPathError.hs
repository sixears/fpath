{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.Error.FPathError
  ( AsFPathError, FPathError(..)
  , __FPathComponentE__ , __FPathEmptyE__, __FPathNonAbsE__, __FPathNotADirE__ )
where

-- base --------------------------------

import Data.Eq        ( Eq )
import Data.Function  ( ($), id )
import Data.Typeable  ( TypeRep )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism', prism' )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Error.FPathComponentError
           ( AsFPathComponentError( _FPathComponentError ), FPathComponentError )
import FPath.Util  ( (⋕) )

--------------------------------------------------------------------------------

data FPathError = FPathEmptyE TypeRep
                | FPathNonAbsE TypeRep Text
                | FPathNotADirE TypeRep Text
                | FPathComponentE FPathComponentError TypeRep Text
  deriving (Eq, Show)

class AsFPathError ε where
  _FPathError ∷ Prism' ε FPathError

instance AsFPathError FPathError where
  _FPathError = id

instance Printable FPathError where
  print (FPathEmptyE ty)     = P.text $ [fmt|empty %w|] ty
  print (FPathNonAbsE ty t)  = P.text $ [fmt|non-absolute %w: '%T'|] ty t
  print (FPathNotADirE ty t) = P.text $ [fmt|%w lacks trailing /: '%T'|]  ty t
  print (FPathComponentE ce ty t) =
    P.text $ [fmt|component error %T in %w '%T'|] ce ty t

------------------------------------------------------------

_FPathEmptyE ∷ AsFPathError ε ⇒ TypeRep → ε
_FPathEmptyE = (_FPathError ⋕) ∘ FPathEmptyE

__FPathEmptyE__ ∷ (AsFPathError ε, MonadError ε η) ⇒ TypeRep → η α
__FPathEmptyE__ = throwError ∘ _FPathEmptyE

_FPathNonAbsE ∷ AsFPathError ε ⇒ TypeRep → Text → ε
_FPathNonAbsE r t = _FPathError ⋕ FPathNonAbsE r t

__FPathNonAbsE__ ∷ (AsFPathError ε,MonadError ε η) ⇒ TypeRep → Text → η α
__FPathNonAbsE__ r t = throwError $ _FPathNonAbsE r t

_FPathNotADirE ∷ AsFPathError ε ⇒ TypeRep → Text → ε
_FPathNotADirE r t = _FPathError ⋕ FPathNotADirE r t

__FPathNotADirE__ ∷ (AsFPathError ε,MonadError ε η) ⇒ TypeRep → Text → η α
__FPathNotADirE__ r t = throwError $ _FPathNotADirE r t

_FPathComponentE ∷ AsFPathError ε ⇒ FPathComponentError → TypeRep → Text → ε
_FPathComponentE ce r t = _FPathError ⋕ FPathComponentE ce r t

__FPathComponentE__ ∷ (AsFPathError ε,MonadError ε η) ⇒
                      FPathComponentError → TypeRep → Text → η α
__FPathComponentE__ ce r t = throwError $ _FPathComponentE ce r t

-- that's all, folks! ----------------------------------------------------------
