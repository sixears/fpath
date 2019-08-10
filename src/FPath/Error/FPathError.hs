{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.Error.FPathError
  ( AsFPathError( _FPathError ), FPathError(..)
  , __FPathComponentE__ , __FPathEmptyE__, __FPathNonAbsE__, __FPathAbsE__
  , __FPathNotADirE__, __FPathNotAFileE__, __FPathRootDirE__
  , mapTypeRepE, tmap, mapTextE
  )
where

import Prelude ( error )

-- base --------------------------------

import Control.Monad  ( return )
import Data.Either    ( Either, either )
import Data.Eq        ( Eq )
import Data.Function  ( ($), id )
import Data.Typeable  ( TypeRep )
import Text.Show      ( Show, show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- mono-traversable --------------------

import Data.MonoTraversable( Element, MonoFunctor( omap ) )

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

import FPath.Error.FPathComponentError ( FPathComponentError )

--------------------------------------------------------------------------------

data FPathError = FPathEmptyE     TypeRep
                | FPathAbsE       TypeRep Text
                | FPathNonAbsE    TypeRep Text
                | FPathNotADirE   TypeRep Text
                | FPathNotAFileE  TypeRep Text
                | FPathComponentE FPathComponentError TypeRep Text
                | FPathRootDirE   TypeRep
  deriving (Eq, Show)

class AsFPathError ε where
  _FPathError ∷ Prism' ε FPathError

instance AsFPathError FPathError where
  _FPathError = id

instance Printable FPathError where
  print (FPathEmptyE    ty)   = P.text $ [fmt|empty %w|] ty
  print (FPathNonAbsE   ty t) = P.text $ [fmt|non-absolute %w: '%T'|] ty t
  print (FPathAbsE      ty t) = P.text $ [fmt|absolute %w: '%T'|] ty t
  print (FPathNotADirE  ty t) = P.text $ [fmt|%w lacks trailing /: '%T'|]  ty t
  print (FPathNotAFileE ty t) = P.text $ [fmt|%w has trailing /: '%T'|]  ty t
  print (FPathComponentE ce ty t) =
    P.text $ [fmt|component error %T in %w '%T'|] ce ty t
  print (FPathRootDirE ty)     = P.text $ [fmt|is root dir: %w|] ty

------------------------------------------------------------

_FPathEmptyE ∷ AsFPathError ε ⇒ TypeRep → ε
_FPathEmptyE = (_FPathError #) ∘ FPathEmptyE

__FPathEmptyE__ ∷ (AsFPathError ε, MonadError ε η) ⇒ TypeRep → η α
__FPathEmptyE__ = throwError ∘ _FPathEmptyE

_FPathNonAbsE ∷ AsFPathError ε ⇒ TypeRep → Text → ε
_FPathNonAbsE r t = _FPathError # FPathNonAbsE r t

__FPathNonAbsE__ ∷ (AsFPathError ε,MonadError ε η) ⇒ TypeRep → Text → η α
__FPathNonAbsE__ r t = throwError $ _FPathNonAbsE r t

_FPathAbsE ∷ AsFPathError ε ⇒ TypeRep → Text → ε
_FPathAbsE r t = _FPathError # FPathAbsE r t

__FPathAbsE__ ∷ (AsFPathError ε,MonadError ε η) ⇒ TypeRep → Text → η α
__FPathAbsE__ r t = throwError $ _FPathAbsE r t

_FPathNotADirE ∷ AsFPathError ε ⇒ TypeRep → Text → ε
_FPathNotADirE r t = _FPathError # FPathNotADirE r t

__FPathNotADirE__ ∷ (AsFPathError ε,MonadError ε η) ⇒ TypeRep → Text → η α
__FPathNotADirE__ r t = throwError $ _FPathNotADirE r t

_FPathNotAFileE ∷ AsFPathError ε ⇒ TypeRep → Text → ε
_FPathNotAFileE r t = _FPathError # FPathNotAFileE r t

__FPathNotAFileE__ ∷ (AsFPathError ε,MonadError ε η) ⇒ TypeRep → Text → η α
__FPathNotAFileE__ r t = throwError $ _FPathNotAFileE r t

_FPathComponentE ∷ AsFPathError ε ⇒ FPathComponentError → TypeRep → Text → ε
_FPathComponentE ce r t = _FPathError # FPathComponentE ce r t

__FPathComponentE__ ∷ (AsFPathError ε,MonadError ε η) ⇒
                      FPathComponentError → TypeRep → Text → η α
__FPathComponentE__ ce r t = throwError $ _FPathComponentE ce r t

_FPathRootDirE ∷ AsFPathError ε ⇒ TypeRep → ε
_FPathRootDirE = (_FPathError #) ∘ FPathRootDirE

__FPathRootDirE__ ∷ (AsFPathError ε, MonadError ε η) ⇒ TypeRep → η α
__FPathRootDirE__ = throwError ∘ _FPathRootDirE

----------------------------------------

type instance Element FPathError = TypeRep
instance MonoFunctor FPathError where
  omap f (FPathEmptyE       r)   = FPathEmptyE       (f r)
  omap f (FPathRootDirE     r)   = FPathRootDirE     (f r)
  omap f (FPathAbsE         r t) = FPathAbsE         (f r) t
  omap f (FPathNonAbsE      r t) = FPathNonAbsE      (f r) t
  omap f (FPathNotADirE     r t) = FPathNotADirE     (f r) t
  omap f (FPathNotAFileE    r t) = FPathNotAFileE    (f r) t
  omap f (FPathComponentE e r t) = FPathComponentE e (f r) t

-- | fmap the Text bit of an FPathError
tmap ∷ (Text → Text) → FPathError → FPathError
tmap f e@(FPathEmptyE     _)   = e
tmap f e@(FPathRootDirE   _)   = e
tmap f (FPathAbsE         r t) = FPathAbsE         r (f t)
tmap f (FPathNonAbsE      r t) = FPathNonAbsE      r (f t)
tmap f (FPathNotADirE     r t) = FPathNotADirE     r (f t)
tmap f (FPathNotAFileE    r t) = FPathNotAFileE    r (f t)
tmap f (FPathComponentE e r t) = FPathComponentE e r (f t)

mapTypeRepE ∷ (MonadError ε η, AsFPathError ε) ⇒
              (TypeRep → TypeRep) → Either FPathError α → η α
mapTypeRepE f = either (throwError ∘ (_FPathError #) ∘ omap f) return

mapTextE ∷ (MonadError ε η, AsFPathError ε) ⇒
              (Text → Text) → Either FPathError α → η α
mapTextE f = either (throwError ∘ (_FPathError #) ∘ tmap f) return

-- that's all, folks! ----------------------------------------------------------
