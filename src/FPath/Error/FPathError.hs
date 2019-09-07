{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.Error.FPathError
  ( AsFPathError( _FPathError ), FPathError(..)
  , AsFPathNotAPrefixError( _FPathNotAPrefixError ), FPathNotAPrefixError(..)
  , FPathIOError

  , _FPathAbsE, _FPathComponentE, _FPathEmptyE, _FPathNonAbsE, _FPathNotADirE
  , _FPathNotAFileE, _FPathNotAPrefixError, _FPathRootDirE

  , __FPathAbsE__, __FPathComponentE__ , __FPathEmptyE__, __FPathNonAbsE__
  , __FPathNotADirE__, __FPathNotAFileE__, __FPathNotAPrefixError__
  , __FPathRootDirE__
  , mapTypeRepE, tmap, mapTextE
  )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Control.Monad      ( return )
import Data.Either        ( Either, either )
import Data.Eq            ( Eq )
import Data.Function      ( ($), id )
import Data.Maybe         ( Maybe( Just, Nothing ) )
import Data.Typeable      ( TypeRep )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.Review  ( (#) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ), IOError )

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

class TMap α where
  -- | fmap the Text bit of an FPathError
  tmap ∷ (Text → Text) → α → α

------------------------------------------------------------

data FPathNotAPrefixError = FPathNotAPrefixError TypeRep Text Text
  deriving (Eq, Show)

instance Exception FPathNotAPrefixError

class AsFPathNotAPrefixError ε where
  _FPathNotAPrefixError ∷ Prism' ε FPathNotAPrefixError

instance AsFPathNotAPrefixError FPathNotAPrefixError where
  _FPathNotAPrefixError = id

instance Printable FPathNotAPrefixError where
  print (FPathNotAPrefixError r t t') =
    P.text $ [fmt|%t is not a prefix of %t (%w)|] t t' r

type instance Element FPathNotAPrefixError = TypeRep
instance MonoFunctor FPathNotAPrefixError where
  omap f (FPathNotAPrefixError r t t') = FPathNotAPrefixError (f r) t t'

instance TMap FPathNotAPrefixError where
  tmap f (FPathNotAPrefixError r t t') = FPathNotAPrefixError r (f t) (f t')

__FPathNotAPrefixError__ ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                           TypeRep → Text → Text → η α
__FPathNotAPrefixError__ r t t' =
  throwError ∘ (_FPathNotAPrefixError #) $ FPathNotAPrefixError r t t'

------------------------------------------------------------

data FPathError = FPathEmptyE       TypeRep
                | FPathAbsE         TypeRep Text
                | FPathNonAbsE      TypeRep Text
                | FPathNotADirE     TypeRep Text
                | FPathNotAFileE    TypeRep Text
                | FPathComponentE   FPathComponentError TypeRep Text
                | FPathRootDirE     TypeRep
                | FPathNotAPrefixE  FPathNotAPrefixError
  deriving (Eq, Show)

instance Exception FPathError

class AsFPathError ε where
  _FPathError ∷ Prism' ε FPathError

instance AsFPathError FPathError where
  _FPathError = id

instance AsFPathNotAPrefixError FPathError where
  _FPathNotAPrefixError = prism' FPathNotAPrefixE
                                 (\ case FPathNotAPrefixE e → Just e
                                         _                  → Nothing)

instance Printable FPathError where
  print (FPathEmptyE    ty)   = P.text $ [fmt|empty %w|] ty
  print (FPathNonAbsE   ty t) = P.text $ [fmt|non-absolute %w: '%T'|] ty t
  print (FPathAbsE      ty t) = P.text $ [fmt|absolute %w: '%T'|] ty t
  print (FPathNotADirE  ty t) = P.text $ [fmt|%w lacks trailing /: '%T'|]  ty t
  print (FPathNotAFileE ty t) = P.text $ [fmt|%w has trailing /: '%T'|]  ty t
  print (FPathComponentE ce ty t) =
    P.text $ [fmt|component error %T in %w '%T'|] ce ty t
  print (FPathRootDirE ty)    = P.text $ [fmt|is root dir: %w|] ty
  print (FPathNotAPrefixE e)  = print e

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

_FPathNotAPrefixE ∷ AsFPathError ε ⇒ FPathNotAPrefixError → ε
_FPathNotAPrefixE nape = _FPathError # FPathNotAPrefixE nape

__FPathNotAPrefixE__ ∷ (AsFPathError ε,MonadError ε η) ⇒
                       FPathNotAPrefixError → η α
__FPathNotAPrefixE__ nape = throwError $ _FPathNotAPrefixE nape

----------------------------------------

type instance Element FPathError = TypeRep
instance MonoFunctor FPathError where
  omap f (FPathEmptyE        r  ) = FPathEmptyE       (f r)
  omap f (FPathRootDirE      r  ) = FPathRootDirE     (f r)
  omap f (FPathAbsE          r t) = FPathAbsE         (f r) t
  omap f (FPathNonAbsE       r t) = FPathNonAbsE      (f r) t
  omap f (FPathNotADirE      r t) = FPathNotADirE     (f r) t
  omap f (FPathNotAFileE     r t) = FPathNotAFileE    (f r) t
  omap f (FPathComponentE  e r t) = FPathComponentE e (f r) t
  omap f (FPathNotAPrefixE e    ) = FPathNotAPrefixE (omap f e)

instance TMap FPathError where
  -- | fmap the Text bit of an FPathError
  tmap ∷ (Text → Text) → FPathError → FPathError
  tmap _ e@(FPathEmptyE     _)   = e
  tmap _ e@(FPathRootDirE   _)   = e
  tmap f (FPathAbsE          r t) = FPathAbsE         r (f t)
  tmap f (FPathNonAbsE       r t) = FPathNonAbsE      r (f t)
  tmap f (FPathNotADirE      r t) = FPathNotADirE     r (f t)
  tmap f (FPathNotAFileE     r t) = FPathNotAFileE    r (f t)
  tmap f (FPathComponentE  e r t) = FPathComponentE e r (f t)
  tmap f (FPathNotAPrefixE e    ) = FPathNotAPrefixE (tmap f e)

mapTypeRepE ∷ (MonadError ε η, AsFPathError ε) ⇒
              (TypeRep → TypeRep) → Either FPathError α → η α
mapTypeRepE f = either (throwError ∘ (_FPathError #) ∘ omap f) return

mapTextE ∷ (MonadError ε η, AsFPathError ε) ⇒
              (Text → Text) → Either FPathError α → η α
mapTextE f = either (throwError ∘ (_FPathError #) ∘ tmap f) return

------------------------------------------------------------

data FPathIOError = FPIO_PATH_ERROR  FPathError
                  | FPIO_IO_ERROR    IOError
  deriving Eq

instance Exception FPathIOError

-- $( makePrisms ''FPathIOError )

_FPIO_PATH_ERROR ∷ Prism' FPathIOError FPathError
_FPIO_PATH_ERROR = prism' (\ e → FPIO_PATH_ERROR e)
                          (\ case FPIO_PATH_ERROR e → Just e; _ → Nothing)

_FPIO_IO_ERROR ∷ Prism' FPathIOError IOError
_FPIO_IO_ERROR = prism' (\ e → FPIO_IO_ERROR e)
                        (\ case FPIO_IO_ERROR e → Just e; _ → Nothing)

instance Show FPathIOError where
  show (FPIO_PATH_ERROR e) = show e
  show (FPIO_IO_ERROR   e) = show e

instance AsFPathError FPathIOError where
  _FPathError = _FPIO_PATH_ERROR

instance AsIOError FPathIOError where
  _IOError = _FPIO_IO_ERROR

instance Printable FPathIOError where
  print (FPIO_PATH_ERROR e) = print e
  print (FPIO_IO_ERROR   e) = print e

-- that's all, folks! ----------------------------------------------------------
