module FPath.Error.FPathError
  ( AsFPathError( _FPathError ), FPathError(..)
  , AsFPathNotAPrefixError( _FPathNotAPrefixError ), FPathNotAPrefixError(..)
  , FPathIOError, _FPIO_IO_ERROR, _FPIO_PATH_ERROR

  , _FPathAbsE, _FPathComponentE, _FPathEmptyE, _FPathNonAbsE, _FPathNotADirE
  , _FPathNotAFileE, _FPathRootDirE

  , __FPathAbsE__, __FPathComponentE__ , __FPathEmptyE__, __FPathNonAbsE__
  , __FPathNotADirE__, __FPathNotAFileE__, __FPathNotAPrefixError__
  , __FPathRootDirE__
  , fPathAbsE, fPathEmptyE, fpathIOErrorEither, fPathNonAbsE, fPathNotADirE
  , fPathNotAFileE, fPathNotAPrefixError, fPathRootDirE
  , mapTypeRepE, tmap, mapTextE
  )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Control.Monad      ( return )
import Data.Bool          ( Bool( False ) )
import Data.Either        ( Either( Left, Right ), either )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( ($), (&), id )
import Data.Maybe         ( Maybe( Just, Nothing ) )
import Data.Typeable      ( TypeRep )
import GHC.Generics       ( Generic )
import GHC.Stack          ( CallStack, HasCallStack, callStack )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.Review  ( (#) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ), IOError )

-- mono-traversable --------------------

import Data.MonoTraversable( Element, MonoFunctor( omap ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣), (⊢) )

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

data FPathNotAPrefixError = FPathNotAPrefixError TypeRep Text Text CallStack
  deriving (Generic,NFData,Show)

instance Exception FPathNotAPrefixError

instance Eq FPathNotAPrefixError where
  FPathNotAPrefixError r t u _ == FPathNotAPrefixError r' t' u' _ =
    (r,t,u) == (r',t',u')

instance HasCallstack FPathNotAPrefixError where
  callstack = lens (\ (FPathNotAPrefixError _ _ _ cs) → cs)
                   (\ (FPathNotAPrefixError r t u _) cs →
                      (FPathNotAPrefixError r t u cs))

class AsFPathNotAPrefixError ε where
  _FPathNotAPrefixError ∷ Prism' ε FPathNotAPrefixError

instance AsFPathNotAPrefixError FPathNotAPrefixError where
  _FPathNotAPrefixError = id

instance Printable FPathNotAPrefixError where
  print (FPathNotAPrefixError r t u _) =
    P.text $ [fmt|%t is not a prefix of %t (%w)|] t u r

type instance Element FPathNotAPrefixError = TypeRep
instance MonoFunctor FPathNotAPrefixError where
  omap f (FPathNotAPrefixError r t u cs) = FPathNotAPrefixError (f r) t u cs

instance TMap FPathNotAPrefixError where
  tmap f (FPathNotAPrefixError r t u cs) = FPathNotAPrefixError r (f t) (f u) cs

fPathNotAPrefixError ∷ HasCallStack ⇒
                       TypeRep → Text → Text → FPathNotAPrefixError
fPathNotAPrefixError r t u = FPathNotAPrefixError r t u callStack

__FPathNotAPrefixError__ ∷ (AsFPathNotAPrefixError ε,
                            MonadError ε η, HasCallStack) ⇒
                           TypeRep → Text → Text → η α
__FPathNotAPrefixError__ r t u =
  throwError ∘ (_FPathNotAPrefixError #) $ FPathNotAPrefixError r t u callStack

------------------------------------------------------------

data FPathError = FPathEmptyE       TypeRep CallStack
                | FPathAbsE         TypeRep Text CallStack
                | FPathNonAbsE      TypeRep Text CallStack
                | FPathNotADirE     TypeRep Text CallStack
                | FPathNotAFileE    TypeRep Text CallStack
                | FPathComponentE   FPathComponentError TypeRep Text
                | FPathRootDirE     TypeRep CallStack
                | FPathNotAPrefixE  FPathNotAPrefixError
  deriving (Generic,NFData,Show)

instance Exception FPathError

instance Eq FPathError where
  (FPathEmptyE r _) == (FPathEmptyE r' _) = r ≡ r'
  (FPathAbsE r t _) == (FPathAbsE r' t' _) = (r,t) ≡ (r',t')
  (FPathNonAbsE r t _) == (FPathNonAbsE r' t' _) = (r,t) ≡ (r',t')
  (FPathNotADirE r t _) == (FPathNotADirE r' t' _) = (r,t) ≡ (r',t')
  (FPathNotAFileE r t _) == (FPathNotAFileE r' t' _) = (r,t) ≡ (r',t')
  (FPathComponentE fpce r t) == (FPathComponentE fpce' r' t') =
    (fpce,r,t) ≡ (fpce',r',t')
  (FPathRootDirE r _) == (FPathRootDirE r' _) = r ≡ r'
  (FPathNotAPrefixE fpnape) == (FPathNotAPrefixE fpnape') = fpnape ≡ fpnape'
  _ == _ = False

instance HasCallstack FPathError where
  callstack = lens (\ case (FPathEmptyE    _   cs) → cs
                           (FPathAbsE      _ _ cs) → cs
                           (FPathNonAbsE   _ _ cs) → cs
                           (FPathNotADirE  _ _ cs) → cs
                           (FPathNotAFileE _ _ cs) → cs
                           (FPathComponentE fpce _ _) → fpce ⊣ callstack
                           (FPathRootDirE  _   cs) → cs
                           (FPathNotAPrefixE fpnape)  → fpnape ⊣ callstack
                   )
                   (\ fpe cs →
                      case fpe of
                        (FPathEmptyE    r _)   → FPathEmptyE r cs
                        (FPathAbsE      r t _) → FPathAbsE r t cs
                        (FPathNonAbsE   r t _) → FPathNonAbsE r t cs
                        (FPathNotADirE  r t _) → FPathNotADirE r t cs
                        (FPathNotAFileE r t _) → FPathNotAFileE r t cs
                        (FPathRootDirE  r _)   → FPathRootDirE r cs
                        (FPathComponentE fpce r t) →
                          FPathComponentE (fpce & callstack ⊢ cs) r t
                        (FPathNotAPrefixE fpnape) →
                          FPathNotAPrefixE $ fpnape & callstack ⊢ cs
                   )

--------------------

class AsFPathError ε where
  _FPathError ∷ Prism' ε FPathError

instance AsFPathError FPathError where
  _FPathError = id

instance AsFPathNotAPrefixError FPathError where
  _FPathNotAPrefixError = prism' FPathNotAPrefixE
                                 (\ case (FPathNotAPrefixE e) → Just e
                                         _                  → Nothing)

instance Printable FPathError where
  print (FPathEmptyE    ty _)   = P.text $ [fmt|empty %w|] ty
  print (FPathNonAbsE   ty t _) = P.text $ [fmt|non-absolute %w: '%T'|] ty t
  print (FPathAbsE      ty t _) = P.text $ [fmt|absolute %w: '%T'|] ty t
  print (FPathNotADirE  ty t _) = P.text $ [fmt|%w lacks trailing /: '%T'|]  ty t
  print (FPathNotAFileE ty t _) = P.text $ [fmt|%w has trailing /: '%T'|]  ty t
  print (FPathComponentE ce ty t) =
    P.text $ [fmt|component error %T in %w '%T'|] ce ty t
  print (FPathRootDirE ty _)    = P.text $ [fmt|is root dir: %w|] ty
  print (FPathNotAPrefixE e)    = print e

------------------------------------------------------------

fPathEmptyE ∷ HasCallStack ⇒ TypeRep → FPathError
fPathEmptyE r = FPathEmptyE r callStack

_FPathEmptyE ∷ (AsFPathError ε, HasCallStack) ⇒ TypeRep → ε
_FPathEmptyE r = (_FPathError #) $ fPathEmptyE r

__FPathEmptyE__ ∷ (AsFPathError ε, MonadError ε η) ⇒ TypeRep → η α
__FPathEmptyE__ = throwError ∘ _FPathEmptyE

fPathNonAbsE ∷ HasCallStack ⇒ TypeRep → Text → FPathError
fPathNonAbsE r t = FPathNonAbsE r t callStack

_FPathNonAbsE ∷ (AsFPathError ε, HasCallStack) ⇒ TypeRep → Text → ε
_FPathNonAbsE r t = _FPathError # fPathNonAbsE r t

__FPathNonAbsE__ ∷ (AsFPathError ε,MonadError ε η) ⇒ TypeRep → Text → η α
__FPathNonAbsE__ r t = throwError $ _FPathNonAbsE r t

fPathAbsE ∷ HasCallStack ⇒ TypeRep → Text → FPathError
fPathAbsE r t = FPathAbsE r t callStack

_FPathAbsE ∷ (AsFPathError ε, HasCallStack) ⇒ TypeRep → Text → ε
_FPathAbsE r t = _FPathError # fPathAbsE r t

__FPathAbsE__ ∷ (AsFPathError ε,MonadError ε η) ⇒ TypeRep → Text → η α
__FPathAbsE__ r t = throwError $ _FPathAbsE r t

fPathNotADirE ∷ HasCallStack ⇒ TypeRep → Text → FPathError
fPathNotADirE r t = FPathNotADirE r t callStack

_FPathNotADirE ∷ (AsFPathError ε, HasCallStack) ⇒ TypeRep → Text → ε
_FPathNotADirE r t = _FPathError # fPathNotADirE r t

__FPathNotADirE__ ∷ (AsFPathError ε,MonadError ε η) ⇒ TypeRep → Text → η α
__FPathNotADirE__ r t = throwError $ _FPathNotADirE r t

fPathNotAFileE ∷ HasCallStack ⇒ TypeRep → Text → FPathError
fPathNotAFileE r t = FPathNotAFileE r t callStack

_FPathNotAFileE ∷ (AsFPathError ε, HasCallStack) ⇒ TypeRep → Text → ε
_FPathNotAFileE r t = _FPathError # fPathNotAFileE r t

__FPathNotAFileE__ ∷ (AsFPathError ε,MonadError ε η) ⇒ TypeRep → Text → η α
__FPathNotAFileE__ r t = throwError $ _FPathNotAFileE r t

_FPathComponentE ∷ (AsFPathError ε, HasCallStack) ⇒
                   FPathComponentError → TypeRep → Text → ε
_FPathComponentE ce r t = _FPathError # FPathComponentE ce r t

__FPathComponentE__ ∷ (AsFPathError ε,MonadError ε η) ⇒
                      FPathComponentError → TypeRep → Text → η α
__FPathComponentE__ ce r t = throwError $ _FPathComponentE ce r t

fPathRootDirE ∷ HasCallStack ⇒ TypeRep → FPathError
fPathRootDirE r = FPathRootDirE r callStack

_FPathRootDirE ∷ (AsFPathError ε, HasCallStack) ⇒ TypeRep → ε
_FPathRootDirE r = (_FPathError #) $ fPathRootDirE r

__FPathRootDirE__ ∷ (AsFPathError ε, MonadError ε η) ⇒ TypeRep → η α
__FPathRootDirE__ = throwError ∘ _FPathRootDirE

_FPathNotAPrefixE ∷ (AsFPathError ε) ⇒ FPathNotAPrefixError → ε
_FPathNotAPrefixE nape = _FPathError # FPathNotAPrefixE nape

__FPathNotAPrefixE__ ∷ (AsFPathError ε,MonadError ε η) ⇒
                       FPathNotAPrefixError → η α
__FPathNotAPrefixE__ nape = throwError $ _FPathNotAPrefixE nape

----------------------------------------

type instance Element FPathError = TypeRep
instance MonoFunctor FPathError where
  omap f (FPathEmptyE        r   cs) = FPathEmptyE       (f r)   cs
  omap f (FPathRootDirE      r   cs) = FPathRootDirE     (f r)   cs
  omap f (FPathAbsE          r t cs) = FPathAbsE         (f r) t cs
  omap f (FPathNonAbsE       r t cs) = FPathNonAbsE      (f r) t cs
  omap f (FPathNotADirE      r t cs) = FPathNotADirE     (f r) t cs
  omap f (FPathNotAFileE     r t cs) = FPathNotAFileE    (f r) t cs
  omap f (FPathComponentE  e r t   ) = FPathComponentE e (f r) t
  omap f (FPathNotAPrefixE e       ) = FPathNotAPrefixE (omap f e)

instance TMap FPathError where
  -- | fmap the Text bit of an FPathError
  tmap ∷ (Text → Text) → FPathError → FPathError
  tmap _ e@(FPathEmptyE      _ _) = e
  tmap _ e@(FPathRootDirE    _ _) = e
  tmap f (FPathAbsE          r t cs) = FPathAbsE         r (f t) cs
  tmap f (FPathNonAbsE       r t cs) = FPathNonAbsE      r (f t) cs
  tmap f (FPathNotADirE      r t cs) = FPathNotADirE     r (f t) cs
  tmap f (FPathNotAFileE     r t cs) = FPathNotAFileE    r (f t) cs
  tmap f (FPathComponentE  e r t   ) = FPathComponentE e r (f t)
  tmap f (FPathNotAPrefixE e       ) = FPathNotAPrefixE  (tmap f e)

mapTypeRepE ∷ (MonadError ε η, AsFPathError ε) ⇒
              (TypeRep → TypeRep) → Either FPathError α → η α
mapTypeRepE f = either (throwError ∘ (_FPathError #) ∘ omap f) return

mapTextE ∷ (MonadError ε η, AsFPathError ε) ⇒
              (Text → Text) → Either FPathError α → η α
mapTextE f = either (throwError ∘ (_FPathError #) ∘ tmap f) return

------------------------------------------------------------

data FPathIOError = FPIO_PATH_ERROR  FPathError
                  | FPIO_IO_ERROR    IOError
  deriving (Eq,Generic,NFData)

instance Exception FPathIOError

instance HasCallstack FPathIOError where
  callstack = lens (\ case (FPIO_PATH_ERROR fpe) → fpe ⊣ callstack
                           (FPIO_IO_ERROR   ioe) → ioe ⊣ callstack )
                   (\ fpioe cs →
                       case fpioe of
                         (FPIO_PATH_ERROR fpe) →
                           FPIO_PATH_ERROR $ fpe & callstack ⊢ cs
                         (FPIO_IO_ERROR ioe) →
                           FPIO_IO_ERROR $ ioe & callstack ⊢ cs
                   )

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

fpathIOErrorEither ∷ FPathIOError → Either IOError FPathError
fpathIOErrorEither (FPIO_IO_ERROR   e) = Left e
fpathIOErrorEither (FPIO_PATH_ERROR e) = Right e

-- that's all, folks! ----------------------------------------------------------
