module FPath.Error.FPathComponentError
  ( AsFPathComponentError(..), FPathComponentError(..)
  , __FPathCEmptyE__, __FPathCIllegalCharE__, __FPathCIllegalE__
  , fPathComponentEmptyE, fPathComponentIllegalCharE, fPathIllegalE
  )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Bool          ( Bool( False, True ) )
import Data.Char          ( Char )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( ($), id )
import Data.String        ( String )
import GHC.Generics       ( Generic )
import GHC.Stack          ( CallStack, HasCallStack, callStack )
import Text.Show          ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode  ( (≡) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

data FPathComponentError = FPathComponentEmptyE CallStack
                         | FPathComponentIllegalCharE Char String CallStack
                         | FPathComponentIllegalE     String CallStack
  deriving (Generic,NFData,Show)

instance Exception FPathComponentError

instance Eq FPathComponentError where
  (FPathComponentEmptyE _) == (FPathComponentEmptyE _) = True
  (FPathComponentIllegalE s _) == (FPathComponentIllegalE s' _) = s ≡ s'
  (FPathComponentIllegalCharE c s _) == (FPathComponentIllegalCharE c' s' _) =
    (c,s) ≡ (c',s')
  _ == _ = False

instance HasCallstack FPathComponentError where
  callstack = lens (\ case (FPathComponentEmptyE           cs) → cs
                           (FPathComponentIllegalCharE _ _ cs) → cs
                           (FPathComponentIllegalE     _   cs) → cs
                   )
                   (\ fpce cs →
                      case fpce of
                        (FPathComponentEmptyE           _) →
                          FPathComponentEmptyE cs
                        (FPathComponentIllegalCharE c s _) →
                          FPathComponentIllegalCharE c s cs
                        (FPathComponentIllegalE       s _) →
                          FPathComponentIllegalE s cs
                   )


----------------------------------------

class AsFPathComponentError ε where
  _FPathComponentError ∷ Prism' ε FPathComponentError

----------------------------------------

fPathComponentEmptyE ∷ HasCallStack ⇒ FPathComponentError
fPathComponentEmptyE = FPathComponentEmptyE callStack

fPathComponentIllegalCharE ∷ HasCallStack ⇒ Char → String → FPathComponentError
fPathComponentIllegalCharE c s = FPathComponentIllegalCharE c s callStack

fPathIllegalE ∷ HasCallStack ⇒ String → FPathComponentError
fPathIllegalE s = FPathComponentIllegalE s callStack

_FPathCEmptyE ∷ (AsFPathComponentError ε, HasCallStack) ⇒ ε
_FPathCEmptyE = (_FPathComponentError #) $ FPathComponentEmptyE callStack

--------------------

__FPathCEmptyE__ ∷ (AsFPathComponentError ε, MonadError ε η) ⇒ η α
__FPathCEmptyE__ = throwError $ _FPathCEmptyE

----------------------------------------

_FPathCIllegalCharE ∷ (AsFPathComponentError ε, HasCallStack) ⇒
                      Char → String → ε
_FPathCIllegalCharE c t =
  _FPathComponentError # FPathComponentIllegalCharE c t callStack

--------------------

__FPathCIllegalCharE__ ∷ (AsFPathComponentError ε,MonadError ε η) ⇒
                                 Char → String → η α
__FPathCIllegalCharE__ c t =
  throwError $ _FPathCIllegalCharE c t

----------------------------------------

_FPathCIllegalE ∷ (AsFPathComponentError ε, HasCallStack) ⇒ String → ε
_FPathCIllegalE t =
  _FPathComponentError # FPathComponentIllegalE t callStack

--------------------

__FPathCIllegalE__ ∷ (AsFPathComponentError ε,MonadError ε η) ⇒ String → η α
__FPathCIllegalE__ t =
  throwError $ _FPathCIllegalE t

----------------------------------------

instance AsFPathComponentError FPathComponentError where
  _FPathComponentError = id

----------------------------------------

instance Printable FPathComponentError where
  print (FPathComponentEmptyE _)           = P.text [fmt|empty pathComponent|]
  print (FPathComponentIllegalCharE c t _) =
    let repr '\0' = "NUL"
        repr '/'  = "SLASH"
        repr x    = [x]
     in P.text $ [fmt|pathComponent contains %s: '%s'|] (repr c) t
  print (FPathComponentIllegalE t _) =
    P.text $ [fmt|illegal pathComponent: "%s"|] t

-- that's all, folks! ----------------------------------------------------------
