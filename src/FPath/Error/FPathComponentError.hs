{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.Error.FPathComponentError
  ( AsFPathComponentError(..), FPathComponentError(..)
  , __FPathCEmptyE__, __FPathCIllegalCharE__, __FPathCIllegalE__ )
where

-- base --------------------------------

import Data.Char      ( Char )
import Data.Eq        ( Eq )
import Data.Function  ( ($), id )
import Data.String    ( String )
import Text.Show      ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

data FPathComponentError = FPathComponentEmptyE
                         | FPathComponentIllegalCharE Char String
                         | FPathComponentIllegalE     String
  deriving (Eq, Show)

----------------------------------------

class AsFPathComponentError ε where
  _FPathComponentError ∷ Prism' ε FPathComponentError

----------------------------------------

_FPathCEmptyE ∷ AsFPathComponentError ε ⇒ ε
_FPathCEmptyE = (_FPathComponentError #) $ FPathComponentEmptyE

--------------------

__FPathCEmptyE__ ∷ (AsFPathComponentError ε, MonadError ε η) ⇒ η α
__FPathCEmptyE__ = throwError $ _FPathCEmptyE

----------------------------------------

_FPathCIllegalCharE ∷ AsFPathComponentError ε ⇒ Char → String → ε
_FPathCIllegalCharE c t =
  _FPathComponentError # FPathComponentIllegalCharE c t

--------------------

__FPathCIllegalCharE__ ∷ (AsFPathComponentError ε,MonadError ε η) ⇒
                                 Char → String → η α
__FPathCIllegalCharE__ c t =
  throwError $ _FPathCIllegalCharE c t

----------------------------------------

_FPathCIllegalE ∷ AsFPathComponentError ε ⇒ String → ε
_FPathCIllegalE t =
  _FPathComponentError # FPathComponentIllegalE t

--------------------

__FPathCIllegalE__ ∷ (AsFPathComponentError ε,MonadError ε η) ⇒ String → η α
__FPathCIllegalE__ t =
  throwError $ _FPathCIllegalE t

----------------------------------------

instance AsFPathComponentError FPathComponentError where
  _FPathComponentError = id

----------------------------------------

instance Printable FPathComponentError where
  print (FPathComponentEmptyE)           = P.text [fmt|empty pathComponent|]
  print (FPathComponentIllegalCharE c t) =
    let repr '\0' = "NUL"
        repr '/'  = "SLASH"
        repr x    = [x]
     in P.text $ [fmt|pathComponent contains %s: '%s'|] (repr c) t
  print (FPathComponentIllegalE t) =
    P.text $ [fmt|illegal pathComponent: "%s"|] t

-- that's all, folks! ----------------------------------------------------------
