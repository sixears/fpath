{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.Error.FPathComponentError
  ( AsFPathComponentError(..), FPathComponentError(..)
  , __FPathCEmptyE__, __FPathCIllegalCharE__ )
where

-- base --------------------------------

import Data.Char      ( Char )
import Data.Eq        ( Eq )
import Data.Function  ( ($), id )
import Text.Show      ( Show( show ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism' )

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

import FPath.Util  ( (⋕) )

--------------------------------------------------------------------------------

data FPathComponentError = FPathComponentEmptyE
                         | FPathComponentIllegalCharE Char Text
  deriving (Eq, Show)

class AsFPathComponentError ε where
  _FPathComponentError ∷ Prism' ε FPathComponentError

_FPathCEmptyE ∷ AsFPathComponentError ε ⇒ ε
_FPathCEmptyE = (_FPathComponentError ⋕) $ FPathComponentEmptyE

__FPathCEmptyE__ ∷ (AsFPathComponentError ε, MonadError ε η) ⇒ η α
__FPathCEmptyE__ = throwError $ _FPathCEmptyE

_FPathCIllegalCharE ∷ AsFPathComponentError ε ⇒ Char → Text → ε
_FPathCIllegalCharE c t =
  _FPathComponentError ⋕ FPathComponentIllegalCharE c t

__FPathCIllegalCharE__ ∷ (AsFPathComponentError ε,MonadError ε η) ⇒
                                 Char → Text → η α
__FPathCIllegalCharE__ c t =
  throwError $ _FPathCIllegalCharE c t

instance AsFPathComponentError FPathComponentError where
  _FPathComponentError = id

instance Printable FPathComponentError where
  print (FPathComponentEmptyE)           = P.text [fmt|empty pathComponent|]
  print (FPathComponentIllegalCharE c t) =
    let repr '\0' = "NUL"
        repr '/'  = "SLASH"
        repr x    = [x]
     in P.text $ [fmt|pathComponent contains %s: '%T'|] (repr c) t

-- that's all, folks! ----------------------------------------------------------
