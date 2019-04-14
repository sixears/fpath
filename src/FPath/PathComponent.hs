{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.PathComponent
  ( PathComponent, parsePathC, pathComponent, pc, qPathC )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Monad   ( return )
import Data.Bifunctor  ( first )
import Data.Bool       ( otherwise )
import Data.Either     ( either )
import Data.Eq         ( Eq )
import Data.List       ( null )
import Data.Function   ( ($), id )
import Data.String     ( String )
import Text.Show       ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- template-haskell --------------------

import Language.Haskell.TH  ( ExpQ, appE, conE, litE, stringL, varE )

-- text --------------------------------

import Data.Text  ( Text, any, pack, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Error.FPathComponentError
           ( AsFPathComponentError, FPathComponentError
           , __FPathCEmptyE__, __FPathCIllegalCharE__ )
import FPath.Util  ( QuasiQuoter, __ERROR'__, mkQuasiQuoterExp )

--------------------------------------------------------------------------------

{- | A single component in a Path, that is, a directory or file ---
     notably no slashes are allowed (or nul chars); must not be empty
 -}
newtype PathComponent = PathComponent Text
  deriving Eq

instance Show PathComponent where
  show (PathComponent t) = "[pathComponent|" ⊕ unpack t ⊕ "|]"

instance Printable PathComponent where
  print (PathComponent t) = P.text t

parsePathC ∷ (Printable ρ, AsFPathComponentError ε, MonadError ε η) ⇒
             ρ → η PathComponent
parsePathC (toText → "")                 = __FPathCEmptyE__
parsePathC (toText → t) | any (≡ '\0') t = __FPathCIllegalCharE__ '\0' t
                        | any (≡ '/')  t = __FPathCIllegalCharE__ '/' t
                        | otherwise      = return $ PathComponent t

parsePathC' ∷ (Printable ρ, MonadError FPathComponentError η) ⇒
              ρ → η PathComponent
parsePathC' = parsePathC

__parsePathC__ ∷ Printable τ ⇒ τ → PathComponent
__parsePathC__ = either __ERROR'__ id ∘ parsePathC'

__parsePathC'__ ∷ String → PathComponent
__parsePathC'__ = __parsePathC__

qPathC ∷ Printable ρ ⇒ ρ → ExpQ
-- qPathC t = either __ERROR'__ (\ (PathComponent p) → appE (conE 'PathComponent) (appE (varE 'pack) $ litE (stringL $ unpack p))) $ parsePathC' t
qPathC t = either __ERROR'__ (\ (PathComponent p) → appE (conE 'PathComponent) (appE (varE 'pack) $ litE (stringL $ unpack p))) $ parsePathC' t


{-
qPathC ∷ String → ExpQ
qPathC t | null t         = error "empty pathComponent"
         | any (≡ '\0') t = error $ "pathComponent contains NUL: '" ⊕ t ⊕ "'"
         | any (≡ '/')  t = error $ "pathComponent contains SLASH: '" ⊕ t ⊕ "'"
         | otherwise      = appE (conE 'PathComponent)
                                 (appE (varE 'pack) $ litE (stringL t))
-}

{- | quasi-quoter for PathComponent -}
pathComponent ∷ QuasiQuoter
pathComponent = mkQuasiQuoterExp "pathComponent" (\ s → ⟦ __parsePathC'__ s ⟧) -- qPathC

{- | abbreviation for `pathComponent` -}
pc ∷ QuasiQuoter
pc = pathComponent

-- that's all, folks! ----------------------------------------------------------
