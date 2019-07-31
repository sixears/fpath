{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.Common
  ( doTest, doTestR, doTestS
  , propInvertibleString, propInvertibleText, propInvertibleUtf8 )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Control.Monad    ( return )
import Data.Bool        ( Bool( False ) )
import Data.Eq          ( Eq )
import Data.Function    ( ($) )
import Data.Maybe       ( Maybe( Just, Nothing ), fromMaybe )
import Data.String      ( String )
import Numeric.Natural  ( Natural )
import System.IO        ( IO )
import Text.Show        ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), Textual, parseString
                     , parseText, parseUtf8, toString, toText, toUtf8 )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad           ( (⪻) )
import Data.MoreUnicode.Tasty           ( (≣) )

-- tasty -------------------------------

import Test.Tasty           ( TestTree, defaultIngredients )
import Test.Tasty.Options   ( OptionSet, singleOption )
import Test.Tasty.Runners   ( TestPattern, defaultMainWithIngredients
                            , parseTestPattern, tryIngredients )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Property, QuickCheckReplay( QuickCheckReplay ) )

--------------------------------------------------------------------------------

propInvertibleString ∷ (Eq α, Show α, Textual α) ⇒ α → Property
propInvertibleString d =
  parseString (toString d) ≣ Parsed d

propInvertibleText ∷ (Eq α, Show α, Textual α) ⇒ α → Property
propInvertibleText d =
  parseText (toText d) ≣ Parsed d

propInvertibleUtf8 ∷ (Eq α, Show α, Textual α) ⇒ α → Property
propInvertibleUtf8 d =
  parseUtf8 (toUtf8 d) ≣ Parsed d

----------------------------------------

-- Cannot use Fluffy.Tasty here, as we will be a dependency of Fluffy...

doTest ∷ TestTree → IO ()
doTest tests = defaultMainWithIngredients defaultIngredients tests

--------------------

doTestS ∷ TestTree → String → IO ()
doTestS tests s =
  let tryOpt ∷ TestPattern → TestTree → Maybe (IO Bool)
      tryOpt = tryIngredients defaultIngredients ∘ singleOption
   in return () ⪻ case parseTestPattern s of
                    Nothing → return False
                    Just p  → fromMaybe (return False) $ tryOpt p tests

doTestR ∷ TestTree → String → Natural → IO ()
doTestR tests s r =
  let replayO ∷ Natural → OptionSet
      replayO = singleOption ∘ QuickCheckReplay ∘ Just ∘ fromIntegral
      tryOpt ∷ TestPattern → TestTree → Maybe (IO Bool)
      tryOpt p = tryIngredients defaultIngredients $
                     singleOption p ⊕ replayO r
   in return () ⪻ case parseTestPattern s of
                    Nothing → return False
                    Just p  → fromMaybe (return False) $ tryOpt p tests

-- that's all, folks! ----------------------------------------------------------
