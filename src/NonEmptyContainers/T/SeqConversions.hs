{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module NonEmptyContainers.T.SeqConversions
  ( tests )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( return )
import Data.Bool            ( Bool( False ) )
import Data.Function        ( ($) )
import Data.Maybe           ( Maybe( Just, Nothing ), fromMaybe )
import Data.String          ( String )
import Numeric.Natural      ( Natural )
import System.IO            ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import Data.Sequence  ( Seq )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad    ( (⪻) )
import Data.MoreUnicode.Monoid   ( ф )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.Tasty    ( (≟) )

-- tasty -------------------------------

import Test.Tasty           ( TestTree, defaultIngredients, testGroup )
import Test.Tasty.Options   ( OptionSet, singleOption )
import Test.Tasty.Runners   ( TestPattern, defaultMainWithIngredients
                            , parseTestPattern, tryIngredients )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( QuickCheckReplay( QuickCheckReplay ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import NonEmptyContainers.SeqConversions  ( stripPrefix )
import NonEmptyContainers.SeqNE           ( (⪬) )

--------------------------------------------------------------------------------

stripPrefixTests ∷ TestTree
stripPrefixTests =
  let ones   = pure 1  ∷ Seq ℕ
      twos   = pure 2  ∷ Seq ℕ
      onetwo = 1 ⪬ [2] ∷ Seq ℕ
   in testGroup "stripPrefix"
                [ testCase "null"   $ Just (pure 1) ≟ stripPrefix ф ones
                , testCase "pfx"    $ Just (pure 2) ≟ stripPrefix ones onetwo
                , testCase "no pfx" $ Nothing       ≟ stripPrefix twos ones
                , testCase "equal"  $ Just ф        ≟ stripPrefix ones ones
                , testCase "longer" $ Nothing       ≟ stripPrefix onetwo ones
                ]

----------------------------------------

tests ∷ TestTree
tests = testGroup "SeqNE" [ stripPrefixTests ]

----------------------------------------

_test ∷ IO ()
_test = defaultMainWithIngredients defaultIngredients tests

_tests ∷ String → IO ()
_tests s = let tryOpt ∷ TestPattern → TestTree → Maybe (IO Bool)
               tryOpt = tryIngredients defaultIngredients ∘ singleOption
            in return () ⪻ case parseTestPattern s of
                             Nothing → return False
                             Just p  → fromMaybe (return False) $ tryOpt p tests

_testr ∷ String → Natural → IO ()
_testr s r = let replayO ∷ Natural → OptionSet
                 replayO = singleOption ∘ QuickCheckReplay ∘ Just ∘ fromIntegral
                 tryOpt ∷ TestPattern → TestTree → Maybe (IO Bool)
                 tryOpt p = tryIngredients defaultIngredients $
                                singleOption p ⊕ replayO r
              in return () ⪻ case parseTestPattern s of
                               Nothing → return False
                               Just p  → fromMaybe (return False) $ tryOpt p tests

-- that's all, folks! ----------------------------------------------------------
