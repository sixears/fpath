{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module NonEmptyContainers.T.SeqNE
  ( tests )
where

import Prelude  ( (+), (*), fromIntegral )

-- base --------------------------------

import Control.Monad    ( return )
import Data.Bool        ( Bool( False ) )
import Data.Function    ( ($) )
import Data.Maybe       ( Maybe( Just, Nothing ), fromMaybe )
import Data.String      ( String )
import Numeric.Natural  ( Natural )
import System.IO        ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- mono-traversable --------------------

import Data.MonoTraversable  ( ofoldr, ofoldr1Ex, omap, otraverse )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad  ( (⪻) )
import Data.MoreUnicode.Tasty  ( (≟) )

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

import NonEmptyContainers.SeqNE          ( (⋖) )

--------------------------------------------------------------------------------

monoFunctorTests ∷ TestTree
monoFunctorTests =
  let one = 1 ∷ Natural
   in testGroup "MonoFunctor"
                [ testCase "3"     $ 3 ⋖ []    ≟ omap (*3) (one ⋖ [])
                , testCase "6,2"   $ 6 ⋖ [2]   ≟ omap (*2) (3 ⋖ [one])
                , testCase "2,3,4" $ 2 ⋖ [3,4] ≟ omap (+1) (one ⋖ [2,3])
                ]
----------------------------------------

monoFoldableTests ∷ TestTree
monoFoldableTests =
  let one = 1 ∷ Natural
   in testGroup "MonoFoldable"
                [ testCase "1" $ 1 ≟ ofoldr (+) 0 (one ⋖ [])
                , testCase "6" $ 6 ≟ ofoldr1Ex (+) (one ⋖ [2,3])
                ]
----------------------------------------

monoTraversableTests ∷ TestTree
monoTraversableTests =
  let one = 1 ∷ Natural
   in testGroup "MonoTraversable"
                [ testCase "1" $ [2⋖[4],2⋖[2],1⋖[4],1⋖[2]]
                               ≟ otraverse (\ x → [2*x,x])  (one ⋖ [2])
                ]
----------------------------------------

tests ∷ TestTree
tests = testGroup "SeqNE" [ monoFunctorTests, monoFoldableTests
                          , monoTraversableTests ]

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
