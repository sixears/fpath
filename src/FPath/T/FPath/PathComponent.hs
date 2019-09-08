{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath.PathComponent
  ( tests )
where

-- base --------------------------------

import Data.Function    ( ($), const )
import Data.Maybe       ( Maybe( Just, Nothing ) )
import Data.String      ( String )
import Numeric.Natural  ( Natural )
import System.IO        ( IO )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode      ( (≢) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), fromString, parseString, toText )

-- genvalidity -------------------------

import Data.GenValidity  ( genValid )

-- genvalidity-property ----------------

import Test.Validity.GenValidity.Property  ( genGeneratesValid )

-- more-unicode ------------------------

import Data.MoreUnicode.Tasty      ( (≟) )
import Data.MoreUnicode.Semigroup  ( (◇) )

-- QuickCheck --------------------------

import Test.QuickCheck.Property  ( property )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Arbitrary( arbitrary ), Gen, Property
                              , shrink, testProperty
                              )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  FPath.PathComponent

import FPath.PathComponent  ( PathComponent
                            , addExt, ext, pc, splitExt, toUpper, updateExt )

import FPath.T.Common       ( doTest, doTestR, doTestS )

--------------------------------------------------------------------------------

pathCArbitraryTests ∷ TestTree
pathCArbitraryTests =
  let propNonEmpty ∷ PathComponent → Property
      propNonEmpty p = property $ toText p ≢ ""
   in testProperty "non-empty" propNonEmpty

pathCTextualTests ∷ TestTree
pathCTextualTests =
  let nothin'     ∷ Maybe PathComponent
      nothin'     = Nothing
      success e s = testCase s $ Parsed e  ≟ parseString s
      fail s      = testCase s $ nothin'   ≟ fromString s
   in testGroup "Textual" [ success [pc|etc|]   "etc"
                          , success [pc|pam.d|] "pam.d"
                          , success [pc|.d|] ".d"
                          , success [pc|..d|] "..d"
                          , success [pc|d..|] "d.."
                          , success [pc|d.|] "d."
                          , fail "."
                          , fail "/."
                          , fail "./"
                          , fail "/./"
                          , fail ".."
                          , fail "/.."
                          , fail "../"
                          , fail "/../"
                          , fail "/etc"
                          , fail "etc/"
                          , fail "e/c"
                          , fail "\0etc"
                          , fail "etc\0"
                          , fail "e\0c"
                          ]

pathCValidityTests ∷ TestTree
pathCValidityTests =
  let genValidPC ∷ Gen PathComponent
      genValidPC = genValid
      arbPC ∷ Gen PathComponent
      arbPC = arbitrary
   in testGroup "Validity"
                [
                  testProperty "genValid"  $ genGeneratesValid genValidPC shrink
                , testProperty "arbitrary" $ genGeneratesValid arbPC      shrink
                ]

----------------------------------------

addExtTests ∷ TestTree
addExtTests =
  testGroup "addExt"
    [ testCase "foo.bar" $ [pc|foo.bar|] ≟ addExt [pc|foo|] [pc|bar|]
    , testCase "f.o.bar" $ [pc|f.o.bar|] ≟ [pc|f.o|] `addExt` [pc|bar|]
    , testCase "f.o.b.r" $ [pc|fo..b.r|] ≟ [pc|fo.|] `addExt` [pc|b.r|]
    ]

splitExtTests ∷ TestTree
splitExtTests =
  testGroup "splitExt"
    [ testCase "foo.bar" $ ([pc|foo|],Just [pc|bar|]) ≟ splitExt [pc|foo.bar|]
    , testCase "f.o.bar" $ ([pc|f.o|],Just [pc|bar|]) ≟ splitExt [pc|f.o.bar|]
    , testCase "foo."    $ ([pc|foo.|],Nothing)       ≟ splitExt [pc|foo.|]
    , testCase "foo"     $ ([pc|foo|],Nothing)        ≟ splitExt [pc|foo|]
    ]

extGetterTests ∷ TestTree
extGetterTests =
  testGroup "getter" [ testCase ".bar" $ Just [pc|bar|] ≟ ext [pc|foo.bar|]
                     , testCase "-"    $ Nothing        ≟ ext [pc|foo|]
                     , testCase "."    $ Nothing        ≟ ext [pc|foo.|]
                     , testCase "baz"  $ Just [pc|baz|] ≟ ext [pc|f.b.x.baz|]
                     ]

extSetterTests ∷ TestTree
extSetterTests =
  testGroup "updateExt (const)"
    [ testCase ".bar -> .baz" $
          [pc|foo.baz|] ≟ updateExt (const [pc|baz|]) [pc|foo.bar|]
    , testCase ".x.bar -> .x.baz" $
          [pc|foo.x.baz|] ≟ updateExt (const [pc|baz|]) [pc|foo.x.bar|]
    , testCase "'' -> ''" $
          [pc|foo|] ≟ updateExt (const [pc|baz|]) [pc|foo|]
    , testCase ". -> ." $
          [pc|foo.|] ≟ updateExt (const [pc|baz|]) [pc|foo.|]
    ]

updateExtTests ∷ TestTree
updateExtTests =
  testGroup "updateExt"
    [ testCase ".baz -> .BAR" $
        [pc|fo.BAZ|] ≟ updateExt toUpper [pc|fo.baz|]
    , testCase ".x.b -> .x.B" $
        [pc|fo.x.B|] ≟ updateExt toUpper [pc|fo.x.b|]
    , testCase ".x -> .xy"    $
        [pc|fo.xy|]  ≟ updateExt (◇ [pc|y|]) [pc|fo.x|]
    , testCase ".    -> ."    $
        [pc|fo.|]    ≟ updateExt (◇ [pc|y|]) [pc|fo.|]

    ]

extTests ∷ TestTree
extTests = testGroup "ext" [ addExtTests, splitExtTests, extGetterTests
                           , extSetterTests, updateExtTests ]

----------------------------------------

tests ∷ TestTree
tests =
  testGroup "PathComponent" [ pathCArbitraryTests
                            , pathCTextualTests, pathCValidityTests
                            , extTests
                            , FPath.PathComponent.tests
                            ]

----------------------------------------

-- Cannot use Fluffy.Tasty here, as we will be a dependency of Fluffy...

_test ∷ IO ()
_test = doTest tests

--------------------

_tests ∷ String → IO ()
_tests = doTestS tests

_testr ∷ String → Natural → IO ()
_testr = doTestR tests

-- that's all, folks! ----------------------------------------------------------
