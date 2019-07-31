{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath
  ( tests )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Function        ( ($), const )
import Data.Functor         ( fmap )
import Data.Maybe           ( Maybe( Nothing ) )
import Data.String          ( String )
import Numeric.Natural      ( Natural )
import System.IO            ( IO )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode  ( (≢) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), fromString, parseString, toText )

-- fluffy ------------------------------

import NonEmptyContainers.SeqNE  ( SeqNE, (⋖) )

-- genvalidity -------------------------

import Data.GenValidity  ( genValid )

-- genvalidity-property ----------------

import Test.Validity.GenValidity.Property  ( genGeneratesValid )

-- mono-traversable --------------------

import Data.MonoTraversable  ( omap )
import Data.Sequences        ( reverse )

-- more-unicode ------------------------

import Data.MoreUnicode.Function        ( (⅋) )
import Data.MoreUnicode.Lens            ( (⊣), (⊢), (⊧) )
import Data.MoreUnicode.MonoTraversable ( (⪦), (⪧) )
import Data.MoreUnicode.Semigroup       ( (◇) )
import Data.MoreUnicode.Tasty           ( (≟) )

-- QuickCheck --------------------------

import Test.QuickCheck.Property  ( property )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Arbitrary( arbitrary ), Gen, Property
                              , shrink, testProperty
                              )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  FPath.T.FPath.AbsDir
import qualified  FPath.T.FPath.RelDir

import FPath                   ( AbsDir, NonRootAbsDir, absdirN, seqNE )
import FPath.PathComponent     ( PathComponent, pc, toUpper )

import FPath.T.Common          ( doTest, doTestR, doTestS
                               , propInvertibleString, propInvertibleText
                               , propInvertibleUtf8 )
import FPath.T.FPath.TestData  ( etcN, pamdN, wgmN )

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

pathComponentTests ∷ TestTree
pathComponentTests =
  testGroup "PathComponent" [ pathCArbitraryTests
                            , pathCTextualTests, pathCValidityTests ]

------------------------------------------------------------

nonRootAbsDirSeqGetTests ∷ TestTree
nonRootAbsDirSeqGetTests =
  let infix 4 ??
      (??) ∷ SeqNE PathComponent → NonRootAbsDir → Assertion
      e ?? g = e ≟ g ⊣ seqNE
   in testGroup "seqNE" [ testCase "etc"   $ pure [pc|etc|] ?? etcN
                        , testCase "pam.d" $ [pc|etc|] ⋖ [[pc|pam.d|]] ?? pamdN
                        , testCase "wgM" $ [pc|w|] ⋖ [[pc|g|],[pc|M|]] ?? wgmN
                        ]

nonRootAbsDirMonoFunctorTests ∷ TestTree
nonRootAbsDirMonoFunctorTests =
  testGroup "MonoFunctor"
            [ testCase "usr" $
                    [absdirN|/usr/|] ≟ omap (const [pc|usr|]) etcN
            , testCase "wgm.d" $ [absdirN|/w.d/g.d/M.d/|] ≟ (◇ [pc|.d|]) ⪧ wgmN
            , testCase "WGM" $ [absdirN|/W/G/M/|] ≟  wgmN ⪦ toUpper
            ]

nonRootAbsDirSeqSetTests ∷ TestTree
nonRootAbsDirSeqSetTests =
  testGroup "seqNE" [ testCase "usr" $
                            [absdirN|/usr/|] ≟ etcN ⅋ seqNE ⊢ pure [pc|usr|]
                    , testCase "dpam" $
                            [absdirN|/pam.d/etc/|] ≟ pamdN ⅋ seqNE ⊧ reverse
                    , testCase "wgm.d" $
                            [absdirN|/w.d/g.d/M.d/|]
                          ≟ wgmN ⅋ seqNE ⊧ fmap (◇ [pc|.d|])

                    , testCase "WGM" $
                            [absdirN|/W/G/M/|] ≟ wgmN ⅋ seqNE ⊧ fmap toUpper
                        ]

nonRootAbsDirSeqTests ∷ TestTree
nonRootAbsDirSeqTests =
  testGroup "nonRootAbsDirSeqTests" [ nonRootAbsDirSeqGetTests
                                    , nonRootAbsDirSeqSetTests
                                    , nonRootAbsDirMonoFunctorTests
                                    ]

nonRootAbsDirPrintableTests ∷ TestTree
nonRootAbsDirPrintableTests =
  testGroup "printable"
            [ testCase "etcN"  $ "/etc/"       ≟ toText etcN
            , testCase "pamdN" $ "/etc/pam.d/" ≟ toText pamdN
            , testCase "wgmN"  $ "/w/g/M/"     ≟ toText wgmN
            ]

nonRootAbsDirTextualTests ∷ TestTree
nonRootAbsDirTextualTests =
  let nothin'     ∷ Maybe AbsDir
      nothin'     = Nothing
      success e s = testCase s $ Parsed e  ≟ parseString s
      fail s      = testCase s $ nothin'   ≟ fromString s
   in testGroup "Textual" [ success [absdirN|/etc/|]       "/etc/"
                          , success [absdirN|/etc/pam.d/|] "/etc/pam.d/"
                          , fail "/etc"
                          , fail "/etc/pam.d"
                          , fail "etc/"
                          , fail "etc/pam.d"
                          , fail "/etc//pam.d/"
                          , fail "e/c"
                          , fail "\0etc"
                          , fail "etc\0"
                          , fail "e\0c"
                          ]

nonRootAbsDirTextualPrintableTests ∷ TestTree
nonRootAbsDirTextualPrintableTests =
  testGroup "textual invertibility"
            [ testProperty "parseString - toString"
                           (propInvertibleString @NonRootAbsDir)
            , testProperty "parseText - toText"
                           (propInvertibleText @NonRootAbsDir)
            , testProperty "parseUtf8 - toUtf8"
                           (propInvertibleUtf8 @NonRootAbsDir)
            ]

nonRootAbsDirTests ∷ TestTree
nonRootAbsDirTests =
  testGroup "nonRootAbsDir" [ nonRootAbsDirSeqTests
                            , nonRootAbsDirPrintableTests
                            , nonRootAbsDirTextualTests
                            , nonRootAbsDirTextualPrintableTests
                            ]

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath" [ pathComponentTests
                          , FPath.T.FPath.AbsDir.tests
                          , nonRootAbsDirTests
                          , FPath.T.FPath.RelDir.tests
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
