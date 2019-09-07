{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module FPath.T.FPath.NonRootAbsDir
  ( tests )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Bool            ( Bool( False, True ) )
import Data.Function        ( ($), (&), const )
import Data.Functor         ( fmap )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Ord             ( Ordering( GT ), (<), comparing )
import Data.String          ( String )
import Numeric.Natural      ( Natural )
import System.IO            ( IO )
import Text.Show            ( show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), fromString, parseString, toText )

-- mono-traversable --------------------

import Data.MonoTraversable  ( maximumByEx, minimumByEx, oall, oany
                             , ocompareLength, oelem, ofoldl', ofoldl1Ex'
                             , ofoldlM, ofoldMap, ofoldMap1Ex, ofoldr, ofoldr1Ex
                             , olength, olength64, omap, onotElem, onull
                             , otoList, unsafeHead, unsafeLast
                             )
import Data.Sequences        ( reverse )

-- more-unicode ------------------------

import Data.MoreUnicode.Function        ( (⅋) )
import Data.MoreUnicode.Lens            ( (⊣), (⊢), (⊧), (⊩), (⩼), (##) )
import Data.MoreUnicode.MonoTraversable ( (⪦), (⪧) )
import Data.MoreUnicode.Natural         ( ℕ )
import Data.MoreUnicode.Semigroup       ( (◇) )
import Data.MoreUnicode.Tasty           ( (≟), (≣) )

-- non-empty-containers ----------------

import NonEmptyContainers.IsNonEmpty        ( fromNonEmpty,nonEmpty,toNonEmpty )
import NonEmptyContainers.SeqNE             ( SeqNE, (⋖) )
import NonEmptyContainers.SeqNEConversions  ( seqNE )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text --------------------------------

import qualified  Data.Text  as  Text

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath        ( filepath )
import FPath.AbsDir            ( AbsDir, NonRootAbsDir, absdirN )
import FPath.Parent            ( parent, parentMay )
import FPath.PathComponent     ( PathComponent, pc, toUpper )
import FPath.T.Common          ( doTest, doTestR, doTestS
                               , propInvertibleString, propInvertibleText
                               , propInvertibleUtf8 )
import FPath.T.FPath.TestData  ( etc, etcN, pamd, pamdN, root, wgm, wgmN )

--------------------------------------------------------------------------------

absdirNQQTests ∷ TestTree
absdirNQQTests =
  testGroup "reldir"
            [ testCase "/etc"       $ etcN  ≟ [absdirN|/etc/|]
            , testCase "/etc/pam.d" $ pamdN ≟ [absdirN|/etc/pam.d/|]
            , testCase "/w/g/M"     $ wgmN  ≟ [absdirN|/w/g/M/|]
            ]

absDirNShowTests ∷ TestTree
absDirNShowTests =
  let fromNonEmptyT = "NonEmptyContainers.IsNonEmpty.fromNonEmpty"
      pcT t         = "PathComponent \"" ⊕ t ⊕ "\""
      etcT          = pcT "etc"
      pamdT         = pcT "pam.d"
      nonRT         = "NonRootAbsDir "

      etcNShow  = nonRT ⊕ fromNonEmptyT ⊕ " (" ⊕ pcT "etc" ⊕ " :| [])"
      pamdNShow = nonRT ⊕ fromNonEmptyT ⊕ " (" ⊕ etcT ⊕ " :| [" ⊕ pamdT ⊕ "])"

   in testGroup "show"
                [ testCase "etc"   $ etcNShow  ≟ show etcN
                , testCase "pam.d" $ pamdNShow ≟ show pamdN
                ]

absDirNIsMonoSeqNEGetterTests ∷ TestTree
absDirNIsMonoSeqNEGetterTests =
  let infix 4 ??
      (??) ∷ SeqNE PathComponent → NonRootAbsDir → Assertion
      e ?? g = e ≟ g ⊣ seqNE
   in testGroup "seqNE" [ testCase "etc"   $ pure [pc|etc|] ?? etcN
                        , testCase "pam.d" $ [pc|etc|] ⋖ [[pc|pam.d|]] ?? pamdN
                        , testCase "wgM" $ [pc|w|] ⋖ [[pc|g|],[pc|M|]] ?? wgmN
                        ]

absDirNIsMonoSeqNESetterTests ∷ TestTree
absDirNIsMonoSeqNESetterTests =
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

absDirNMonoFunctorTests ∷ TestTree
absDirNMonoFunctorTests =
  testGroup "MonoFunctor"
            [ testCase "usr" $
                    [absdirN|/usr/|] ≟ omap (const [pc|usr|]) etcN
            , testCase "wgm.d" $ [absdirN|/w.d/g.d/M.d/|] ≟ (◇ [pc|.d|]) ⪧ wgmN
            , testCase "WGM" $ [absdirN|/W/G/M/|] ≟  wgmN ⪦ toUpper
            ]

absDirNMonoFoldableTests ∷ TestTree
absDirNMonoFoldableTests =
  testGroup "MonoFoldable"
            [ testCase "ofoldMap" $
                "w-g-M-" ≟ ofoldMap ((⊕ "-") ∘ toText) wgmN
            , testCase "ofoldr" $
                "w-g-M-ф" ≟ ofoldr (\ a b → toText a ⊕ "-" ⊕ b) "ф" wgmN
            , testCase "ofoldl'" $
                "ф-w-g-M" ≟ ofoldl' (\ b a → b ⊕ "-" ⊕ toText a) "ф" wgmN
            , testCase "otoList" $
                [ [pc|w|], [pc|g|], [pc|M|] ] ≟ otoList wgmN
            , testCase "oall (F)" $
                False ≟ oall (Text.any (≡ 'r' ) ∘ toText) wgmN
            , testCase "oall (T)" $
                True ≟ oall ((< 6) ∘ Text.length ∘ toText) wgmN
            , testCase "oany (F)" $
                False ≟ oany (Text.any (≡ 'x' ) ∘ toText) wgmN
            , testProperty "onull" (\ (x ∷ NonRootAbsDir) → False ≣ onull x)
            , testCase "olength" $
                3 ≟ olength wgmN
            , testCase "olength64" $
                1 ≟ olength64 etcN
            , testCase "ocompareLength" $
               GT ≟ ocompareLength wgmN (2 ∷ ℕ)
            , testCase "ofoldlM" $
                  Just [[pc|M|],[pc|g|],[pc|w|]]
                ≟ ofoldlM (\ a e → Just $ e : a) [] wgmN
            , testCase "ofoldMap1Ex" $
                [[pc|w|],[pc|g|],[pc|M|]] ≟ ofoldMap1Ex pure wgmN
            , testCase "ofoldr1Ex" $
                [pc|wgM|] ≟ ofoldr1Ex (◇) wgmN
            , testCase "ofoldl1Ex'" $
                [pc|wgM|] ≟ ofoldl1Ex' (◇) wgmN
            , testCase "unsafeHead" $
                [pc|w|] ≟ unsafeHead wgmN
            , testCase "unsafeLast" $
                [pc|M|] ≟ unsafeLast wgmN
            , testCase "maximumByEx" $
                [pc|w|] ≟ maximumByEx (comparing toText) wgmN
            , testCase "minimumByEx" $
                [pc|M|] ≟ minimumByEx (comparing toText) wgmN
            , testCase "oelem (T)" $
                True ≟ oelem [pc|g|] wgmN
            , testCase "oelem (F)" $
                False ≟ oelem [pc|x|] wgmN
            , testCase "onotElem (T)" $
                True ≟ onotElem [pc|x|] wgmN
            , testCase "onotElem (F)" $
                False ≟ onotElem [pc|g|] wgmN
            ]

absDirNIsMonoSeqNETests ∷ TestTree
absDirNIsMonoSeqNETests =
  testGroup "absDirNIsMonoSeqNETests" [ absDirNIsMonoSeqNEGetterTests
                                      , absDirNIsMonoSeqNESetterTests ]

absDirNPrintableTests ∷ TestTree
absDirNPrintableTests =
  testGroup "printable"
            [ testCase "etcN"  $ "/etc/"       ≟ toText etcN
            , testCase "pamdN" $ "/etc/pam.d/" ≟ toText pamdN
            , testCase "wgmN"  $ "/w/g/M/"     ≟ toText wgmN
            ]

absDirNTextualTests ∷ TestTree
absDirNTextualTests =
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

absDirNTextualPrintableTests ∷ TestTree
absDirNTextualPrintableTests =
  testGroup "textual invertibility"
            [ testProperty "parseString - toString"
                           (propInvertibleString @NonRootAbsDir)
            , testProperty "parseText - toText"
                           (propInvertibleText @NonRootAbsDir)
            , testProperty "parseUtf8 - toUtf8"
                           (propInvertibleUtf8 @NonRootAbsDir)
            ]

absDirNIsNonEmptyTests ∷ TestTree
absDirNIsNonEmptyTests =
  testGroup "IsNonEmpty"
    [ testGroup "fromNonEmpty"
                [ testCase "etc"   $
                    etcN  ≟ fromNonEmpty (pure [pc|etc|])
                , testCase "pam.d" $
                    pamdN ≟ fromNonEmpty ([pc|etc|] :| [ [pc|pam.d|] ])
                , testCase "wgm"   $
                    wgmN  ≟ fromNonEmpty ([pc|w|] :| [ [pc|g|], [pc|M|] ])
                ]
    , testGroup "toNonEmpty"
                [ testCase "etc"   $
                    pure [pc|etc|]                  ≟ toNonEmpty etcN
                , testCase "pam.d" $
                    [pc|etc|] :| [ [pc|pam.d|] ]    ≟ toNonEmpty pamdN
                , testCase "wgm"   $
                    [pc|w|] :| [ [pc|g|], [pc|M|] ] ≟ toNonEmpty wgmN
                ]
    , testGroup "nonEmpty"
                [ testCase "wgm"   $
                    [pc|w|] :| [ [pc|g|], [pc|M|] ] ≟ wgmN ⊣ nonEmpty
                , testCase "wgm"   $
                    wgmN ≟ etcN ⅋ nonEmpty ⊢ [pc|w|] :| [[pc|g|],[pc|M|]]
                ]
    ]


absDirNParentMayTests ∷ TestTree
absDirNParentMayTests =
  let -- set parent of d to d'
      d ~~ d' = d & parentMay ⊩ d'
   in testGroup "parentMay"
                [ testCase "etc → root"  $ Just root ≟ etcN  ⊣ parentMay
                , testCase "pamd → etc"  $ Just etc  ≟ pamdN ⊣ parentMay

                , testCase "etc → root"  $ etcN ≟ etcN ~~ root

                , testCase "pamd → root" $ [absdirN|/pam.d/|] ≟ pamdN ~~ root

                , testCase "etc → wgm"   $ [absdirN|/w/g/M/etc/|] ≟ etcN ~~ wgm
                , testCase "wgm → etc"   $ [absdirN|/etc/M/|] ≟ wgmN ~~ etc

                , testCase "wgm → root"  $ [absdirN|/M/|] ≟ wgmN ~~ root

                , testCase "pamd → etc"  $ pamdN ≟ pamdN ~~ etc
                , testCase "etc → pamd"  $
                      [absdirN|/etc/pam.d/etc/|] ≟ etcN ~~ pamd

                , testCase "pamd → Nothing" $
                      [absdirN|/pam.d/|]  ≟ pamdN ⅋ parentMay ⊢ Nothing
                ]

absDirNParentTests ∷ TestTree
absDirNParentTests =
  let d ~~ d' = d & parent ⊢ d'
   in testGroup "parent"
                [ testCase "etc"         $ root ≟ etcN ⊣ parent
                , testCase "pamd"        $ etc  ≟ pamdN ⊣ parent
                , testCase "etc → root"  $ etcN ≟ etcN ~~ root
                , testCase "pamd → root" $ [absdirN|/pam.d/|] ≟ pamdN ~~ root
                , testCase "etc → wgm"   $ [absdirN|/w/g/M/etc/|] ≟ etcN ~~ wgm
                , testCase "wgm → etc"   $ [absdirN|/etc/M/|] ≟ wgmN ~~ etc

                , testCase "wgm → root"  $ [absdirN|/M/|] ≟ wgmN ~~ root
                , testCase "pamd → etc"  $ pamdN ≟ pamdN ~~ etc
                , testCase "etc → pamd"  $
                      [absdirN|/etc/pam.d/etc/|] ≟ etcN ~~ pamd
                ]

----------------------------------------

absDirNFilepathTests ∷ TestTree
absDirNFilepathTests =
  let nothin' = Nothing ∷ Maybe NonRootAbsDir
      fail s  = testCase s $ nothin' ≟ s ⩼ filepath
   in testGroup "filepath"
            [ testCase "etc"   $ "/etc/"       ≟ etcN     ## filepath
            , testCase "pam.d" $ "/etc/pam.d/" ≟ pamdN    ## filepath
            , testCase "wgm"   $ "/w/g/M/"     ≟ wgmN     ## filepath
            , testCase "/etc/" $ Just etcN     ≟ "/etc/" ⩼ filepath
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

----------------------------------------

absDirNConstructionTests ∷ TestTree
absDirNConstructionTests = testGroup "construction" [ absdirNQQTests ]

absDirNTextualGroupTests ∷ TestTree
absDirNTextualGroupTests =
  testGroup "textual group" [ absDirNTextualTests, absDirNPrintableTests
                            , absDirNTextualPrintableTests ]

absDirNParentGroupTests ∷ TestTree
absDirNParentGroupTests =
  testGroup "parent group" [ absDirNParentTests, absDirNParentMayTests ]

tests ∷ TestTree
tests =
  testGroup "NonRootAbsDir" [ absDirNConstructionTests, absDirNShowTests
                            , absDirNIsNonEmptyTests
                            , absDirNMonoFunctorTests
                            , absDirNMonoFoldableTests
                            , absDirNTextualGroupTests
                            , absDirNIsMonoSeqNETests
                            , absDirNParentGroupTests
                            , absDirNFilepathTests
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
