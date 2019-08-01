{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath.NonRootAbsDir
  ( tests )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Either          ( Either( Left, Right ) )
import Data.Function        ( ($), (&), const )
import Data.Functor         ( fmap )
import Data.Maybe           ( Maybe( Nothing ) )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), typeRep )
import Numeric.Natural      ( Natural )
import System.IO            ( IO )
import Text.Show            ( show )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), fromString, parseString, toText )

-- mono-traversable --------------------

import Data.MonoTraversable  ( omap )
import Data.Sequences        ( reverse )

-- more-unicode ------------------------

import Data.MoreUnicode.Function        ( (⅋) )
import Data.MoreUnicode.Lens            ( (⊣), (⊢), (⊧) )
import Data.MoreUnicode.MonoTraversable ( (⪦), (⪧) )
import Data.MoreUnicode.Semigroup       ( (◇) )
import Data.MoreUnicode.Tasty           ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import NonEmptyContainers.IsNonEmpty  ( fromNonEmpty, nonEmpty, toNonEmpty )
import NonEmptyContainers.SeqNE       ( SeqNE, (⋖) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath                   ( AbsDir, NonRootAbsDir
                               , absdirN, parseAbsDirN', seqNE )
import FPath.PathComponent     ( PathComponent, pc, toUpper )
import FPath.Error.FPathError  ( FPathError( FPathComponentE, FPathEmptyE
                                           , FPathNonAbsE , FPathNotADirE )
                               , __FPathComponentE__, __FPathEmptyE__
                               , __FPathAbsE__, __FPathNonAbsE__
                               , __FPathNotADirE__
                               )
import FPath.Error.FPathComponentError
                               ( FPathComponentError( FPathComponentEmptyE
                                                    , FPathComponentIllegalCharE
                                                    )
                               )

import FPath.T.Common          ( doTest, doTestR, doTestS
                               , propInvertibleString, propInvertibleText
                               , propInvertibleUtf8 )
import FPath.T.FPath.TestData  ( etcN, pamdN, wgmN )

--------------------------------------------------------------------------------

absdirNQQTests ∷ TestTree
absdirNQQTests =
  testGroup "reldir"
            [ testCase "/etc"       $ etcN  ≟ [absdirN|/etc/|]
            , testCase "/etc/pam.d" $ pamdN ≟ [absdirN|/etc/pam.d/|]
            , testCase "/w/g/M"     $ wgmN  ≟ [absdirN|/w/g/M/|]
            ]

parseAbsDirNTests ∷ TestTree
parseAbsDirNTests =
  let absdirT     = typeRep (Proxy ∷ Proxy AbsDir)
      pamNUL      = "/etc/pam\0/"
      pamF        = "/etc/pam"
      illegalCE   = let fpcice = FPathComponentIllegalCharE '\0' "pam\0"
                     in FPathComponentE fpcice absdirT pamNUL
      emptyCompCE = FPathComponentE FPathComponentEmptyE absdirT "/etc//pam.d/"
      parseAbsDirN_ ∷ MonadError FPathError η ⇒ Text → η NonRootAbsDir
      parseAbsDirN_ = parseAbsDirN'
   in testGroup "parseAbsDirN"
                [ testCase "etc"   $ Right etcN   ≟ parseAbsDirN_ "/etc/"
                , testCase "pam.d" $ Right pamdN  ≟ parseAbsDirN_ "/etc/pam.d/"
                , testCase "wgm"   $ Right wgmN   ≟ parseAbsDirN_ "/w/g/M/"
                , testCase "no trailing /" $
                      Left (FPathNotADirE absdirT pamF) ≟ parseAbsDirN_ pamF
                , testCase "empty" $
                      Left (FPathEmptyE absdirT)  ≟ parseAbsDirN_ ""
                , testCase "no leading /" $
                      Left (FPathNonAbsE absdirT "etc/") ≟ parseAbsDirN_ "etc/"
                , testCase "bad component" $
                      Left illegalCE ≟ parseAbsDirN_ pamNUL
                , testCase "empty component" $
                      Left emptyCompCE ≟ parseAbsDirN_ "/etc//pam.d/"
                ]

absDirNShowTests ∷ TestTree
absDirNShowTests =
  let etcNShow  = "NonRootAbsDir (PathComponent \"etc\") AbsRootDir"
      pamdNShow = "NonRootAbsDir (PathComponent \"pam.d\") "
                ⊕ "(AbsNonRootDir (NonRootAbsDir (PathComponent \"etc\") "
                ⊕ "AbsRootDir))"

   in testGroup "show"
                [ testCase "etc"   $ etcNShow  ≟ show etcN
                , testCase "pam.d" $ pamdNShow ≟ show pamdN
                ]

absDirNSeqGetTests ∷ TestTree
absDirNSeqGetTests =
  let infix 4 ??
      (??) ∷ SeqNE PathComponent → NonRootAbsDir → Assertion
      e ?? g = e ≟ g ⊣ seqNE
   in testGroup "seqNE" [ testCase "etc"   $ pure [pc|etc|] ?? etcN
                        , testCase "pam.d" $ [pc|etc|] ⋖ [[pc|pam.d|]] ?? pamdN
                        , testCase "wgM" $ [pc|w|] ⋖ [[pc|g|],[pc|M|]] ?? wgmN
                        ]

absDirNMonoFunctorTests ∷ TestTree
absDirNMonoFunctorTests =
  testGroup "MonoFunctor"
            [ testCase "usr" $
                    [absdirN|/usr/|] ≟ omap (const [pc|usr|]) etcN
            , testCase "wgm.d" $ [absdirN|/w.d/g.d/M.d/|] ≟ (◇ [pc|.d|]) ⪧ wgmN
            , testCase "WGM" $ [absdirN|/W/G/M/|] ≟  wgmN ⪦ toUpper
            ]

absDirNSeqSetTests ∷ TestTree
absDirNSeqSetTests =
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

absDirNSeqTests ∷ TestTree
absDirNSeqTests =
  testGroup "absDirNSeqTests" [ absDirNSeqGetTests, absDirNSeqSetTests
                              , absDirNMonoFunctorTests ]

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
                    wgmN ≟ (etcN & nonEmpty ⊢ [pc|w|] :| [[pc|g|],[pc|M|]])
                ]
    ]

absDirNConstructionTests ∷ TestTree
absDirNConstructionTests = testGroup "construction" [ parseAbsDirNTests
                                                          , absdirNQQTests ]

absDirNTextualGroupTests ∷ TestTree
absDirNTextualGroupTests =
  testGroup "textual group" [ absDirNTextualTests, absDirNTextualPrintableTests
                            , absDirNPrintableTests ]

tests ∷ TestTree
tests =
  testGroup "NonRootAbsDir" [ absDirNConstructionTests
                            , absDirNShowTests
                            , absDirNTextualGroupTests
                            , absDirNIsNonEmptyTests

                            , absDirNSeqTests
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
