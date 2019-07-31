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
import Data.Either          ( Either( Left, Right  ) )
import Data.Function        ( ($), (&), const )
import Data.Functor         ( fmap )
import Data.Maybe           ( Maybe( Nothing ) )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), typeRep )
import GHC.Exts             ( fromList, toList )
import Numeric.Natural      ( Natural )
import System.IO            ( IO )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≢) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), fromString, parseString
                     , toString, toText )

-- fluffy ------------------------------

import NonEmptyContainers.SeqNE  ( SeqNE, (⪬), (⋖) )

-- genvalidity -------------------------

import Data.GenValidity  ( genValid )

-- genvalidity-property ----------------

import Test.Validity.GenValidity.Property  ( genGeneratesValid )

-- mono-traversable --------------------

import Data.MonoTraversable  ( omap )
import Data.Sequences        ( reverse )

-- more-unicode ------------------------

import Data.MoreUnicode.Function        ( (⅋) )
import Data.MoreUnicode.Lens            ( (⊣), (⊥), (⊢), (⊧) )
import Data.MoreUnicode.Monoid          ( ф )
import Data.MoreUnicode.MonoTraversable ( (⪦), (⪧) )
import Data.MoreUnicode.Semigroup       ( (◇) )
import Data.MoreUnicode.Tasty           ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

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

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  FPath.T.FPath.AbsDir

import FPath                   ( AbsDir, NonRootAbsDir, RelDir, absdirN
                               , parseRelDir', seq, seqNE, reldir )
import FPath.Error.FPathError  ( FPathError( FPathAbsE, FPathComponentE
                                           , FPathNotADirE )
                               )
import FPath.Error.FPathComponentError
                               ( FPathComponentError( FPathComponentEmptyE
                                                    , FPathComponentIllegalCharE
                                                    )
                               )
import FPath.PathComponent     ( PathComponent, pc, toUpper )

import FPath.T.Common          ( doTest, doTestR, doTestS
                               , propInvertibleString, propInvertibleText
                               , propInvertibleUtf8 )
import FPath.T.FPath.TestData  ( etcN, pamdN, wgmN, r0, r1, r2, r3 )

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

------------------------------------------------------------

relDirParseRelDirTests ∷ TestTree
relDirParseRelDirTests =
  let reldirT     = typeRep (Proxy ∷ Proxy RelDir)
      pamF        = "etc/pam"
      illegalCE s t = let fpcice = FPathComponentIllegalCharE '\0' t
                       in FPathComponentE fpcice reldirT s
      badChar s p = testCase ("bad component " ⊕ toString s) $
                        Left (illegalCE s p) ≟ parseRelDir_ s
      emptyCompCE t = FPathComponentE FPathComponentEmptyE reldirT t
      parseRelDir_ ∷ MonadError FPathError η ⇒ Text → η RelDir
      parseRelDir_ = parseRelDir'
   in testGroup "parseRelDir"
                [ testCase "r0" $ Right r0 ≟ parseRelDir_ "./"
                , testCase "r1" $ Right r1 ≟ parseRelDir_ "r/"
                , testCase "r2" $ Right r2 ≟ parseRelDir_ "r/p/"
                , testCase "r3" $ Right r3 ≟ parseRelDir_ "p/q/r/"
                , testCase "no trailing /" $
                      Left (FPathNotADirE reldirT pamF) ≟ parseRelDir_ pamF
                , testCase "leading /" $
                      Left (FPathAbsE reldirT "/r/") ≟ parseRelDir_ "/r/"
                , badChar "x/\0/y/" "\0"
                , badChar "r/p\0/" "p\0"
                , badChar "\0r/p/" "\0r"
                , testCase "empty component" $
                      Left (emptyCompCE "r//p/") ≟ parseRelDir_ "r//p/"
                ]

relDirQuasiQuotesTests ∷ TestTree
relDirQuasiQuotesTests =
  testGroup "reldir"
            [ testCase "r0" $ r0 ≟ [reldir|./|]
            , testCase "r1" $ r1 ≟ [reldir|r/|]
            , testCase "r2" $ r2 ≟ [reldir|r/p/|]
            , testCase "r3" $ r3 ≟ [reldir|p/q/r/|]
            ]

relDirIsMonoSeqGetterTests ∷ TestTree
relDirIsMonoSeqGetterTests =
  testGroup "getter"
            [ testCase "r0" $ ф                                    ≟ r0 ⊣ seq
            , testCase "r1" $ ([pc|r|] ⪬ ф)                        ≟ r1 ⊣ seq
            , testCase "r2" $ Seq.fromList [[pc|r|], [pc|p|]]          ≟ r2 ⊣ seq
            , testCase "r3" $ Seq.fromList [[pc|p|], [pc|q|], [pc|r|]] ≟ r3 ⊣ seq
            ]

relDirIsListTests ∷ TestTree
relDirIsListTests =
  testGroup "IsList"
    [ testGroup "fromList"
                [ testCase "r0" $ r0 ≟ fromList []
                , testCase "r1" $ r1 ≟ fromList [ [pc|r|] ]
                , testCase "r2" $ r2 ≟ fromList [ [pc|r|], [pc|p|] ]
                , testCase "r3" $ r3 ≟ fromList [ [pc|p|], [pc|q|], [pc|r|] ]
                ]
    , testGroup "toList"
                [ testCase "r0" $ []                            ≟ toList r0
                , testCase "r1" $ [ [pc|r|] ]                   ≟ toList r1
                , testCase "r2" $ [ [pc|r|], [pc|p|] ]          ≟ toList r2
                , testCase "r3" $ [ [pc|p|], [pc|q|], [pc|r|] ] ≟ toList r3
                ]
    ]

relDirIsMonoSeqSetterTests ∷ TestTree
relDirIsMonoSeqSetterTests =
  let x ~!~ y = x & seq ⊢ fromList y
   in testGroup "setter"
                [ testCase "r1"  $ r1 ≟ r0 ~!~ [[pc|r|]]
                , testCase "r0"  $ r0 ≟ r1 ~!~ ф
                , testCase "r2"  $ r2 ≟ r0 ~!~ [[pc|r|],[pc|p|]]
                , testCase "r3"  $ r3 ≟ r2 ~!~ [[pc|p|],[pc|q|],[pc|r|]]
                , testCase "r3'" $
                     [reldir|p/t/r/|] ≟ (r3 & seq ⊥ 1 ⊢ [pc|t|])
                ]

relDirShowTests ∷ TestTree
relDirShowTests =
  let r0Show = "RelDir (fromList [])"
      r1Show = "RelDir (fromList [PathComponent \"r\"])"
      r2Show = "RelDir (fromList [PathComponent \"r\",PathComponent \"p\"])"
      r3Show = "RelDir (fromList "
             ⊕ "[PathComponent \"p\",PathComponent \"q\",PathComponent \"r\"])"

   in testGroup "show"
                [ testCase "r0" $ r0Show ≟ show r0
                , testCase "r1" $ r1Show ≟ show r1
                , testCase "r2" $ r2Show ≟ show r2
                , testCase "r3" $ r3Show ≟ show r3
                ]

relDirPrintableTests ∷ TestTree
relDirPrintableTests =
  testGroup "printable"
            [ testCase "r0" $ "./"     ≟ toText r0
            , testCase "r1" $ "r/"     ≟ toText r1
            , testCase "r2" $ "r/p/"   ≟ toText r2
            , testCase "r3" $ "p/q/r/" ≟ toText r3
            ]

relDirIsMonoSeqTests ∷ TestTree
relDirIsMonoSeqTests = testGroup "IsMonoSeq" [ relDirIsMonoSeqGetterTests
                                             , relDirIsMonoSeqSetterTests ]
relDirConstructionTests ∷ TestTree
relDirConstructionTests = testGroup "construction" [ relDirParseRelDirTests
                                                   , relDirQuasiQuotesTests ]
relDirTests ∷ TestTree
relDirTests =
  testGroup "RelDir" [ relDirConstructionTests
                     , relDirIsListTests
                     , relDirIsMonoSeqTests
                     , relDirShowTests, relDirPrintableTests
                     ]

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath" [ pathComponentTests
                          , FPath.T.FPath.AbsDir.tests
                          , nonRootAbsDirTests
                          , relDirTests
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
