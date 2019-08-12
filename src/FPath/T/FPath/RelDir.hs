{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module FPath.T.FPath.RelDir
  ( tests )
where

-- base --------------------------------

import Data.Either      ( Either( Left, Right  ) )
import Data.Foldable    ( foldr )
import Data.Function    ( ($), (&), const )
import Data.Functor     ( fmap )
import Data.Maybe       ( Maybe( Just, Nothing ) )
import Data.String      ( String )
import Data.Typeable    ( Proxy( Proxy ), typeRep )
import GHC.Exts         ( fromList, toList )
import Numeric.Natural  ( Natural )
import System.IO        ( IO )
import Text.Show        ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed )
                     , fromString, parseString, toString, toText )

-- fluffy ------------------------------

import NonEmptyContainers.SeqNE  ( (⪬) )

-- lens --------------------------------

import Control.Lens.Setter  ( (?~) )

-- mono-traversable --------------------

import Data.MonoTraversable  ( omap )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens             ( (⊣), (⊥), (⊢), (⊧), (⩼), (##) )
import Data.MoreUnicode.Monoid           ( ю, ф )
import Data.MoreUnicode.MonoTraversable  ( (⪦), (⪧) )
import Data.MoreUnicode.Semigroup        ( (◇) )
import Data.MoreUnicode.Tasty            ( (≟), (≣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqConversions ( IsMonoSeq( seq ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath        ( filepath )
import FPath.Error.FPathError  ( FPathError( FPathAbsE, FPathComponentE
                                           , FPathNotADirE )
                               )
import FPath.Error.FPathComponentError
                               ( FPathComponentError( FPathComponentEmptyE
                                                    , FPathComponentIllegalCharE
                                                    )
                               )
import FPath.HasParent         ( parentMay )
import FPath.PathComponent     ( pc, toUpper )
import FPath.RelDir            ( RelDir, parseRelDir', reldir )

import FPath.T.Common          ( doTest, doTestR, doTestS
                               , propAssociative, propInvertibleString
                               , propInvertibleText, propInvertibleUtf8
                               )
import FPath.T.FPath.TestData  ( r0, r1, r2, r3 )

--------------------------------------------------------------------------------

parseRelDirTests ∷ TestTree
parseRelDirTests =
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

reldirQQTests ∷ TestTree
reldirQQTests =
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

relDirSemigroupTests ∷ TestTree
relDirSemigroupTests =
  testGroup "Semigroup"
            [ testProperty "associativity" (propAssociative @RelDir (◇))
            ]

relDirMonoidTests ∷ TestTree
relDirMonoidTests =
  testGroup "Monoid"
            [ testProperty "left identity"  (\ (x ∷ RelDir) → ф ⊕ x ≣ x)
            , testProperty "right identity" (\ (x ∷ RelDir) → x ⊕ ф ≣ x)
            , testProperty "associativity"  (propAssociative @RelDir (⊕))
            , testProperty "mconcat"  (\ (x ∷ [RelDir]) → ю x ≣ foldr (⊕) ф x)
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

relDirTextualTests ∷ TestTree
relDirTextualTests =
  let nothin'     ∷ Maybe RelDir
      nothin'     = Nothing
      success e s = testCase s $ Parsed e  ≟ parseString s
      fail s      = testCase s $ nothin'   ≟ fromString s
   in testGroup "Textual" [ success r0 "./"
                          , success r1 "r/"
                          , success r2 "r/p/"
                          , success r3 "p/q/r/"
                          , fail "/etc"
                          , fail "/etc/pam.d"
                          , fail "etc"
                          , fail "etc/pam.d"
                          , fail "/etc//pam.d/"
                          , fail "e/c"
                          , fail "\0etc"
                          , fail "etc\0"
                          , fail "e\0c"
                          ]

relDirTextualPrintableTests ∷ TestTree
relDirTextualPrintableTests =
  testGroup "invertibility"
            [ testProperty "parseString - toString"
                           (propInvertibleString @RelDir)
            , testProperty "parseText - toText" (propInvertibleText @RelDir)
            , testProperty "parseUtf8 - toUtf8" (propInvertibleUtf8 @RelDir)
            ]

relDirMonoFunctorTests ∷ TestTree
relDirMonoFunctorTests =
  testGroup "MonoFunctor"
            [ testCase "r0 → r0" $
                    [reldir|./|] ≟ omap (const [pc|usr|]) r0
            , testCase "r1 → usr" $
                    [reldir|usr/|] ≟ omap (const [pc|usr|]) r1
            , testCase "r3.d" $ [reldir|p.d/q.d/r.d/|] ≟ (◇ [pc|.d|]) ⪧ r3
            , testCase "r3 toUpper" $ [reldir|P/Q/R/|] ≟  r3 ⪦ toUpper
            ]

relDirIsMonoSeqTests ∷ TestTree
relDirIsMonoSeqTests = testGroup "IsMonoSeq" [ relDirIsMonoSeqGetterTests
                                             , relDirIsMonoSeqSetterTests ]

relDirParentMayGetterTests ∷ TestTree
relDirParentMayGetterTests =
   testGroup "getter" [ testCase "r0"  $ Nothing ≟ r0  ⊣ parentMay
                      , testCase "r1"  $ Just r0 ≟ r1  ⊣ parentMay
                      , testCase "r2"  $ Just r1 ≟ r2 ⊣ parentMay
                      ]

relDirParentMaySetterTests ∷ TestTree
relDirParentMaySetterTests =
  let d ~~ d' = d & parentMay ?~ d'
   in testGroup "setter" [ testCase "r1 → r0" $ r1 ≟ r1 ~~ r0
                         , testCase "r0 → r1" $ r1 ≟ r0 ~~ r1

                         , testCase "r2 → r0" $ [reldir|p/|] ≟ r2 ~~ r0
                         , testCase "r0 → r2" $ r2 ≟ r0 ~~ r2

                         , testCase "r1 → r3" $ [reldir|p/q/r/r/|] ≟ r1 ~~ r3
                         , testCase "r3 → r1" $ [reldir|r/r/|] ≟ r3 ~~ r1

                         , testCase "r0 → r3" $ r3 ≟ r0 ~~ r3
                         , testCase "r3 → r0" $ r1 ≟ r3 ~~ r0

                         , testCase "r2 → r1" $ r2 ≟ r2 ~~ r1
                         , testCase "r1 → r2" $ [reldir|r/p/r/|] ≟ r1 ~~ r2

                         , testCase "r2 → r0 (Nothing)" $
                              [reldir|p/|] ≟ (r2 & parentMay ⊢ Nothing)
                         ]

relDirParentMayAdjusterTests ∷ TestTree
relDirParentMayAdjusterTests =
  -- reverse the directories in the parent seq
  testGroup "adjuster" [ testCase "r3 reverse" $
                           let reverseP = fmap (& seq ⊧ Seq.reverse)
                            in [reldir|q/p/r/|] ≟ (r3 & parentMay ⊧ reverseP)

                       ]

relDirParentMayTests ∷ TestTree
relDirParentMayTests = testGroup "parentMay"
                                 [ relDirParentMayGetterTests
                                 , relDirParentMaySetterTests
                                 , relDirParentMayAdjusterTests
                                 ]

relDirFilepathTests ∷ TestTree
relDirFilepathTests =
  let nothin' = Nothing ∷ Maybe RelDir
      fail s  = testCase s $ nothin' ≟ s ⩼ filepath
   in testGroup "filepath"
            [ testCase "r0" $ "./"     ≟ r0 ## filepath
            , testCase "r1" $ "r/"     ≟ r1 ## filepath
            , testCase "r2" $ "r/p/"   ≟ r2 ## filepath
            , testCase "r3" $ "p/q/r/" ≟ r3 ## filepath

            , testCase "r0" $ Just r0 ≟ "./"     ⩼ filepath
            , testCase "r1" $ Just r1 ≟ "r/"     ⩼ filepath
            , testCase "r2" $ Just r2 ≟ "r/p/"   ⩼ filepath
            , testCase "r3" $ Just r3 ≟ "p/q/r/" ⩼ filepath

            , fail "/etc"
            , fail "/etc"
            , fail "etc"
            , fail "/etc/pam.d"
            , fail "etc/pam.d"
            , fail "/etc//pam.d/"
            , fail "e/c"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"
            ]

relDirConstructionTests ∷ TestTree
relDirConstructionTests = testGroup "construction" [ parseRelDirTests
                                                   , reldirQQTests ]

relDirTextualGroupTests ∷ TestTree
relDirTextualGroupTests =
  testGroup "textual group" [ relDirTextualTests, relDirTextualPrintableTests
                            , relDirPrintableTests ]

tests ∷ TestTree
tests =
  testGroup "RelDir" [ relDirConstructionTests, relDirShowTests
                     , relDirSemigroupTests
                     , relDirMonoidTests
                     , relDirIsListTests
                     , relDirTextualGroupTests
                     , relDirIsMonoSeqTests
                     , relDirParentMayTests
                     , relDirFilepathTests

                     , relDirMonoFunctorTests
                     ]

----------------------------------------

_test ∷ IO ()
_test = doTest tests

--------------------

_tests ∷ String → IO ()
_tests = doTestS tests

_testr ∷ String → Natural → IO ()
_testr = doTestR tests

-- that's all, folks! ----------------------------------------------------------
