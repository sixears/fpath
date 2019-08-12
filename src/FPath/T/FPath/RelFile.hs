{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module FPath.T.FPath.RelFile
  ( tests )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Bool            ( Bool( False, True ) )
import Data.Either          ( Either( Left, Right  ) )
import Data.Function        ( ($), (&), const )
import Data.Functor         ( fmap )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Ord             ( Ordering( GT ), (<), compare, comparing )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), typeRep )
import GHC.Exts             ( fromList )
import Numeric.Natural      ( Natural )
import System.IO            ( IO )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

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

import Data.MonoTraversable  ( maximumByEx, minimumByEx, oall, oany
                             , ocompareLength, oelem, ofoldMap, ofoldl', ofoldlM
                             , ofoldl1Ex', ofoldMap1Ex, ofoldr, ofoldr1Ex
                             , olength, olength64, omap, onotElem, onull
                             , otoList, otraverse_, unsafeHead, unsafeLast
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens             ( (⊣), (⊥), (⊢), (⊧), (⩼), (##) )
import Data.MoreUnicode.Monoid           ( ф )
import Data.MoreUnicode.MonoTraversable  ( (⪦), (⪧) )
import Data.MoreUnicode.Semigroup        ( (◇) )
import Data.MoreUnicode.Tasty            ( (≟), (≣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import NonEmptyContainers.IsNonEmpty      ( fromNonEmpty, toNonEmpty )
import NonEmptyContainers.SeqConversions  ( IsMonoSeq( seq ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertFailure, testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text --------------------------------

import qualified  Data.Text  as  Text
import Data.Text  ( Text, length )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath        ( filepath )
import FPath.Error.FPathError  ( FPathError( FPathAbsE, FPathComponentE
                                           , FPathEmptyE, FPathNotAFileE )
                               )
import FPath.Error.FPathComponentError
                               ( FPathComponentError( FPathComponentEmptyE
                                                    , FPathComponentIllegalCharE
                                                    , FPathComponentIllegalE
                                                    )
                               )
import FPath.HasParent         ( parentMay )
import FPath.PathComponent     ( pc, toUpper )
import FPath.RelFile           ( RelDir, RelFile, parseRelFile', relfile )

import FPath.T.Common          ( doTest, doTestR, doTestS, propInvertibleString
                               , propInvertibleText, propInvertibleUtf8 )
import FPath.T.FPath.TestData  ( rf1, rf2, rf3, rf4, r0, r1, r2, r3 )

--------------------------------------------------------------------------------

parseRelFileTests ∷ TestTree
parseRelFileTests =
  let relfileT    = typeRep (Proxy ∷ Proxy RelFile)
      illegalCE s t = let fpcice = FPathComponentIllegalCharE '\0' t
                       in FPathComponentE fpcice relfileT s
      emptyCompCE t = FPathComponentE FPathComponentEmptyE relfileT t

      illegalPC t = let s = toString t
                        fpipce s = FPathComponentIllegalE s
                        fpipce' s t = FPathComponentE (fpipce s) relfileT t
                     in testCase ("illegal path component '" ⊕ s ⊕ "'") $
                          Left (fpipce' s t) ≟ parseRelFile_ t
      badChar s p = testCase ("bad component " ⊕ toString s) $
                        Left (illegalCE s p) ≟ parseRelFile_ s
      absfile t  = testCase ("absolute file '" ⊕ toString t ⊕ "'") $
                      Left (FPathAbsE relfileT t) ≟ parseRelFile_ t

      notAFile t = testCase ("not a file: '" ⊕ toString t ⊕ "'") $
                      Left (FPathNotAFileE relfileT t) ≟ parseRelFile_ t
      notAFile t = testCase ("not a file: '" ⊕ toString t ⊕ "'") $
                      Left (FPathNotAFileE relfileT t) ≟ parseRelFile_ t
      parseRelFile_ ∷ MonadError FPathError η ⇒ Text → η RelFile
      parseRelFile_ = parseRelFile'
   in testGroup "parseRelFile"
                [ testCase "rf1" $ Right rf1 ≟ parseRelFile_ "r.e"
                , testCase "rf1" $ Right rf1 ≟ parseRelFile_ "./r.e"
                , testCase "rf4" $ Right rf4 ≟ parseRelFile_ ".x"
                , testCase "rf2" $ Right rf2 ≟ parseRelFile_ "r/p.x"
                , testCase "rf3" $ Right rf3 ≟ parseRelFile_ "p/q/r.mp3"
                , absfile "/p.e"
                , absfile "/r/p.e"
                , testCase "empty" $
                      Left (FPathEmptyE relfileT) ≟ parseRelFile_ ""
                , notAFile "./p/"
                , notAFile "/r/"
                , notAFile "./"
                , illegalPC "."
                , illegalPC ".."
                , badChar "x/\0/y" "\0"
                , badChar "r/p\0" "p\0"
                , badChar "\0r/p" "\0r"
                , testCase "empty component" $
                      Left (emptyCompCE "r//p") ≟ parseRelFile_ "r//p"
                ]

relfileQQTests ∷ TestTree
relfileQQTests =
  testGroup "relfile"
            [ testCase "rf1" $ rf1 ≟ [relfile|r.e|]
            , testCase "rf2" $ rf2 ≟ [relfile|r/p.x|]
            , testCase "rf3" $ rf3 ≟ [relfile|p/q/r.mp3|]
            ]

relFileIsMonoSeqGetterTests ∷ TestTree
relFileIsMonoSeqGetterTests =
  testGroup "getter"
            [ testCase "r0" $ ф                                    ≟ r0 ⊣ seq
            , testCase "rf1" $ ([pc|r|] ⪬ ф)                        ≟ rf1 ⊣ seq
            , testCase "rf2" $ Seq.fromList [[pc|r|], [pc|p|]]          ≟ rf2 ⊣ seq
            , testCase "rf3" $ Seq.fromList [[pc|p|], [pc|q|], [pc|r|]] ≟ rf3 ⊣ seq
            ]

relFileIsNonEmptyTests ∷ TestTree
relFileIsNonEmptyTests =
  testGroup "IsNonEmpty"
    [ testGroup "fromNonEmpty"
                [ testCase "rf1" $ rf1 ≟ fromNonEmpty (pure [pc|r.e|])
                , testCase "rf2" $ rf2 ≟ fromNonEmpty ([pc|r|] :| [ [pc|p.x|] ])
                , testCase "rf3" $ rf3 ≟
                    fromNonEmpty ([pc|p|] :| [ [pc|q|], [pc|r.mp3|] ])
                , testCase "rf4" $ rf4 ≟ fromNonEmpty (pure [pc|.x|])
                ]
    , testGroup "toNonEmpty"
                [ testCase "rf1" $ pure [pc|r.e|]           ≟ toNonEmpty rf1
                , testCase "rf2" $ [pc|r|] :| [ [pc|p.x|] ] ≟ toNonEmpty rf2
                , testCase "rf3" $
                    [pc|p|] :| [ [pc|q|], [pc|r.mp3|] ]     ≟ toNonEmpty rf3
                , testCase "rf4" $ pure [pc|.x|]            ≟ toNonEmpty rf4
                ]
    ]

relFileIsMonoSeqSetterTests ∷ TestTree
relFileIsMonoSeqSetterTests =
  let x ~!~ y = x & seq ⊢ fromList y
   in testGroup "setter"
                [ -- testCase "rf1"  $ rf1 ≟ r0 ~!~ [[pc|r|]]
--                , testCase "r0"  $ r0 ≟ rf1 ~!~ ф
--                , testCase "rf2"  $ rf2 ≟ r0 ~!~ [[pc|r|],[pc|p|]]
                 testCase "rf3"  $ rf3 ≟ rf2 ~!~ [[pc|p|],[pc|q|],[pc|r|]]
                , testCase "rf3'" $
                     [relfile|p/t/r/|] ≟ (rf3 & seq ⊥ 1 ⊢ [pc|t|])
                ]

relFileShowTests ∷ TestTree
relFileShowTests =
  let rf1Show = "RelFile (RelDir (fromList [])) (PathComponent \"r.e\")"
      rf2Show = "RelFile (RelDir (fromList [PathComponent \"r\"])) "
              ⊕ "(PathComponent \"p.x\")"
      rf3Show = let pq = "[PathComponent \"p\",PathComponent \"q\"]"
                    r  = "PathComponent \"r.mp3\""
                 in "RelFile (RelDir (fromList "⊕ pq ⊕")) ("⊕ r ⊕")"
      rf4Show = "RelFile (RelDir (fromList [])) (PathComponent \".x\")"

   in testGroup "show"
                [ testCase "rf1" $ rf1Show ≟ show rf1
                , testCase "rf2" $ rf2Show ≟ show rf2
                , testCase "rf3" $ rf3Show ≟ show rf3
                , testCase "rf4" $ rf4Show ≟ show rf4
                ]

relFilePrintableTests ∷ TestTree
relFilePrintableTests =
  testGroup "printable"
            [ testCase "r0" $ "./"     ≟ toText r0
            , testCase "rf1" $ "r/"     ≟ toText rf1
            , testCase "rf2" $ "r/p/"   ≟ toText rf2
            , testCase "rf3" $ "p/q/r/" ≟ toText rf3
            ]

relFileTextualTests ∷ TestTree
relFileTextualTests =
  let nothin'     ∷ Maybe RelFile
      nothin'     = Nothing
      success e s = testCase s $ Parsed e  ≟ parseString s
      fail s      = testCase s $ nothin'   ≟ fromString s
   in testGroup "Textual" [ success rf1 "r.e"
                          , success rf2 "r/p/"
                          , success rf3 "p/q/r/"
                          , success rf4 "./"
                          , fail "/etc"
                          , fail "/etc/pam.d"
                          , success [relfile|etc|] "etc"
                          , success [relfile|etc/pam.d|] "etc/pam.d"
                          , fail "/etc//pam.d/"
                          , success [relfile|e/c|] "e/c"
                          , fail "e/c/"
                          , fail "\0etc"
                          , fail "etc\0"
                          , fail "e\0c"
                          ]

relFileTextualPrintableTests ∷ TestTree
relFileTextualPrintableTests =
  testGroup "invertibility"
            [ testProperty "parseString - toString"
                           (propInvertibleString @RelFile)
            , testProperty "parseText - toText" (propInvertibleText @RelFile)
            , testProperty "parseUtf8 - toUtf8" (propInvertibleUtf8 @RelFile)
            ]

relFileMonoFunctorTests ∷ TestTree
relFileMonoFunctorTests =
  testGroup "MonoFunctor"
            [ testCase "rf1 → usr" $
                    [relfile|usr/|] ≟ omap (const [pc|usr|]) rf1
            , testCase "rf3.d" $ [relfile|p.d/q.d/r.d/|] ≟ (◇ [pc|.d|]) ⪧ rf3
            , testCase "rf3 toUpper" $ [relfile|P/Q/R/|] ≟  rf3 ⪦ toUpper
            ]

relFileIsMonoSeqTests ∷ TestTree
relFileIsMonoSeqTests = testGroup "IsMonoSeq" [ relFileIsMonoSeqGetterTests
                                             , relFileIsMonoSeqSetterTests ]

relFileParentMayGetterTests ∷ TestTree
relFileParentMayGetterTests =
   testGroup "getter" [ testCase "r0"  $ Nothing ≟ r0  ⊣ parentMay
                      , testCase "rf1" $ Just r0 ≟ rf1  ⊣ parentMay
                      , testCase "rf2" $ Just r1 ≟ rf2 ⊣ parentMay
                      ]

relFileParentMaySetterTests ∷ TestTree
relFileParentMaySetterTests =
  let (~~) ∷ RelFile → RelDir → RelFile
      d ~~ d' = d & parentMay ?~ d'
   in testGroup "setter" [ -- testCase "rf1 → r0" $ rf1 ≟ rf1 ~~ r0
--                         , testCase "r0 → rf1" $ rf1 ≟ r0 ~~ r1

--                         , testCase "rf2 → r0" $ [relfile|p/|] ≟ rf2 ~~ r0
--                         , testCase "r0 → rf2" $ rf2 ≟ r0 ~~ r2

                          testCase "rf1 → rf3" $ [relfile|p/q/r/r/|] ≟ rf1 ~~ r3
                         , testCase "rf3 → rf1" $ [relfile|r/r/|] ≟ rf3 ~~ r1

--                         , testCase "r0 → rf3" $ rf3 ≟ r0 ~~ r3
--                         , testCase "rf3 → r0" $ rf1 ≟ rf3 ~~ r0

                         , testCase "rf2 → rf1" $ rf2 ≟ rf2 ~~ r1
                         , testCase "rf1 → rf2" $ [relfile|r/p/r/|] ≟ rf1 ~~ r2

--                         , testCase "rf2 → r0 (Nothing)" $
--                              [relfile|p/|] ≟ (rf2 & parentMay ⊢ Nothing)
                         ]

relFileParentMayAdjusterTests ∷ TestTree
relFileParentMayAdjusterTests =
  -- reverse the directories in the parent seq
  testGroup "adjuster" [ testCase "rf3 reverse" $
                           let reverseP = fmap (& seq ⊧ Seq.reverse)
                            in [relfile|q/p/r/|] ≟ (rf3 & parentMay ⊧ reverseP)

                       ]

relFileParentMayTests ∷ TestTree
relFileParentMayTests = testGroup "parentMay"
                                 [ relFileParentMayGetterTests
                                 , relFileParentMaySetterTests
                                 , relFileParentMayAdjusterTests
                                 ]

relFileFilepathTests ∷ TestTree
relFileFilepathTests =
  let nothin' = Nothing ∷ Maybe RelFile
      fail s  = testCase s $ nothin' ≟ s ⩼ filepath
   in testGroup "filepath"
            [ testCase "r0" $ "./"     ≟ r0 ## filepath
            , testCase "rf1" $ "r/"     ≟ rf1 ## filepath
            , testCase "rf2" $ "r/p/"   ≟ rf2 ## filepath
            , testCase "rf3" $ "p/q/r/" ≟ rf3 ## filepath

            , testCase "rf1" $ Just rf1 ≟ "r/"     ⩼ filepath
            , testCase "rf2" $ Just rf2 ≟ "r/p/"   ⩼ filepath
            , testCase "rf3" $ Just rf3 ≟ "p/q/r/" ⩼ filepath

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

relFileConstructionTests ∷ TestTree
relFileConstructionTests = testGroup "construction" [ parseRelFileTests
                                                   , relfileQQTests ]

relFileMonoFoldableTests ∷ TestTree
relFileMonoFoldableTests =
  testGroup "MonoFoldable"
            [ testCase "ofoldMap" $
                "p-q-r.mp3-" ≟ ofoldMap ((⊕ "-") ∘ toText) rf3
            , testCase "ofoldr" $
                "p-q-r.mp3-ф" ≟ ofoldr (\ a b → toText a ⊕ "-" ⊕ b) "ф" rf3
            , testCase "ofoldl'" $
                "ф-p-q-r.mp3" ≟ ofoldl' (\ b a → b ⊕ "-" ⊕ toText a) "ф" rf3
            , testCase "otoList" $
                [ [pc|p|], [pc|q|], [pc|r.mp3|] ] ≟ otoList rf3
            , testCase "oall (F)" $
                False ≟ oall (Text.any (≡ 'r' ) ∘ toText) rf3
            , testCase "oall (T)" $
                True ≟ oall ((< 6) ∘ Text.length ∘ toText) rf3
            , testCase "oany (F)" $
                False ≟ oany (Text.any (≡ 'x' ) ∘ toText) rf3
            , testProperty "onull" (\ (x ∷ RelFile) → False ≣ onull x)
            , testCase "olength" $
                3 ≟ olength rf3
            , testCase "olength64" $
                1 ≟ olength64 rf4
            , testCase "ocompareLength" $
               GT ≟ ocompareLength rf3 2
            , testCase "ofoldlM" $
                  Just [[pc|r.mp3|],[pc|q|],[pc|p|]]
                ≟ ofoldlM (\ a e → Just $ e : a) [] rf3
            , testCase "ofoldMap1Ex" $
                [[pc|p|],[pc|q|],[pc|r.mp3|]] ≟ ofoldMap1Ex pure rf3
            , testCase "ofoldr1Ex" $
                [pc|pqr.mp3|] ≟ ofoldr1Ex (◇) rf3
            , testCase "ofoldl1Ex'" $
                [pc|pqr.mp3|] ≟ ofoldl1Ex' (◇) rf3
            , testCase "unsafeHead" $
                [pc|p|] ≟ unsafeHead rf3
            , testCase "unsafeLast" $
                [pc|r.mp3|] ≟ unsafeLast rf3
            , testCase "maximumByEx" $
                [pc|r.mp3|] ≟ maximumByEx (comparing toText) rf3
            , testCase "minimumByEx" $
                [pc|p|] ≟ minimumByEx (comparing toText) rf3
            , testCase "oelem (T)" $
                True ≟ oelem [pc|q|] rf3
            , testCase "oelem (F)" $
                False ≟ oelem [pc|x|] rf3
            , testCase "onotElem (T)" $
                True ≟ onotElem [pc|x|] rf3
            , testCase "onotElem (F)" $
                False ≟ onotElem [pc|q|] rf3
            ]


relFileTextualGroupTests ∷ TestTree
relFileTextualGroupTests =
  testGroup "textual group" [ relFileTextualTests, relFileTextualPrintableTests
                            , relFilePrintableTests ]

tests ∷ TestTree
tests =
  testGroup "RelFile" [ relFileConstructionTests, relFileShowTests
                      , relFileIsNonEmptyTests
                      , relFileMonoFoldableTests
                      , relFileTextualGroupTests
                      , relFileIsMonoSeqTests
                      , relFileParentMayTests
                      , relFileFilepathTests

                      , relFileMonoFunctorTests
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
