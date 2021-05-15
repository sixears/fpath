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
import Data.Ord             ( Ordering( GT ), (<), comparing )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), typeRep )
import System.Exit          ( ExitCode )
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

-- lens --------------------------------

import Control.Lens.Setter  ( (?~) )

-- mono-traversable --------------------

import Data.MonoTraversable  ( maximumByEx, minimumByEx, oall, oany
                             , ocompareLength, oelem, ofoldMap, ofoldl', ofoldlM
                             , ofoldl1Ex', ofoldMap1Ex, ofoldr, ofoldr1Ex
                             , olength, olength64, omap, onotElem, onull
                             , otoList, unsafeHead, unsafeLast
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens             ( (⊣), (⊥), (⊢), (⊧), (⩼), (##) )
import Data.MoreUnicode.MonoTraversable  ( (⪦), (⪧) )
import Data.MoreUnicode.Natural          ( ℕ )
import Data.MoreUnicode.Semigroup        ( (◇) )
import Data.MoreUnicode.Tasty            ( (≣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import NonEmptyContainers.IsNonEmpty        ( fromNonEmpty, toNonEmpty )
import NonEmptyContainers.SeqConversions    ( IsMonoSeq( seq ) )
import NonEmptyContainers.SeqNEConversions  ( seqNE )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), propInvertibleString, propInvertibleText
                  , propInvertibleUtf8, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text --------------------------------

import qualified  Data.Text  as  Text
import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  FPath.RelFile

import FPath.AsFilePath        ( filepath )
import FPath.Error.FPathError  ( FPathError( FPathComponentE )
                               , fPathAbsE, fPathEmptyE, fPathNotAFileE
                               )
import FPath.Error.FPathComponentError
                               ( fPathComponentEmptyE
                               , fPathComponentIllegalCharE, fPathIllegalE
                               )
import FPath.FileLike          ( FileLike( (⊙), addExt, dir, ext, file, splitExt
                                         , updateExt ) )
import FPath.Parent            ( parent, parentMay )
import FPath.Parseable         ( parse' )
import FPath.PathComponent     ( PathComponent, pc, toUpper )
import FPath.RelDir            ( RelDir, reldir )
import FPath.RelFile           ( RelFile, relfile )
import FPath.T.FPath.TestData  ( rf1, rf2, rf3, rf4, r0, r1, r2, r3 )

--------------------------------------------------------------------------------

parseRelFileTests ∷ TestTree
parseRelFileTests =
  let relfileT    = typeRep (Proxy ∷ Proxy RelFile)
      illegalCE s t = let fpcice = fPathComponentIllegalCharE '\0' t
                       in FPathComponentE fpcice relfileT s
      emptyCompCE t = FPathComponentE fPathComponentEmptyE relfileT t

      illegalPC t = let s = toString t
                        fpipce e = fPathIllegalE e
                        fpipce' e f = FPathComponentE (fpipce e) relfileT f
                     in testCase ("illegal path component '" ⊕ s ⊕ "'") $
                          Left (fpipce' s t) @=? parseRelFile_ t
      badChar s p = testCase ("bad component " ⊕ toString s) $
                        Left (illegalCE s p) @=? parseRelFile_ s
      absfile t  = testCase ("absolute file '" ⊕ toString t ⊕ "'") $
                      Left (fPathAbsE relfileT t) @=? parseRelFile_ t

      notAFile t = testCase ("not a file: '" ⊕ toString t ⊕ "'") $
                      Left (fPathNotAFileE relfileT t) @=? parseRelFile_ t
      parseRelFile_ ∷ MonadError FPathError η ⇒ Text → η RelFile
      parseRelFile_ = parse'
   in testGroup "parseRelFile"
                [ testCase "rf1" $ Right rf1 @=? parseRelFile_ "r.e"
                , testCase "rf1" $ Right rf1 @=? parseRelFile_ "./r.e"
                , testCase "rf4" $ Right rf4 @=? parseRelFile_ ".x"
                , testCase "rf2" $ Right rf2 @=? parseRelFile_ "r/p.x"
                , testCase "rf3" $ Right rf3 @=? parseRelFile_ "p/q/r.mp3"
                , absfile "/p.e"
                , absfile "/r/p.e"
                , testCase "empty" $
                      Left (fPathEmptyE relfileT) @=? parseRelFile_ ""
                , notAFile "./p/"
                , notAFile "/r/"
                , notAFile "./"
                , illegalPC "."
                , illegalPC ".."
                , badChar "x/\0/y" "\0"
                , badChar "r/p\0" "p\0"
                , badChar "\0r/p" "\0r"
                , testCase "empty component" $
                      Left (emptyCompCE "r//p") @=? parseRelFile_ "r//p"
                ]

relfileQQTests ∷ TestTree
relfileQQTests =
  testGroup "relfile"
            [ testCase "rf1" $ rf1 ≟ [relfile|r.e|]
            , testCase "rf2" $ rf2 ≟ [relfile|r/p.x|]
            , testCase "rf3" $ rf3 ≟ [relfile|p/q/r.mp3|]
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
                [ testCase "rf1" $ pure [pc|r.e|]           @=? toNonEmpty rf1
                , testCase "rf2" $ [pc|r|] :| [ [pc|p.x|] ] @=? toNonEmpty rf2
                , testCase "rf3" $
                    [pc|p|] :| [ [pc|q|], [pc|r.mp3|] ]     @=? toNonEmpty rf3
                , testCase "rf4" $ pure [pc|.x|]            @=? toNonEmpty rf4
                ]
    ]

relFileIsMonoSeqNEGetterTests ∷ TestTree
relFileIsMonoSeqNEGetterTests =
  testGroup "getter"
            [ testCase "rf1" $ (pure [pc|r.e|])                 @=? rf1 ⊣ seqNE
            , testCase "rf2" $
                fromNonEmpty ([pc|r|]:|[[pc|p.x|]]) @=? rf2 ⊣ seqNE
            , testCase "rf3" $
                fromNonEmpty ([pc|p|] :| [[pc|q|],[pc|r.mp3|]]) @=? rf3 ⊣ seqNE
            , testCase "rf4" $ fromNonEmpty (pure [pc|.x|])     @=? rf4 ⊣ seqNE
            ]

relFileIsMonoSeqNESetterTests ∷ TestTree
relFileIsMonoSeqNESetterTests =
  let (~!~) ∷ RelFile → NonEmpty PathComponent → RelFile
      x ~!~ y = x & seqNE ⊢ fromNonEmpty y
   in testGroup "setter"
                [ testCase "rf1"  $ rf1 ≟ rf4 ~!~ pure [pc|r.e|]
                , testCase "rf2"  $ rf2 ≟ rf4 ~!~ ([pc|r|] :| [[pc|p.x|]])
                , testCase "rf3"  $ rf3 ≟ rf2 ~!~ ([pc|p|] :| [[pc|q|],[pc|r.mp3|]])
                , testCase "rf3'" $
                     [relfile|p/t/r.mp3|] ≟ (rf3 & seqNE ⊥ 1 ⊢ [pc|t|])
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
            [ testCase "rf1" $ "r.e"       ≟ toText rf1
            , testCase "rf2" $ "r/p.x"     ≟ toText rf2
            , testCase "rf3" $ "p/q/r.mp3" ≟ toText rf3
            , testCase "rf4" $ ".x"        ≟ toText rf4
            ]

relFileTextualTests ∷ TestTree
relFileTextualTests =
  let nothin'     ∷ Maybe RelFile
      nothin'     = Nothing
      success e s = testCase s $ Parsed e  @=? parseString s
      fail s      = testCase s $ nothin'   @=? fromString s
   in testGroup "Textual" [ success rf1 "r.e"
                          , success rf2 "r/p.x"
                          , success rf3 "p/q/r.mp3"
                          , success rf4 ".x"
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
                    [relfile|usr|] ≟ omap (const [pc|usr|]) rf1
            , testCase "rf3.d" $ [relfile|p.d/q.d/r.mp3.d|] ≟ (◇ [pc|.d|]) ⪧ rf3
            , testCase "rf3 toUpper" $ [relfile|P/Q/R.MP3|] ≟  rf3 ⪦ toUpper
            ]

relFileIsMonoSeqNETests ∷ TestTree
relFileIsMonoSeqNETests =
  testGroup "IsMonoSeqNE" [ relFileIsMonoSeqNEGetterTests
                          , relFileIsMonoSeqNESetterTests ]

relFileParentMayGetterTests ∷ TestTree
relFileParentMayGetterTests =
   testGroup "getter" [ testCase "rf1" $ Just r0             @=? rf1 ⊣ parentMay
                      , testCase "rf2" $ Just r1             @=? rf2 ⊣ parentMay
                      , testCase "rf3" $ Just [reldir|p/q/|] @=? rf3 ⊣ parentMay
                      , testCase "rf4" $ Just r0             @=? rf4 ⊣ parentMay
                      ]

relFileParentMaySetterTests ∷ TestTree
relFileParentMaySetterTests =
  let (~~) ∷ RelFile → RelDir → RelFile
      d ~~ d' = d & parentMay ?~ d'
   in testGroup "setter" [ testCase "rf1 → r0" $ rf1                 ≟ rf1 ~~ r0
                         , testCase "rf2 → r0" $ [relfile|p.x|]      ≟ rf2 ~~ r0
                         , testCase "rf1 → r3" $ [relfile|p/q/r/r.e|]≟ rf1 ~~ r3
                         , testCase "rf3 → r1" $ [relfile|r/r.mp3|]  ≟ rf3 ~~ r1
                         , testCase "rf3 → r0" $ [relfile|r.mp3|]    ≟ rf3 ~~ r0
                         , testCase "rf2 → r1" $ rf2                 ≟ rf2 ~~ r1
                         , testCase "rf1 → r2" $ [relfile|r/p/r.e|]  ≟ rf1 ~~ r2

                         , testCase "rf2 → r0 (Nothing)" $
                            [relfile|p.x|] ≟ (rf2 & parentMay ⊢ Nothing)
                         ]

relFileParentMayAdjusterTests ∷ TestTree
relFileParentMayAdjusterTests =
  -- reverse the directories in the parent seq
  testGroup "adjuster" [ testCase "rf3 reverse" $
                           let reverseP = fmap (& seq ⊧ Seq.reverse)
                            in   [relfile|q/p/r.mp3|]
                               ≟ (rf3 & parentMay ⊧ reverseP)

                       ]

relFileParentMayTests ∷ TestTree
relFileParentMayTests = testGroup "parentMay"
                                 [ relFileParentMayGetterTests
                                 , relFileParentMaySetterTests
                                 , relFileParentMayAdjusterTests
                                 ]


relFileParentTests ∷ TestTree
relFileParentTests =
  let (~~) ∷ RelFile → RelDir → RelFile
      f ~~ d' = f & parent ⊢ d'
   in testGroup "parent"
                [ testCase "rf1"       $ [reldir|./|]   ≟ rf1 ⊣ parent
                , testCase "rf2"       $ [reldir|r/|]   ≟ rf2 ⊣ parent
                , testCase "rf3"       $ [reldir|p/q/|] ≟ rf3 ⊣ parent
                , testCase "rf4"       $ [reldir|./|]   ≟ rf4 ⊣ parent
                , testCase "rf1 → r0"  $ rf1 ≟ rf1 ~~ r0
                , testCase "rf2 → p.x" $ [relfile|p.x|] ≟ rf2 ~~ r0
                , testCase "rf1 → r3"  $ [relfile|p/q/r/r.e|] ≟ rf1 ~~ r3
                , testCase "rf3 → r1"  $ [relfile|r/r.mp3|] ≟ rf3 ~~ r1

                , testCase "rf3 → r0"  $ [relfile|r.mp3|] ≟ rf3 ~~ r0
                , testCase "rf2 → r1"  $ rf2 ≟ rf2 ~~ r1
                , testCase "rf1 → r2"  $ [relfile|r/p/r.e|] ≟ rf1 ~~ r2
                ]

relFileFilepathTests ∷ TestTree
relFileFilepathTests =
  let nothin' = Nothing ∷ Maybe RelFile
      fail s  = testCase s $ nothin' @=? s ⩼ filepath
   in testGroup "filepath"
            [ testCase "rf1" $ "r.e"       ≟ rf1 ## filepath
            , testCase "rf2" $ "r/p.x"     ≟ rf2 ## filepath
            , testCase "rf3" $ "p/q/r.mp3" ≟ rf3 ## filepath
            , testCase "rf4" $ ".x"        ≟ rf4 ## filepath

            , testCase "rf1" $ Just rf1 @=? "r.e"       ⩼ filepath
            , testCase "rf2" $ Just rf2 @=? "r/p.x"     ⩼ filepath
            , testCase "rf3" $ Just rf3 @=? "p/q/r.mp3" ⩼ filepath
            , testCase "rf4" $ Just rf4 @=? ".x"        ⩼ filepath

            , fail "/etc"
            , fail "etc/"
            , fail "/etc/pam.d/"
            , fail "etc/pam.d/"
            , fail "/etc//pam.d/"
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
                [ [pc|p|], [pc|q|], [pc|r.mp3|] ] @=? otoList rf3
            , testCase "oall (F)" $
                False @=? oall (Text.any (≡ 'r' ) ∘ toText) rf3
            , testCase "oall (T)" $
                True @=? oall ((< 6) ∘ Text.length ∘ toText) rf3
            , testCase "oany (F)" $
                False @=? oany (Text.any (≡ 'x' ) ∘ toText) rf3
            , testProperty "onull" (\ (x ∷ RelFile) → False ≣ onull x)
            , testCase "olength" $
                3 ≟ olength rf3
            , testCase "olength64" $
                1 ≟ olength64 rf4
            , testCase "ocompareLength" $
               GT @=? ocompareLength rf3 (2 ∷ ℕ)
            , testCase "ofoldlM" $
                    Just [[pc|r.mp3|],[pc|q|],[pc|p|]]
                @=? ofoldlM (\ a e → Just $ e : a) [] rf3
            , testCase "ofoldMap1Ex" $
                [[pc|p|],[pc|q|],[pc|r.mp3|]] @=? ofoldMap1Ex pure rf3
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
                True @=? oelem [pc|q|] rf3
            , testCase "oelem (F)" $
                False @=? oelem [pc|x|] rf3
            , testCase "onotElem (T)" $
                True @=? onotElem [pc|x|] rf3
            , testCase "onotElem (F)" $
                False @=? onotElem [pc|q|] rf3
            ]

relFileFileTests ∷ TestTree
relFileFileTests =
  let (~~) ∷ RelFile → PathComponent → RelFile
      f ~~ d' = f & file ⊢ d'
   in testGroup "file"
                [ testCase "rf1"       $ [pc|r.e|]   ≟ rf1 ⊣ file
                , testCase "rf2"       $ [pc|p.x|]   ≟ rf2 ⊣ file
                , testCase "rf3"       $ [pc|r.mp3|] ≟ rf3 ⊣ file
                , testCase "rf4"       $ [pc|.x|]    ≟ rf4 ⊣ file

                , testCase "rf3 → a0"  $ [relfile|p/q/foo|] ≟ rf3 ~~ [pc|foo|]
                , testCase "rf2 → a1"  $ rf2 ≟ rf2 ~~ [pc|p.x|]
                , testCase "rf1 → a2"  $ [relfile|.z|] ≟ rf1 ~~ [pc|.z|]
                ]

relFileDirTests ∷ TestTree
relFileDirTests =
  let (~~) ∷ RelFile → RelDir → RelFile
      f ~~ d' = f & dir ⊢ d'
   in testGroup "dir"
                [ testCase "rf1"      $ [reldir|./|]   ≟ rf1 ⊣ dir
                , testCase "rf2"      $ [reldir|r/|]   ≟ rf2 ⊣ dir
                , testCase "rf3"      $ [reldir|p/q/|] ≟ rf3 ⊣ dir
                , testCase "rf4"      $ [reldir|./|]   ≟ rf4 ⊣ dir

                , testCase "rf3 → a0" $ [relfile|s/r.mp3|]≟rf3 ~~ [reldir|s/|]
                , testCase "rf2 → a1" $ rf2               ≟rf2 ~~ [reldir|r/|]
                , testCase "rf1 → a2" $ [relfile|p.x|]    ≟rf2 ~~ [reldir|./|]
                , testCase "rf1 → a2" $ [relfile|q/p/.x|] ≟rf4 ~~ [reldir|q/p/|]
                ]

relFileTextualGroupTests ∷ TestTree
relFileTextualGroupTests =
  testGroup "textual group" [ relFileTextualTests, relFilePrintableTests
                            , relFileTextualPrintableTests ]

relFileParentGroupTests ∷ TestTree
relFileParentGroupTests =
  testGroup "parent group" [ relFileParentTests, relFileParentMayTests ]

relFileAddExtTests ∷ TestTree
relFileAddExtTests =
  testGroup "addExt"
    [ testCase "foo.bar" $ [relfile|foo.bar|] ≟ addExt [relfile|foo|] [pc|bar|]
    , testCase "r.e.bar" $ [relfile|r.e.bar|] ≟ rf1 ⊙ [pc|bar|]
    , testCase "f.o.b.r" $ [relfile|p/q/r.mp3.b.r|] ≟ rf3 ⊙ [pc|b.r|]
    ]

relFileSplitExtTests ∷ TestTree
relFileSplitExtTests =
  testGroup "splitExt"
    [ testCase "foo/bar" $
        ([relfile|foo/bar|], Nothing) @=? splitExt [relfile|foo/bar|]
    , testCase "r/p.x"   $
        ([relfile|r/p|], Just [pc|x|]) @=? splitExt rf2
    , testCase "f.x/g.y" $
        ([relfile|f.x/g|], Just [pc|y|]) @=? splitExt [relfile|f.x/g.y|]
    , testCase "f.x/g"   $
        ([relfile|f.x/g|], Nothing) @=? splitExt  [relfile|f.x/g|]
    ]

relFileExtGetterTests ∷ TestTree
relFileExtGetterTests =
  testGroup "getter" [ testCase "foo.z/bar.x" $
                         Just [pc|x|] @=? ext [relfile|foo.z/bar.x|]
                     , testCase "foo/bar" $
                         Nothing @=? ext [relfile|foo/bar|]
                     , testCase "g/f.b.x.baz"  $
                         Just [pc|baz|] @=? ext [relfile|g/f.b.x.baz|]
                     ]

relFileExtSetterTests ∷ TestTree
relFileExtSetterTests =
  testGroup "setter"
    [ testCase "foo.bar -> foo.baz" $
            [relfile|p/foo.baz|]
          ≟ updateExt (const [pc|baz|]) [relfile|p/foo.bar|]
    , testCase "foo.x/bar -> foo.x/bar" $
            [relfile|foo.x/bar|]
          ≟ updateExt (const [pc|baz|]) [relfile|foo.x/bar|]
    , testCase "foo -> foo" $
          [relfile|foo|] ≟ updateExt (const [pc|baz|]) [relfile|foo|]
    , testCase "g/foo. -> g/foo." $
          [relfile|g/foo.|] ≟ updateExt (const [pc|baz|]) [relfile|g/foo.|]
    ]

relFileExtAdjusterTests ∷ TestTree
relFileExtAdjusterTests =
  testGroup "adjuster"
    [ testCase ".baz -> .BAR" $
        [relfile|g/fo.BAZ|] ≟ updateExt toUpper [relfile|g/fo.baz|]
    , testCase ".x.b -> .x.B" $
        [relfile|fo.x.B|] ≟ updateExt toUpper [relfile|fo.x.b|]
    , testCase ".x -> .xy"    $
        [relfile|fo.xy|]  ≟ updateExt (◇ [pc|y|]) [relfile|fo.x|]
    , testCase ".    -> ."    $
        [relfile|fo.|]    ≟ updateExt (◇ [pc|y|]) [relfile|fo.|]
    ]

relFileFileLikeTests ∷ TestTree
relFileFileLikeTests =
  testGroup "FileLike" [ relFileFileTests, relFileDirTests
                       , relFileAddExtTests, relFileSplitExtTests
                       , relFileExtGetterTests, relFileExtSetterTests
                       , relFileExtAdjusterTests
                       ]

tests ∷ TestTree
tests =
  testGroup "RelFile" [ relFileConstructionTests, relFileShowTests
                      , relFileIsNonEmptyTests
                      , relFileMonoFunctorTests
                      , relFileMonoFoldableTests
                      , relFileTextualGroupTests
                      , relFileIsMonoSeqNETests
                      , relFileParentGroupTests
                      , relFileFilepathTests
                      , relFileFileLikeTests
                      , FPath.RelFile.tests
                      ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
