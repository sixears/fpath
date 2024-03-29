{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module FPath.T.FPath.AbsFile
  ( tests )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Bool            ( Bool( False, True ) )
import Data.Either          ( Either( Left, Right  ) )
import Data.Function        ( ($), (&), const )
import Data.Functor         ( fmap )
import Data.List            ( tail )
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
import NonEmptyContainers.SeqConversions    ( IsSeq( seq ) )
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

import qualified FPath.AbsFile

import FPath.AsFilePath        ( filepath )
import FPath.Error.FPathError  ( FPathError( FPathComponentE )
                               , fPathEmptyE, fPathNonAbsE, fPathNotAFileE
                               )
import FPath.Error.FPathComponentError
                               ( fPathComponentEmptyE
                               , fPathComponentIllegalCharE, fPathIllegalE
                               )
import FPath.FileLike          ( (⊙)
                               , addExt, dir, ext, file, splitExt, updateExt )
import FPath.Parent            ( parent, parentMay )
import FPath.Parseable         ( parse' )
import FPath.PathComponent     ( PathComponent, pc, toUpper )
import FPath.AbsDir            ( AbsDir, absdir )
import FPath.AbsFile           ( AbsFile, absfile )
import FPath.T.FPath.TestData  ( af1, af2, af3, af4, a0, a1, a2, a3 )

--------------------------------------------------------------------------------

parseAbsFileTests ∷ TestTree
parseAbsFileTests =
  let absfileT    = typeRep (Proxy ∷ Proxy AbsFile)
      illegalCE s t = let fpcice = fPathComponentIllegalCharE '\0' t
                       in FPathComponentE fpcice absfileT s
      emptyCompCE t = FPathComponentE fPathComponentEmptyE absfileT t

      illegalPC t = let s = toString t
                        fpipce e = fPathIllegalE e
                        fpipce' e f = FPathComponentE (fpipce e) absfileT f
                     in testCase ("illegal path component '" ⊕ s ⊕ "'") $
                          Left (fpipce' (tail s) t) @=? parseAbsFile_ t
      badChar s p = testCase ("bad component " ⊕ toString s) $
                        Left (illegalCE s p) @=? parseAbsFile_ s
      relfile t  = testCase ("non-absolute file '" ⊕ toString t ⊕ "'") $
                      Left (fPathNonAbsE absfileT t) @=? parseAbsFile_ t

      notAFile t = testCase ("not a file: '" ⊕ toString t ⊕ "'") $
                      Left (fPathNotAFileE absfileT t) @=? parseAbsFile_ t
      parseAbsFile_ ∷ MonadError FPathError η ⇒ Text → η AbsFile
      parseAbsFile_ = parse'
   in testGroup "parseAbsFile"
                [ testCase "af1" $ Right af1 @=? parseAbsFile_ "/r.e"
                , testCase "af4" $ Right af4 @=? parseAbsFile_ "/.x"
                , testCase "af2" $ Right af2 @=? parseAbsFile_ "/r/p.x"
                , testCase "af3" $ Right af3 @=? parseAbsFile_ "/p/q/r.mp3"
                , relfile "p.e"
                , relfile "r/p.e"
                , testCase "empty" $
                      Left (fPathEmptyE absfileT) @=? parseAbsFile_ ""
                , notAFile "./p/"
                , notAFile "/r/"
                , notAFile "./"
                , illegalPC "/."
                , illegalPC "/.."
                , badChar "/x/\0/y" "\0"
                , badChar "/r/p\0" "p\0"
                , badChar "/\0r/p" "\0r"
                , testCase "empty component" $
                      Left (emptyCompCE "/r//p") @=? parseAbsFile_ "/r//p"
                ]

absfileQQTests ∷ TestTree
absfileQQTests =
  testGroup "absfile"
            [ testCase "af1" $ af1 ≟ [absfile|/r.e|]
            , testCase "af2" $ af2 ≟ [absfile|/r/p.x|]
            , testCase "af3" $ af3 ≟ [absfile|/p/q/r.mp3|]
            ]

absFileIsNonEmptyTests ∷ TestTree
absFileIsNonEmptyTests =
  testGroup "IsNonEmpty"
    [ testGroup "fromNonEmpty"
                [ testCase "af1" $ af1 ≟ fromNonEmpty (pure [pc|r.e|])
                , testCase "af2" $ af2 ≟ fromNonEmpty ([pc|r|] :| [ [pc|p.x|] ])
                , testCase "af3" $ af3 ≟
                    fromNonEmpty ([pc|p|] :| [ [pc|q|], [pc|r.mp3|] ])
                , testCase "af4" $ af4 ≟ fromNonEmpty (pure [pc|.x|])
                ]
    , testGroup "toNonEmpty"
                [ testCase "af1" $ pure [pc|r.e|]           @=? toNonEmpty af1
                , testCase "af2" $ [pc|r|] :| [ [pc|p.x|] ] @=? toNonEmpty af2
                , testCase "af3" $
                    [pc|p|] :| [ [pc|q|], [pc|r.mp3|] ]     @=? toNonEmpty af3
                , testCase "af4" $ pure [pc|.x|]            @=? toNonEmpty af4
                ]
    ]

absFileIsMonoSeqNEGetterTests ∷ TestTree
absFileIsMonoSeqNEGetterTests =
  testGroup "getter"
            [ testCase "af1" $ pure [pc|r.e|]                   @=? af1 ⊣ seqNE
            , testCase "af2" $
                  fromNonEmpty ([pc|r|]:|[[pc|p.x|]]) @=? af2 ⊣ seqNE
            , testCase "af3" $
                fromNonEmpty ([pc|p|] :| [[pc|q|],[pc|r.mp3|]]) @=? af3 ⊣ seqNE
            , testCase "af4" $ fromNonEmpty (pure [pc|.x|])     @=? af4 ⊣ seqNE
            ]

absFileIsMonoSeqNESetterTests ∷ TestTree
absFileIsMonoSeqNESetterTests =
  let (~!~) ∷ AbsFile → NonEmpty PathComponent → AbsFile
      x ~!~ y = x & seqNE ⊢ fromNonEmpty y
   in testGroup "setter"
                [ testCase "af1"  $ af1 ≟ af4 ~!~ pure [pc|r.e|]
                , testCase "af2"  $ af2 ≟ af4 ~!~ ([pc|r|] :| [[pc|p.x|]])
                , testCase "af3"  $ af3 ≟ af2 ~!~ ([pc|p|] :| [[pc|q|],[pc|r.mp3|]])
                , testCase "af3'" $
                     [absfile|/p/t/r.mp3|] ≟ (af3 & seqNE ⊥ 1 ⊢ [pc|t|])
                ]

absFileShowTests ∷ TestTree
absFileShowTests =
  testGroup "show"
            [ testCase "af1" $ "[absfile|/r.e|]"       ≟ show af1
            , testCase "af2" $ "[absfile|/r/p.x|]"     ≟ show af2
            , testCase "af3" $ "[absfile|/p/q/r.mp3|]" ≟ show af3
            , testCase "af4" $ "[absfile|/.x|]"        ≟ show af4
            ]

absFilePrintableTests ∷ TestTree
absFilePrintableTests =
  testGroup "printable"
            [ testCase "af1" $ "/r.e"       ≟ toText af1
            , testCase "af2" $ "/r/p.x"     ≟ toText af2
            , testCase "af3" $ "/p/q/r.mp3" ≟ toText af3
            , testCase "af4" $ "/.x"        ≟ toText af4
            ]

absFileTextualTests ∷ TestTree
absFileTextualTests =
  let nothin'     ∷ Maybe AbsFile
      nothin'     = Nothing
      success e s = testCase s $ Parsed e  @=? parseString s
      fail s      = testCase s $ nothin'   @=? fromString s
   in testGroup "Textual" [ success af1 "/r.e"
                          , success af2 "/r/p.x"
                          , success af3 "/p/q/r.mp3"
                          , success af4 "/.x"
                          , fail "etc"
                          , fail "etc/pam.d"
                          , success [absfile|/etc|] "/etc"
                          , success [absfile|/etc/pam.d|] "/etc/pam.d"
                          , fail "etc//pam.d/"
                          , success [absfile|/e/c|] "/e/c"
                          , fail "/e/c/"
                          , fail "/\0etc"
                          , fail "/etc\0"
                          , fail "/e\0c"
                          ]

absFileTextualPrintableTests ∷ TestTree
absFileTextualPrintableTests =
  testGroup "invertibility"
            [ testProperty "parseString - toString"
                           (propInvertibleString @AbsFile)
            , testProperty "parseText - toText" (propInvertibleText @AbsFile)
            , testProperty "parseUtf8 - toUtf8" (propInvertibleUtf8 @AbsFile)
            ]

absFileMonoFunctorTests ∷ TestTree
absFileMonoFunctorTests =
  testGroup "MonoFunctor"
            [ testCase "af1 → usr" $
                [absfile|/usr|] ≟ omap (const [pc|usr|]) af1
            , testCase "af3.d" $
                [absfile|/p.d/q.d/r.mp3.d|] ≟ (◇ [pc|.d|]) ⪧ af3
            , testCase "af3 toUpper" $
                [absfile|/P/Q/R.MP3|] ≟  af3 ⪦ toUpper
            ]

absFileIsMonoSeqNETests ∷ TestTree
absFileIsMonoSeqNETests =
  testGroup "IsMonoSeqNE" [ absFileIsMonoSeqNEGetterTests
                          , absFileIsMonoSeqNESetterTests ]

absFileParentMayGetterTests ∷ TestTree
absFileParentMayGetterTests =
   testGroup "getter" [ testCase "af1" $ Just a0              @=? af1⊣ parentMay
                      , testCase "af2" $ Just a1              @=? af2⊣ parentMay
                      , testCase "af3" $ Just [absdir|/p/q/|] @=? af3⊣ parentMay
                      , testCase "af4" $ Just a0              @=? af4⊣ parentMay
                      ]

absFileParentMaySetterTests ∷ TestTree
absFileParentMaySetterTests =
  let (~~) ∷ AbsFile → AbsDir → AbsFile
      d ~~ d' = d & parentMay ?~ d'
   in testGroup "setter" [ testCase "af1 → a0" $ af1                 ≟ af1 ~~ a0
                         , testCase "af2 → a0" $ [absfile|/p.x|]     ≟ af2 ~~ a0
                         , testCase "af3 → a1" $ [absfile|/r/r.mp3|] ≟ af3 ~~ a1
                         , testCase "af3 → a0" $ [absfile|/r.mp3|]   ≟ af3 ~~ a0
                         , testCase "af2 → a1" $ af2                 ≟ af2 ~~ a1
                         , testCase "af1 → a2" $ [absfile|/r/p/r.e|] ≟ af1 ~~ a2
                         , testCase "af1 → a3" $
                             [absfile|/p/q/r/r.e|] ≟ af1 ~~ a3
                         , testCase "af2 → a0 (Nothing)" $
                            [absfile|/p.x|] ≟ (af2 & parentMay ⊢ Nothing)
                         ]

absFileParentMayAdjusterTests ∷ TestTree
absFileParentMayAdjusterTests =
  -- reverse the directories in the parent seq
  testGroup "adjuster" [ testCase "af3 reverse" $
                           let reverseP = fmap (& seq ⊧ Seq.reverse)
                            in   [absfile|/q/p/r.mp3|]
                               ≟ (af3 & parentMay ⊧ reverseP)

                       ]

absFileParentMayTests ∷ TestTree
absFileParentMayTests = testGroup "parentMay"
                                 [ absFileParentMayGetterTests
                                 , absFileParentMaySetterTests
                                 , absFileParentMayAdjusterTests
                                 ]


absFileParentTests ∷ TestTree
absFileParentTests =
  let (~~) ∷ AbsFile → AbsDir → AbsFile
      f ~~ d' = f & parent ⊢ d'
   in testGroup "parent"
                [ testCase "af1"       $ [absdir|/|]   ≟ af1 ⊣ parent
                , testCase "af2"       $ [absdir|/r/|]   ≟ af2 ⊣ parent
                , testCase "af3"       $ [absdir|/p/q/|] ≟ af3 ⊣ parent
                , testCase "af4"       $ [absdir|/|]   ≟ af4 ⊣ parent
                , testCase "af1 → a0"  $ af1 ≟ af1 ~~ a0
                , testCase "af2 → p.x" $ [absfile|/p.x|] ≟ af2 ~~ a0
                , testCase "af1 → a3"  $ [absfile|/p/q/r/r.e|] ≟ af1 ~~ a3
                , testCase "af3 → a1"  $ [absfile|/r/r.mp3|] ≟ af3 ~~ a1

                , testCase "af3 → a0"  $ [absfile|/r.mp3|] ≟ af3 ~~ a0
                , testCase "af2 → a1"  $ af2 ≟ af2 ~~ a1
                , testCase "af1 → a2"  $ [absfile|/r/p/r.e|] ≟ af1 ~~ a2
                ]

absFileFileTests ∷ TestTree
absFileFileTests =
  let (~~) ∷ AbsFile → PathComponent → AbsFile
      f ~~ d' = f & file ⊢ d'
   in testGroup "file"
                [ testCase "af1"       $ [pc|r.e|]   ≟ af1 ⊣ file
                , testCase "af2"       $ [pc|p.x|]   ≟ af2 ⊣ file
                , testCase "af3"       $ [pc|r.mp3|] ≟ af3 ⊣ file
                , testCase "af4"       $ [pc|.x|]   ≟ af4 ⊣ file

                , testCase "af3 → a0"  $ [absfile|/p/q/foo|] ≟ af3 ~~ [pc|foo|]
                , testCase "af2 → a1"  $ af2 ≟ af2 ~~ [pc|p.x|]
                , testCase "af1 → a2"  $ [absfile|/.z|] ≟ af1 ~~ [pc|.z|]
                ]

absFileFilepathTests ∷ TestTree
absFileFilepathTests =
  let nothin' = Nothing ∷ Maybe AbsFile
      fail s  = testCase s $ nothin' @=? s ⩼ filepath
   in testGroup "filepath"
            [ testCase "af1" $ "/r.e"       ≟ af1 ## filepath
            , testCase "af2" $ "/r/p.x"     ≟ af2 ## filepath
            , testCase "af3" $ "/p/q/r.mp3" ≟ af3 ## filepath
            , testCase "af4" $ "/.x"        ≟ af4 ## filepath

            , testCase "af1" $ Just af1 @=? "/r.e"       ⩼ filepath
            , testCase "af2" $ Just af2 @=? "/r/p.x"     ⩼ filepath
            , testCase "af3" $ Just af3 @=? "/p/q/r.mp3" ⩼ filepath
            , testCase "af4" $ Just af4 @=? "/.x"        ⩼ filepath

            , fail "etc"
            , fail "/etc/"
            , fail "etc/pam.d/"
            , fail "/etc/pam.d/"
            , fail "etc//pam.d/"
            , fail "/\0etc"
            , fail "/etc\0"
            , fail "/e\0c"
            ]

absFileConstructionTests ∷ TestTree
absFileConstructionTests = testGroup "construction" [ parseAbsFileTests
                                                    , absfileQQTests ]

absFileMonoFoldableTests ∷ TestTree
absFileMonoFoldableTests =
  testGroup "MonoFoldable"
            [ testCase "ofoldMap" $
                "p-q-r.mp3-" ≟ ofoldMap ((⊕ "-") ∘ toText) af3
            , testCase "ofoldr" $
                "p-q-r.mp3-ф" ≟ ofoldr (\ a b → toText a ⊕ "-" ⊕ b) "ф" af3
            , testCase "ofoldl'" $
                "ф-p-q-r.mp3" ≟ ofoldl' (\ b a → b ⊕ "-" ⊕ toText a) "ф" af3
            , testCase "otoList" $
                [ [pc|p|], [pc|q|], [pc|r.mp3|] ] @=? otoList af3
            , testCase "oall (F)" $
                False @=? oall (Text.any (≡ 'r' ) ∘ toText) af3
            , testCase "oall (T)" $
                True @=? oall ((< 6) ∘ Text.length ∘ toText) af3
            , testCase "oany (F)" $
                False @=? oany (Text.any (≡ 'x' ) ∘ toText) af3
            , testProperty "onull" (\ (x ∷ AbsFile) → False ≣ onull x)
            , testCase "olength" $
                3 @=? olength af3
            , testCase "olength64" $
                1 ≟ olength64 af4
            , testCase "ocompareLength" $
               GT @=? ocompareLength af3 (2 ∷ ℕ)
            , testCase "ofoldlM" $
                  Just [[pc|r.mp3|],[pc|q|],[pc|p|]]
                @=? ofoldlM (\ a e → Just $ e : a) [] af3
            , testCase "ofoldMap1Ex" $
                [[pc|p|],[pc|q|],[pc|r.mp3|]] @=? ofoldMap1Ex pure af3
            , testCase "ofoldr1Ex" $
                [pc|pqr.mp3|] ≟ ofoldr1Ex (◇) af3
            , testCase "ofoldl1Ex'" $
                [pc|pqr.mp3|] ≟ ofoldl1Ex' (◇) af3
            , testCase "unsafeHead" $
                [pc|p|] ≟ unsafeHead af3
            , testCase "unsafeLast" $
                [pc|r.mp3|] ≟ unsafeLast af3
            , testCase "maximumByEx" $
                [pc|r.mp3|] ≟ maximumByEx (comparing toText) af3
            , testCase "minimumByEx" $
                [pc|p|] ≟ minimumByEx (comparing toText) af3
            , testCase "oelem (T)" $
                True @=? oelem [pc|q|] af3
            , testCase "oelem (F)" $
                False @=? oelem [pc|x|] af3
            , testCase "onotElem (T)" $
                True @=? onotElem [pc|x|] af3
            , testCase "onotElem (F)" $
                False @=? onotElem [pc|q|] af3
            ]


absFileDirTests ∷ TestTree
absFileDirTests =
  let (~~) ∷ AbsFile → AbsDir → AbsFile
      f ~~ d' = f & dir ⊢ d'
   in testGroup "dir"
                [ testCase "af1"      $ [absdir|/|]     ≟ af1 ⊣ dir
                , testCase "af2"      $ [absdir|/r/|]   ≟ af2 ⊣ dir
                , testCase "af3"      $ [absdir|/p/q/|] ≟ af3 ⊣ dir
                , testCase "af4"      $ [absdir|/|]     ≟ af4 ⊣ dir

                , testCase "af3 → a0" $ [absfile|/s/r.mp3|]≟af3 ~~ [absdir|/s/|]
                , testCase "af2 → a1" $ af2                ≟af2 ~~ [absdir|/r/|]
                , testCase "af1 → a2" $ [absfile|/p.x|]    ≟af2 ~~ [absdir|/|]
                , testCase "af1 → a2" $
                    [absfile|/q/p/.x|] ≟af4 ~~ [absdir|/q/p/|]
                ]

absFileTextualGroupTests ∷ TestTree
absFileTextualGroupTests =
  testGroup "textual group" [ absFileTextualTests, absFilePrintableTests
                            , absFileTextualPrintableTests ]

absFileParentGroupTests ∷ TestTree
absFileParentGroupTests =
  testGroup "parent group" [ absFileParentTests, absFileParentMayTests ]


absFileAddExtTests ∷ TestTree
absFileAddExtTests =
  testGroup "addExt"
    [ testCase "foo.bar" $ [absfile|/foo.bar|] ≟ addExt [absfile|/foo|] [pc|bar|]
    , testCase "r.e.bar" $ [absfile|/r.e.bar|] ≟ af1 ⊙ [pc|bar|]
    , testCase "f.o.b.r" $ [absfile|/p/q/r.mp3.b.r|] ≟ af3 ⊙ [pc|b.r|]
    ]

absFileSplitExtTests ∷ TestTree
absFileSplitExtTests =
  testGroup "splitExt"
    [ testCase "foo/bar" $
        ([absfile|/foo/bar|], Nothing) @=? splitExt [absfile|/foo/bar|]
    , testCase "r/p.x"   $
        ([absfile|/r/p|],Just [pc|x|]) @=? splitExt af2
    , testCase "f.x/g.y" $
        ([absfile|/f.x/g|], Just [pc|y|]) @=? splitExt [absfile|/f.x/g.y|]
    , testCase "f.x/g"   $
        ([absfile|/f.x/g|], Nothing) @=? splitExt [absfile|/f.x/g|]
    ]

absFileExtGetterTests ∷ TestTree
absFileExtGetterTests =
  testGroup "getter" [ testCase "foo.z/bar.x" $
                         Just [pc|x|] @=? ext [absfile|/foo.z/bar.x|]
                     , testCase "foo/bar" $
                         Nothing @=? ext [absfile|/foo/bar|]
                     , testCase "g/f.b.x.baz"  $
                         Just [pc|baz|] @=? ext [absfile|/g/f.b.x.baz|]
                     ]

absFileExtSetterTests ∷ TestTree
absFileExtSetterTests =
  testGroup "setter"
    [ testCase "foo.bar -> foo.baz" $
          [absfile|/p/foo.baz|] ≟ updateExt (const [pc|baz|]) [absfile|/p/foo.bar|]
    , testCase "/foo.x/bar -> /foo.x/bar" $
          [absfile|/foo.x/bar|] ≟ updateExt (const [pc|baz|]) [absfile|/foo.x/bar|]
    , testCase "foo -> foo" $
          [absfile|/foo|] ≟ updateExt (const [pc|baz|]) [absfile|/foo|]
    , testCase "g/foo. -> g/foo." $
          [absfile|/g/foo.|] ≟ updateExt (const [pc|baz|]) [absfile|/g/foo.|]
    ]

absFileExtAdjusterTests ∷ TestTree
absFileExtAdjusterTests =
  testGroup "adjuster"
    [ testCase ".baz -> .BAR" $
        [absfile|/g/fo.BAZ|] ≟ updateExt toUpper [absfile|/g/fo.baz|]
    , testCase ".x.b -> .x.B" $
        [absfile|/fo.x.B|]   ≟ updateExt toUpper [absfile|/fo.x.b|]
    , testCase ".x -> .xy"    $
        [absfile|/fo.xy|]    ≟ updateExt (◇ [pc|y|]) [absfile|/fo.x|]
    , testCase ".    -> ."    $
        [absfile|/fo.|]      ≟ updateExt (◇ [pc|y|]) [absfile|/fo.|]

    ]

absFileFileLikeTests ∷ TestTree
absFileFileLikeTests =
  testGroup "FileLike" [ absFileFileTests, absFileDirTests
                       , absFileAddExtTests, absFileSplitExtTests
                       , absFileExtGetterTests, absFileExtSetterTests
                       , absFileExtAdjusterTests
                       ]

tests ∷ TestTree
tests =
  testGroup "AbsFile" [ absFileConstructionTests, absFileShowTests
                      , absFileIsNonEmptyTests
                      , absFileMonoFunctorTests
                      , absFileMonoFoldableTests
                      , absFileTextualGroupTests
                      , absFileIsMonoSeqNETests
                      , absFileParentGroupTests
                      , absFileFilepathTests
                      , absFileFileLikeTests
                      , FPath.AbsFile.tests
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
