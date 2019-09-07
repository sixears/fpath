{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath.AbsDir
  ( tests )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Bool            ( Bool( False, True ) )
import Data.Function        ( ($), (&), const )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Ord             ( Ordering( GT ), (<), comparing )
import Data.String          ( String )
import GHC.Exts             ( fromList, toList )
import Numeric.Natural      ( Natural )
import System.IO            ( IO )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

import Data.Sequence  ( Seq( Empty ) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), fromString, parseString, toText )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Setter  ( (?~) )

-- mono-traversable --------------------

import Data.MonoTraversable  ( maximumByEx, minimumByEx, oall, oany
                             , ocompareLength, oelem, ofoldl', ofoldl1Ex'
                             , ofoldlM, ofoldMap, ofoldMap1Ex, ofoldr, ofoldr1Ex
                             , olength, olength64, omap, onotElem, onull
                             , otoList, unsafeHead, unsafeLast )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor          ( (⊳) )
import Data.MoreUnicode.Lens             ( (⊣), (⫣), (⊢), (⩼), (##) )
import Data.MoreUnicode.MonoTraversable  ( (⪦), (⪧) )
import Data.MoreUnicode.Natural          ( ℕ )
import Data.MoreUnicode.Semigroup        ( (◇) )
import Data.MoreUnicode.Tasty            ( (≟), (≣) )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqConversions ( IsMonoSeq( seq ) )

-- tasty -------------------------------

import Test.Tasty           ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text --------------------------------

import qualified  Data.Text  as  Text

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  FPath.AbsDir

import FPath.AbsDir            ( AbsDir, absdir, nonRootAbsDir )
import FPath.AsFilePath        ( filepath )
import FPath.Parent            ( parent, parentMay )
import FPath.PathComponent     ( pc, toUpper )
import FPath.T.Common          ( doTest, doTestR, doTestS
                               , propInvertibleString, propInvertibleText
                               , propInvertibleUtf8 )
import FPath.T.FPath.TestData  ( etc, pamd, root, wgm )

--------------------------------------------------------------------------------

absdirQQTests ∷ TestTree
absdirQQTests =
  testGroup "reldir"
            [ testCase "root" $ root ≟ [absdir|/|]
            , testCase "/etc" $ etc ≟ [absdir|/etc/|]
            , testCase "/etc/pam.d" $ pamd ≟ [absdir|/etc/pam.d/|]
            , testCase "/w/g/M" $ wgm ≟ [absdir|/w/g/M/|]
            ]

absDirShowTests ∷ TestTree
absDirShowTests =
  let fromNonEmptyT = "NonEmptyContainers.IsNonEmpty.fromNonEmpty"
      pcT t         = "PathComponent \"" ⊕ t ⊕ "\""
      etcT          = pcT "etc"
      pamdT         = pcT "pam.d"
      nonRT         = "AbsNonRootDir (NonRootAbsDir "
      rootShow = "AbsRootDir"
      etcShow  = nonRT ⊕ fromNonEmptyT ⊕ " (" ⊕ pcT "etc" ⊕ " :| []))"
      pamdShow = nonRT ⊕ fromNonEmptyT ⊕ " (" ⊕ etcT ⊕ " :| [" ⊕ pamdT ⊕ "]))"

   in testGroup "show"
                [ testCase "root"  $ rootShow ≟ show root
                , testCase "etc"   $ etcShow  ≟ show etc
                , testCase "pam.d" $ pamdShow ≟ show pamd
                ]

absDirPrintableTests ∷ TestTree
absDirPrintableTests =
  testGroup "printable"
            [ testCase "root"  $ "/"           ≟ toText root
            , testCase "etc"   $ "/etc/"       ≟ toText etc
            , testCase "pam.d" $ "/etc/pam.d/" ≟ toText pamd
            , testCase "wgm"   $ "/w/g/M/"     ≟ toText wgm
            ]

absDirIsListTests ∷ TestTree
absDirIsListTests =
  testGroup "IsList"
    [ testGroup "fromList"
                [ testCase "root"  $ root ≟ fromList []
                , testCase "etc"   $ etc  ≟ fromList [ [pc|etc|] ]
                , testCase "pam.d" $ pamd ≟ fromList [ [pc|etc|], [pc|pam.d|] ]
                , testCase "wgm"   $ wgm  ≟ fromList [ [pc|w|],[pc|g|],[pc|M|] ]
                ]
    , testGroup "toList"
                [ testCase "root"  $ []                            ≟ toList root
                , testCase "etc"   $ [ [pc|etc|] ]                 ≟ toList etc
                , testCase "pam.d" $ [ [pc|etc|], [pc|pam.d|] ]    ≟ toList pamd
                , testCase "wgm"   $ [ [pc|w|], [pc|g|], [pc|M|] ] ≟ toList wgm
                ]
    ]

absDirMonoFoldableTests ∷ TestTree
absDirMonoFoldableTests =
  testGroup "MonoFoldable"
            [ testCase "ofoldMap" $
                "w-g-M-" ≟ ofoldMap ((⊕ "-") ∘ toText) wgm
            , testCase "ofoldr" $
                "w-g-M-ф" ≟ ofoldr (\ a b → toText a ⊕ "-" ⊕ b) "ф" wgm
            , testCase "ofoldl'" $
                "ф-w-g-M" ≟ ofoldl' (\ b a → b ⊕ "-" ⊕ toText a) "ф" wgm
            , testCase "otoList" $
                [ [pc|w|], [pc|g|], [pc|M|] ] ≟ otoList wgm
            , testCase "oall (F)" $
                False ≟ oall (Text.any (≡ 'r' ) ∘ toText) wgm
            , testCase "oall (T)" $
                True ≟ oall ((< 6) ∘ Text.length ∘ toText) wgm
            , testCase "oany (F)" $
                False ≟ oany (Text.any (≡ 'x' ) ∘ toText) wgm
            , testProperty "onull" (\ x → (x ≡ [absdir|/|]) ≣ onull x)
            , testCase "olength" $
                3 ≟ olength wgm
            , testCase "olength64" $
                0 ≟ olength64 root
            , testCase "ocompareLength" $
               GT ≟ ocompareLength wgm (2 ∷ ℕ)
            , testCase "ofoldlM" $
                  Just [[pc|M|],[pc|g|],[pc|w|]]
                ≟ ofoldlM (\ a e → Just $ e : a) [] wgm
            , testCase "ofoldMap1Ex" $
                [[pc|w|],[pc|g|],[pc|M|]] ≟ ofoldMap1Ex pure wgm
            , testCase "ofoldr1Ex" $
                [pc|wgM|] ≟ ofoldr1Ex (◇) wgm
            , testCase "ofoldl1Ex'" $
                [pc|wgM|] ≟ ofoldl1Ex' (◇) wgm
            , testCase "unsafeHead" $
                [pc|w|] ≟ unsafeHead wgm
            , testCase "unsafeLast" $
                [pc|M|] ≟ unsafeLast wgm
            , testCase "maximumByEx" $
                [pc|w|] ≟ maximumByEx (comparing toText) wgm
            , testCase "minimumByEx" $
                [pc|M|] ≟ minimumByEx (comparing toText) wgm
            , testCase "oelem (T)" $
                True ≟ oelem [pc|g|] wgm
            , testCase "oelem (F)" $
                False ≟ oelem [pc|x|] wgm
            , testCase "onotElem (T)" $
                True ≟ onotElem [pc|x|] wgm
            , testCase "onotElem (F)" $
                False ≟ onotElem [pc|g|] wgm
            ]

absDirIsMonoSeqGetterTests ∷ TestTree
absDirIsMonoSeqGetterTests =
  testGroup "getter"
            [ testCase "root"  $ Empty                   ≟ root ⊣ seq
            , testCase "etc"   $ pure [pc|etc|]          ≟ etc  ⊣ seq
            , testCase "pam.d" $ [[pc|etc|],[pc|pam.d|]] ≟ toList (pamd ⊣ seq)
            , testCase "wgm" $ [[pc|w|],[pc|g|],[pc|M|]] ≟ toList (wgm  ⊣ seq)
            ]

absDirIsMonoSeqSetterTests ∷ TestTree
absDirIsMonoSeqSetterTests =
  let d_pam ∷ AbsDir
      d_pam = [absdir|/pam.d/etc/|]
      x ~~ y = x & seq ⊢ Seq.fromList y
   in testGroup "setter"
                [ testCase "etc"   $ etc   ≟ root ~~ [ [pc|etc|] ]
                , testCase "root"  $ root  ≟ etc ~~ []
                , testCase "d.pam" $
                      d_pam ≟ d_pam ~~ [ [pc|pam.d|], [pc|etc|] ]
                , testCase "wgm"   $
                      wgm   ≟ (⫣ seq) (Seq.fromList [[pc|w|],[pc|g|],[pc|M|]])
                ]

absDirMonoFunctorTests ∷ TestTree
absDirMonoFunctorTests =
  testGroup "MonoFunctor"
            [ testCase "usr" $
                    [absdir|/usr/|] ≟ omap (const [pc|usr|]) etc
            , testCase "wgm.d" $ [absdir|/w.d/g.d/M.d/|] ≟ (◇ [pc|.d|]) ⪧ wgm
            , testCase "WGM" $ [absdir|/W/G/M/|] ≟ wgm ⪦ toUpper
            ]

absDirIsMonoSeqTests ∷ TestTree
absDirIsMonoSeqTests =
  testGroup "IsMonoSeq" [ absDirIsMonoSeqGetterTests
                        , absDirIsMonoSeqSetterTests ]

absDirParentMayTests ∷ TestTree
absDirParentMayTests =
  let d ~~ d' = d & parentMay ?~ d'
   in testGroup "parentMay"
                [ testCase "root"   $ Nothing   ≟ root  ⊣ parentMay
                , testCase "etc"    $ Just root ≟ etc   ⊣ parentMay
                , testCase "pamd"  $ Just etc  ≟ pamd ⊣ parentMay

                , testCase "etc → root" $ etc ≟ etc ~~ root
                , testCase "root → etc" $ etc ≟ root ~~ etc

                , testCase "pamd → root" $ [absdir|/pam.d/|] ≟ pamd ~~ root
                , testCase "root → pamd" $ pamd ≟ root ~~ pamd

                , testCase "etc → wgm" $ [absdir|/w/g/M/etc/|] ≟ etc ~~ wgm
                , testCase "wgm → etc" $ [absdir|/etc/M/|] ≟ wgm ~~ etc

                , testCase "root → wgm" $ wgm ≟ root ~~ wgm
                , testCase "wgm → root" $ [absdir|/M/|] ≟ wgm ~~ root

                , testCase "pamd → etc" $ pamd ≟ pamd ~~ etc
                , testCase "etc → pamd" $
                      [absdir|/etc/pam.d/etc/|] ≟ etc ~~ pamd

                , testCase "pamd → Nothing" $
                      [absdir|/pam.d/|]  ≟ (pamd & parentMay ⊢ Nothing)
                ]

absDirParentTests ∷ TestTree
absDirParentTests =
  let par d = (view parent) ⊳ (d ⩼ nonRootAbsDir)
   in testGroup "parent"
                [ testCase "root"        $ Nothing   ≟ par root
                , testCase "etc"         $ Just root ≟ par etc
                , testCase "pamd"        $ Just etc  ≟ par pamd
                ]

absDirTextualTests ∷ TestTree
absDirTextualTests =
  let nothin'     ∷ Maybe AbsDir
      nothin'     = Nothing
      success e s = testCase s $ Parsed e  ≟ parseString s
      fail s      = testCase s $ nothin'   ≟ fromString s
   in testGroup "Textual" [ success [absdir|/|]           "/"
                          , success [absdir|/etc/|]       "/etc/"
                          , success [absdir|/etc/pam.d/|] "/etc/pam.d/"
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

absDirTextualPrintableTests ∷ TestTree
absDirTextualPrintableTests =
  testGroup "textual invertibility"
            [ testProperty "parseString - toString"
                           (propInvertibleString @AbsDir)
            , testProperty "parseText - toText" (propInvertibleText @AbsDir)
            , testProperty "parseUtf8 - toUtf8" (propInvertibleUtf8 @AbsDir)
            ]

----------------------------------------

absDirFilepathTests ∷ TestTree
absDirFilepathTests =
  let nothin' = Nothing ∷ Maybe AbsDir
      fail s  = testCase s $ nothin' ≟ s ⩼ filepath
   in testGroup "filepath"
            [ testCase "root"  $ "/"           ≟ root    ## filepath
            , testCase "etc"   $ "/etc/"       ≟ etc     ## filepath
            , testCase "pam.d" $ "/etc/pam.d/" ≟ pamd    ## filepath
            , testCase "wgm"   $ "/w/g/M/"     ≟ wgm     ## filepath
            , testCase "/etc/" $ Just etc      ≟ "/etc/" ⩼ filepath
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

absDirConstructionTests ∷ TestTree
absDirConstructionTests = testGroup "construction" [ absdirQQTests ]

absDirTextualGroupTests ∷ TestTree
absDirTextualGroupTests =
  testGroup "textual group" [ absDirTextualTests, absDirPrintableTests
                            , absDirTextualPrintableTests ]

absDirParentGroupTests ∷ TestTree
absDirParentGroupTests =
  testGroup "parent group" [ absDirParentTests, absDirParentMayTests ]


tests ∷ TestTree
tests =
  testGroup "AbsDir" [ absDirConstructionTests, absDirShowTests
                     , absDirIsListTests
                     , absDirMonoFunctorTests
                     , absDirMonoFoldableTests
                     , absDirTextualGroupTests
                     , absDirIsMonoSeqTests
                     , absDirParentGroupTests
                     , absDirFilepathTests
                     , FPath.AbsDir.tests
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
