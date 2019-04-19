{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath
  ( tests )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( return )
import Data.Bool            ( Bool( False ) )
import Data.Either          ( Either( Left, Right  ) )
import Data.Foldable        ( toList )
import Data.Function        ( ($), (&) )
import Data.Maybe           ( Maybe( Just, Nothing ), fromMaybe )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), typeRep )
import Numeric.Natural      ( Natural )
import System.IO            ( IO )
import Text.Show            ( show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≢) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

import Data.Sequence  ( Seq( Empty ) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), fromString, parseString, parseText
                     , parseUtf8, toString, toText, toUtf8 )

-- genvalidity -------------------------

import Data.GenValidity  ( genValid )

-- genvalidity-property ----------------

import Test.Validity.GenValidity.Property  ( genGeneratesValid )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( from )
import Control.Lens.Setter  ( (?~) )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣), (⊢), (⩼), (##) )
import Data.MoreUnicode.List     ( (⋮) )
import Data.MoreUnicode.Monad    ( (⪻) )
import Data.MoreUnicode.Tasty    ( (≟), (≣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- QuickCheck --------------------------

import Test.QuickCheck.Property  ( property )

-- tasty -------------------------------

import Test.Tasty           ( TestTree, defaultIngredients, testGroup )
import Test.Tasty.Options   ( OptionSet, singleOption )
import Test.Tasty.Runners   ( TestPattern, defaultMainWithIngredients
                            , parseTestPattern, tryIngredients )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Arbitrary( arbitrary ), Gen, Property
                              , QuickCheckReplay( QuickCheckReplay )
                              , shrink, testProperty
                              )

-- text --------------------------------

import Data.Text  ( Text )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath                ( AbsDir, NonRootAbsDir, RelDir
                            , absdir, absdirN, filepath, fromList, fromListNE
                            , nonRootAbsDir, parent, parentMay, parseAbsDir'
                            , parseRelDir', pcSeq, reldir
                            )
import FPath.Error.FPathError
                            ( FPathError( FPathAbsE, FPathComponentE
                                        , FPathEmptyE, FPathNonAbsE
                                        , FPathNotADirE
                                        )
                            )
import FPath.Error.FPathComponentError
                            ( FPathComponentError( FPathComponentEmptyE
                                                 , FPathComponentIllegalCharE )
                            )
import FPath.PathComponent  ( PathComponent, pc )

--------------------------------------------------------------------------------

root ∷ AbsDir
root = [absdir|/|]

etc ∷ AbsDir
etc = [absdir|/etc/|]

pamd ∷ AbsDir
pamd = [absdir|/etc/pam.d/|]

wgm ∷ AbsDir
wgm = [absdir|/w/g/M/|]

etcN ∷ NonRootAbsDir
etcN = [absdirN|/etc/|]

pamdN ∷ NonRootAbsDir
pamdN = [absdirN|/etc/pam.d/|]

wgmN ∷ NonRootAbsDir
wgmN = [absdirN|/w/g/M/|]

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
  testGroup "PathComponent" [ pathCArbitraryTests, pathCTextualTests
                            , pathCValidityTests ]

absParseDirTests ∷ TestTree
absParseDirTests =
  let absdirT     = typeRep (Proxy ∷ Proxy AbsDir)
      pamNUL      = "/etc/pam\0/"
      pamF        = "/etc/pam"
      illegalCE   = let fpcice = FPathComponentIllegalCharE '\0' "pam\0"
                     in FPathComponentE fpcice absdirT pamNUL
      emptyCompCE = FPathComponentE FPathComponentEmptyE absdirT "/etc//pam.d/"
      parseAbsDir_ ∷ MonadError FPathError η ⇒ Text → η AbsDir
      parseAbsDir_ = parseAbsDir'
   in testGroup "parseAbsDir"
                [ testCase "root"  $ Right root   ≟ parseAbsDir_ "/"
                , testCase "etc"   $ Right etc    ≟ parseAbsDir_ "/etc/"
                , testCase "pam.d" $ Right pamd   ≟ parseAbsDir_ "/etc/pam.d/"
                , testCase "wgm"   $ Right wgm    ≟ parseAbsDir_ "/w/g/M/"
                , testCase "no trailing /" $
                      Left (FPathNotADirE absdirT pamF) ≟ parseAbsDir_ pamF
                , testCase "empty" $
                      Left (FPathEmptyE absdirT)  ≟ parseAbsDir_ ""
                , testCase "no leading /" $
                      Left (FPathNonAbsE absdirT "etc/") ≟ parseAbsDir_ "etc/"
                , testCase "bad component" $
                      Left illegalCE ≟ parseAbsDir_ pamNUL
                , testCase "empty component" $
                      Left emptyCompCE ≟ parseAbsDir_ "/etc//pam.d/"
                ]

absDirShowTests ∷ TestTree
absDirShowTests =
  let pcl ∷ Text → [Text] → String
      pcl n ps = [fmt|(%t [%L])|] n ((\ p → "[pathComponent|" ⊕ p ⊕ "|]") ⊳ ps)
      pclad ∷ [Text] → String
      pclad = pcl "(^. from pcList)"

   in testGroup "show"
                [ testCase "root"  $ pclad []               ≟ show root
                , testCase "etc"   $ pclad ["etc"]          ≟ show etc
                , testCase "pam.d" $ pclad ["etc", "pam.d"] ≟ show pamd
                ]

absDirPrintableTests ∷ TestTree
absDirPrintableTests =
  testGroup "printable"
            [ testCase "root"  $ "/"           ≟ toText root
            , testCase "etc"   $ "/etc/"       ≟ toText etc
            , testCase "pam.d" $ "/etc/pam.d/" ≟ toText pamd
            , testCase "wgm"   $ "/w/g/M/"     ≟ toText wgm
            ]

absDirAsPCSeqGetterTests ∷ TestTree
absDirAsPCSeqGetterTests =
  testGroup "getter"
            [ testCase "root"  $ []                         ≟ toList (root  ⊣ pcSeq)
            , testCase "etc"   $ [ [pc|etc|] ]              ≟ toList (etc   ⊣ pcSeq)
            , testCase "pam.d" $ [ [pc|etc|], [pc|pam.d|] ] ≟ toList (pamd ⊣ pcSeq)
            , testCase "wgm"   $ [[pc|w|],[pc|g|],[pc|M|]]  ≟ toList (wgm ⊣ pcSeq)
            ]

absDirAsPCSeqSetterTests ∷ TestTree
absDirAsPCSeqSetterTests =
  let d_pam ∷ AbsDir
      d_pam = [absdir|/pam.d/etc/|]
      x ~~ y = x & pcSeq ⊢ Seq.fromList y
   in testGroup "setter"
                [ testCase "etc"   $ etc   ≟ root ~~ [ [pc|etc|] ]
                , testCase "root"  $ root  ≟ etc ~~ []
                , testCase "d.pam" $
                      d_pam ≟ d_pam ~~ [ [pc|pam.d|], [pc|etc|] ]
                , testCase "wgm"   $
                      wgm   ≟ (⊣ from pcSeq)
                                   (Seq.fromList [[pc|w|],[pc|g|],[pc|M|]])
                ]

absDirFromListTests ∷ TestTree
absDirFromListTests =
  let dpam ∷ AbsDir
      dpam = [absdir|/pam.d/etc/|]
   in testGroup "fromList"
                [ testCase "etc"   $ etc  ≟ fromList [ [pc|etc|] ]
                , testCase "root"  $ root ≟ fromList []
                , testCase "pam.d" $ pamd ≟ fromList [ [pc|etc|], [pc|pam.d|] ]
                , testCase "d.pam" $ dpam ≟ fromList [ [pc|pam.d|], [pc|etc|] ]
                , testCase "wgm"   $ wgm  ≟ fromList [[pc|w|],[pc|g|],[pc|M|]]
                ]

absDirNRFromListNETests ∷ TestTree
absDirNRFromListNETests =
  let dpam ∷ AbsDir
      dpam = [absdir|/pam.d/etc/|]
      x ?? y = (x ⩼ nonRootAbsDir) ≟ Just (fromListNE y)
   in testGroup "fromList"
                [ testCase "etc"   $ etc  ?? pure [pc|etc|]
                , testCase "pam.d" $ pamd ?? ([pc|etc|]⋮[[pc|pam.d|]])
                , testCase "d.pam" $ dpam ?? ([pc|pam.d|]⋮[[pc|etc|]])
                , testCase "wgm"   $ wgm  ?? ([pc|w|]⋮[[pc|g|],[pc|M|]])
                ]

absDirAsPCSeqTests ∷ TestTree
absDirAsPCSeqTests =
  testGroup "asPCSeq"  [ absDirAsPCSeqGetterTests, absDirAsPCSeqSetterTests ]

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
      d ~~ d' = d & parent ⊢ d'
   in testGroup "parent"
                [ testCase "root"   $ Nothing   ≟ par root
                , testCase "etc"    $ Just root ≟ par etc
                , testCase "pamd"  $ Just etc  ≟ par pamd

                , testCase "etc → root" $ etcN  ≟ etcN ~~ root

                , testCase "pamd → root" $ [absdirN|/pam.d/|] ≟ pamdN ~~ root

                , testCase "etc → wgm" $ [absdirN|/w/g/M/etc/|] ≟ etcN ~~ wgm
                , testCase "wgm → etc" $ [absdirN|/etc/M/|] ≟ wgmN ~~ etc

                , testCase "wgm → root" $ [absdirN|/M/|] ≟ wgmN ~~ root
                , testCase "pamd → etc" $ pamdN ≟ pamdN ~~ etc
                , testCase "etc → pamd" $
                      [absdirN|/etc/pam.d/etc/|] ≟ etcN ~~ pamd
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

propInvertibleString ∷ AbsDir → Property
propInvertibleString d =
  parseString (toString d) ≣ Parsed d

propInvertibleText ∷ AbsDir → Property
propInvertibleText d =
  parseText (toText d) ≣ Parsed d

propInvertibleUtf8 ∷ AbsDir → Property
propInvertibleUtf8 d =
  parseUtf8 (toUtf8 d) ≣ Parsed d

absDirTextualPrintableTests ∷ TestTree
absDirTextualPrintableTests =
  testGroup "textual invertibility"
            [ testProperty "parseString - toString" propInvertibleString
            , testProperty "parseText - toText" propInvertibleText
            , testProperty "parseUtf8 - toUtf8" propInvertibleUtf8
            ]


absDirFilepathTests ∷ TestTree
absDirFilepathTests =
  let nothin' = Nothing ∷ Maybe AbsDir
   in testGroup "filepath"
            [ testCase "root"  $ "/"           ≟ root    ## filepath
            , testCase "etc"   $ "/etc/"       ≟ etc     ## filepath
            , testCase "pam.d" $ "/etc/pam.d/" ≟ pamd    ## filepath
            , testCase "wgm"   $ "/w/g/M/"     ≟ wgm     ## filepath
            , testCase "/etc/" $ Just etc      ≟ "/etc/" ⩼ filepath
            , testCase "/etc"         $ nothin' ≟ "/etc" ⩼ filepath
            , testCase "/etc/pam.d"   $ nothin' ≟ "/etc/pam.d" ⩼ filepath
            , testCase "etc/"         $ nothin' ≟ "etc/" ⩼ filepath
            , testCase "etc/pam.d"    $ nothin' ≟ "etc/pam.d" ⩼ filepath
            , testCase "/etc//pam.d/" $ nothin' ≟ "/etc//pam.d/" ⩼ filepath
            , testCase "e/c"          $ nothin' ≟ "e/c" ⩼ filepath
            , testCase "\0etc"        $ nothin' ≟ "\0etc" ⩼ filepath
            , testCase "etc\0"        $ nothin' ≟ "etc\0" ⩼ filepath
            , testCase "e\0c"         $ nothin' ≟ "e\0c" ⩼ filepath
            ]

absDirTests ∷ TestTree
absDirTests =
  testGroup "AbsDir" [ absParseDirTests, absDirShowTests, absDirPrintableTests
                     , absDirAsPCSeqTests, absDirTextualTests
                     , absDirParentMayTests, absDirParentTests
                     , absDirTextualPrintableTests, absDirFilepathTests
                     , absDirFromListTests, absDirNRFromListNETests
                     ]

------------------------------------------------------------

r0 ∷ RelDir
r0 = fromList []

r0' ∷ RelDir
r0' = fromList [ [pc|.|] ]

r1 ∷ RelDir
r1 = fromList [ [pc|r|] ]

r2 ∷ RelDir
r2 = fromList [ [pc|r|],[pc|p|] ]

r3 ∷ RelDir
r3 = fromList [ [pc|p|],[pc|q|],[pc|r|] ]

relParseDirTests ∷ TestTree
relParseDirTests =
  let reldirT     = typeRep (Proxy ∷ Proxy RelDir)
      pamNUL      = "/etc/pam\0/"
      pamF        = "/etc/pam"
      illegalCE s t = let fpcice = FPathComponentIllegalCharE '\0' t
                       in FPathComponentE fpcice reldirT s
      badChar s p = testCase ("bad component " ⊕ toString s) $
                        Left (illegalCE s p) ≟ parseRelDir_ s
      emptyCompCE t = FPathComponentE FPathComponentEmptyE reldirT t
      parseRelDir_ ∷ MonadError FPathError η ⇒ Text → η RelDir
      parseRelDir_ = parseRelDir'
   in testGroup "parseRelDir"
                [ testCase "r0"  $ Right r0  ≟ parseRelDir_ ""
                , testCase "r0'" $ Right r0' ≟ parseRelDir_ "./"
                , testCase "r1"  $ Right r1  ≟ parseRelDir_ "r/"
                , testCase "r2"  $ Right r2  ≟ parseRelDir_ "r/p/"
                , testCase "r3"  $ Right r3  ≟ parseRelDir_ "p/q/r/"
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

relDirTests ∷ TestTree
relDirTests =
  testGroup "RelDir" [ relParseDirTests ]

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath" [ pathComponentTests, absDirTests, relDirTests ]

----------------------------------------

_test ∷ IO ()
_test = defaultMainWithIngredients defaultIngredients tests

_tests ∷ String → IO ()
_tests s = let tryOpt ∷ TestPattern → TestTree → Maybe (IO Bool)
               tryOpt = tryIngredients defaultIngredients ∘ singleOption
            in return () ⪻ case parseTestPattern s of
                             Nothing → return False
                             Just p  → fromMaybe (return False) $ tryOpt p tests

_testr ∷ String → Natural → IO ()
_testr s r = let replayO ∷ Natural → OptionSet
                 replayO = singleOption ∘ QuickCheckReplay ∘ Just ∘ fromIntegral
                 tryOpt ∷ TestPattern → TestTree → Maybe (IO Bool)
                 tryOpt p = tryIngredients defaultIngredients $
                                singleOption p ⊕ replayO r
              in return () ⪻ case parseTestPattern s of
                               Nothing → return False
                               Just p  → fromMaybe (return False) $ tryOpt p tests

-- that's all, folks! ----------------------------------------------------------
