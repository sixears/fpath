{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath
  ( tests )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Control.Monad    ( return )
import Data.Bool        ( Bool( False ) )
import Data.Either      ( Either( Left, Right  ) )
import Data.Function    ( ($), (&) )
import Data.Maybe       ( Maybe( Just, Nothing ), fromMaybe )
import Data.String      ( String )
import Data.Typeable    ( Proxy( Proxy ), typeRep )
import Numeric.Natural  ( Natural )
import System.IO        ( IO )
import Text.Show        ( show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≢) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), fromString, parseString, parseText
                     , parseUtf8, toString, toText, toUtf8 )

-- genvalidity -------------------------

import Data.GenValidity  ( genValid )

-- genvalidity-property ----------------

import Test.Validity.GenValidity.Property  ( genGeneratesValid )

-- lens --------------------------------

import Control.Lens.Iso     ( from )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣), (⊢), (⩼), (##) )
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

import FPath                ( AbsDir, absdir, filepath, nonRootAbsDir, parent
                            , parentMay, parseAbsDir', pcList )
import FPath.Error.FPathError
                            ( FPathError( FPathComponentE, FPathEmptyE
                                        , FPathNonAbsE, FPathNotADirE ) )
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

pam_d ∷ AbsDir
pam_d = [absdir|/etc/pam.d/|]

d_pam ∷ AbsDir
d_pam = [absdir|/pam.d/etc/|]

wgM ∷ AbsDir
wgM = [absdir|/w/g/M/|]

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
                , testCase "pam.d" $ Right pam_d  ≟ parseAbsDir_ "/etc/pam.d/"
                , testCase "wgM"   $ Right wgM    ≟ parseAbsDir_ "/w/g/M/"
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
                , testCase "pam.d" $ pclad ["etc", "pam.d"] ≟ show pam_d
                ]

absDirPrintableTests ∷ TestTree
absDirPrintableTests =
  testGroup "printable"
            [ testCase "root"  $ "/"           ≟ toText root
            , testCase "etc"   $ "/etc/"       ≟ toText etc
            , testCase "pam.d" $ "/etc/pam.d/" ≟ toText pam_d
            , testCase "wgM"   $ "/w/g/M/"     ≟ toText wgM
            ]

absDirAsPCListGetterTests ∷ TestTree
absDirAsPCListGetterTests =
  testGroup "getter"
            [ testCase "root"  $ []                         ≟ root  ⊣ pcList
            , testCase "etc"   $ [ [pc|etc|] ]              ≟ etc   ⊣ pcList
            , testCase "pam.d" $ [ [pc|etc|], [pc|pam.d|] ] ≟ pam_d ⊣ pcList
            , testCase "wgM"   $ [[pc|w|],[pc|g|],[pc|M|]]  ≟ wgM ⊣ pcList
            ]

absDirAsPCListSetterTests ∷ TestTree
absDirAsPCListSetterTests =
  testGroup "setter"
            [ testCase "etc"   $ etc   ≟ (root  & pcList ⊢ [ [pc|etc|] ])
            , testCase "root"  $ root  ≟ (etc   & pcList ⊢ [ ])
            , testCase "d.pam" $ d_pam ≟ (d_pam & pcList ⊢ [ [pc|pam.d|]
                                                              , [pc|etc|] ])
            , testCase "wgM"   $ wgM   ≟ (⊣ from pcList) [[pc|w|],[pc|g|],[pc|M|]]
            ]

absDirAsPCListTests ∷ TestTree
absDirAsPCListTests =
  testGroup "asPCList"  [ absDirAsPCListGetterTests, absDirAsPCListSetterTests ]

absDirParentMayTests ∷ TestTree
absDirParentMayTests =
  testGroup "parentMay" [ testCase "root"   $ Nothing   ≟ parentMay root
                        , testCase "etc"    $ Just root ≟ parentMay etc
                        , testCase "pam_d"  $ Just etc  ≟ parentMay pam_d
                        ]

absDirParentTests ∷ TestTree
absDirParentTests =
  let par d = parent ⊳ (d ⩼ nonRootAbsDir)
   in testGroup "parent" [ testCase "root"   $ Nothing   ≟ par root
                         , testCase "etc"    $ Just root ≟ par etc
                         , testCase "pam_d"  $ Just etc  ≟ par pam_d
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
            , testCase "pam.d" $ "/etc/pam.d/" ≟ pam_d   ## filepath
            , testCase "wgM"   $ "/w/g/M/"     ≟ wgM     ## filepath
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
                     , absDirAsPCListTests, absDirTextualTests
                     , absDirParentMayTests, absDirParentTests
                     , absDirTextualPrintableTests, absDirFilepathTests
                     ]

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath" [ pathComponentTests, absDirTests ]

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
