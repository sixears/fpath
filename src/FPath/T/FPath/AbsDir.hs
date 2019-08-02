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
import Data.Either          ( Either( Left, Right  ) )
import Data.Function        ( ($), (&) )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), typeRep )
import GHC.Exts             ( fromList, toList )
import Numeric.Natural      ( Natural )
import System.IO            ( IO )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

import Data.Sequence  ( Seq( Empty ) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), fromString, parseString, toText )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( from )
import Control.Lens.Setter  ( (?~) )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor         ( (⊳) )
import Data.MoreUnicode.Lens            ( (⊣), (⊢), (⩼), (##) )
import Data.MoreUnicode.Tasty           ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty           ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath                   ( AbsDir, absdir, filepath, nonRootAbsDir, parent
                               , parentMay, parseAbsDir', seq )
import FPath.Error.FPathError  ( FPathError( FPathComponentE, FPathEmptyE
                                           , FPathNonAbsE , FPathNotADirE )
                               )
import FPath.Error.FPathComponentError
                               ( FPathComponentError( FPathComponentEmptyE
                                                    , FPathComponentIllegalCharE
                                                    )
                               )
import FPath.PathComponent     ( pc )

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

parseAbsDirTests ∷ TestTree
parseAbsDirTests =
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
  let rootShow = "AbsRootDir"
      etcShow  = "AbsNonRootDir (NonRootAbsDir (PathComponent \"etc\") "
               ⊕ "AbsRootDir)"
      pamdShow = "AbsNonRootDir (NonRootAbsDir (PathComponent \"pam.d\") "
               ⊕ "(AbsNonRootDir (NonRootAbsDir (PathComponent \"etc\") "
               ⊕ "AbsRootDir)))"

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
                      wgm   ≟ (⊣ from seq)
                                   (Seq.fromList [[pc|w|],[pc|g|],[pc|M|]])
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
absDirConstructionTests = testGroup "construction" [ parseAbsDirTests
                                                   , absdirQQTests ]

absDirTextualGroupTests ∷ TestTree
absDirTextualGroupTests =
  testGroup "textual group" [ absDirTextualTests, absDirTextualPrintableTests
                            , absDirPrintableTests ]

absDirParentGroupTests ∷ TestTree
absDirParentGroupTests =
  testGroup "parent group" [ absDirParentTests, absDirParentMayTests ]


tests ∷ TestTree
tests =
  testGroup "AbsDir" [ absDirConstructionTests, absDirShowTests
                     , absDirTextualGroupTests
                     , absDirIsListTests
                     , absDirIsMonoSeqTests
                     , absDirParentGroupTests
                     , absDirFilepathTests
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
