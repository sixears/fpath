{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath
  ( tests )
where

-- base --------------------------------

import Control.Monad  ( (>>), return )
import Data.Bool      ( Bool( False ) )
import Data.Either    ( Either( Left, Right  ) )
import Data.Function  ( ($), (&), flip )
import Data.Functor   ( (<$>) )
import Data.Maybe     ( Maybe( Just, Nothing ), fromMaybe )
import Data.String    ( String )
import Data.Typeable  ( Proxy( Proxy ), typeRep )
import System.IO      ( IO )
import Text.Show      ( show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( toText )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Getter  ( (^.) )
import Control.Lens.Setter  ( (.~) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty          ( TestTree, defaultIngredients, testGroup )
import Test.Tasty.Options  ( singleOption )
import Test.Tasty.Runners  ( defaultMainWithIngredients, parseTestPattern
                           , tryIngredients )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- text --------------------------------

import Data.Text  ( Text )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath                ( AbsDir, absdir, nonRootAbsDir, parent, parentMay
                            , parseAbsDir', pcList )
import FPath.Error.FPathError
                            ( FPathError( FPathComponentE, FPathEmptyE
                                        , FPathNonAbsE, FPathNotADirE ) )
import FPath.Error.FPathComponentError
                            ( FPathComponentError( FPathComponentEmptyE
                                                 , FPathComponentIllegalCharE )
                            )
import FPath.PathComponent  ( pc )

--------------------------------------------------------------------------------

root ∷ AbsDir
root = [absdir|/|]

etc ∷ AbsDir
etc = [absdir|/etc/|]

pam_d ∷ AbsDir
pam_d = [absdir|/etc/pam.d/|]

d_pam ∷ AbsDir
d_pam = [absdir|/pam.d/etc/|]

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
                [ testCase "root"  $ Right root   @=? parseAbsDir_ "/"
                , testCase "etc"   $ Right etc    @=? parseAbsDir_ "/etc/"
                , testCase "pam.d" $ Right pam_d  @=? parseAbsDir_ "/etc/pam.d/"
                , testCase "no trailing /" $
                      Left (FPathNotADirE absdirT pamF) @=? parseAbsDir_ pamF
                , testCase "empty" $
                      Left (FPathEmptyE absdirT)  @=? parseAbsDir_ ""
                , testCase "no leading /" $
                      Left (FPathNonAbsE absdirT "etc/") @=? parseAbsDir_ "etc/"
                , testCase "bad component" $
                      Left illegalCE @=? parseAbsDir_ pamNUL
                , testCase "empty component" $
                      Left emptyCompCE @=? parseAbsDir_ "/etc//pam.d/"
                ]

absDirShowTests ∷ TestTree
absDirShowTests =
  let pcl ∷ Text → [Text] → String
      pcl n ps = [fmt|(%t [%L])|] n ((\ p → "[pathComponent|" ⊕ p ⊕ "|]") <$> ps)
      pclad ∷ [Text] → String
      pclad = pcl "absDirFromList"

   in testGroup "show"
                [ testCase "root"  $ pclad []               @=? show root
                , testCase "etc"   $ pclad ["etc"]          @=? show etc
                , testCase "pam.d" $ pclad ["etc", "pam.d"] @=? show pam_d
                ]

absDirPrintableTests ∷ TestTree
absDirPrintableTests =
  testGroup "printable"
            [ testCase "root"  $ "/"           @=? toText root
            , testCase "etc"   $ "/etc/"       @=? toText etc
            , testCase "pam.d" $ "/etc/pam.d/" @=? toText pam_d
            ]

absDirAsPCListGetterTests ∷ TestTree
absDirAsPCListGetterTests =
  testGroup "getter"
            [ testCase "root"  $ []                         @=? root  ^. pcList
            , testCase "etc"   $ [ [pc|etc|] ]              @=? etc   ^. pcList
            , testCase "pam.d" $ [ [pc|etc|], [pc|pam.d|] ] @=? pam_d ^. pcList
            ]

absDirAsPCListSetterTests ∷ TestTree
absDirAsPCListSetterTests =
  testGroup "setter"
            [ testCase "etc"   $ etc   @=? (root  & pcList .~ [ [pc|etc|] ])
            , testCase "root"  $ root  @=? (etc   & pcList .~ [ ])
            , testCase "d.pam" $ d_pam @=? (d_pam & pcList .~ [ [pc|pam.d|]
                                                              , [pc|etc|] ])
            ]

absDirAsPCListTests ∷ TestTree
absDirAsPCListTests =
  testGroup "asPCList"  [ absDirAsPCListGetterTests, absDirAsPCListSetterTests ]

absDirParentMayTests ∷ TestTree
absDirParentMayTests =
  testGroup "parentMay" [ testCase "root"   $ Nothing   @=? parentMay root
                        , testCase "etc"    $ Just root @=? parentMay etc
                        , testCase "pam_d"  $ Just etc  @=? parentMay pam_d
                        ]

absDirParentTests ∷ TestTree
absDirParentTests =
  let par d = parent <$> (d ^? nonRootAbsDir)
   in testGroup "parent" [ testCase "root"   $ Nothing   @=? par root
                         , testCase "etc"    $ Just root @=? par etc
                         , testCase "pam_d"  $ Just etc  @=? par pam_d
                         ]

absDirTests ∷ TestTree
absDirTests =
  testGroup "AbsDir" [ absParseDirTests, absDirShowTests, absDirPrintableTests
                     , absDirAsPCListTests
                     , absDirParentMayTests, absDirParentTests
                     ]

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath" [ absDirTests ]

----------------------------------------

_test ∷ IO ()
_test = defaultMainWithIngredients defaultIngredients tests


_tests ∷ String → IO ()
_tests s = let (<<) = flip (>>)
               tryOpt = tryIngredients defaultIngredients ∘ singleOption
            in return () << case parseTestPattern s of
                             Nothing → return False
                             Just p  → fromMaybe (return False) $ tryOpt p tests

-- that's all, folks! ----------------------------------------------------------
