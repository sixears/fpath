{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath
  ()
where

-- base --------------------------------

import Control.Monad  ( (>>), return )
import Data.Bool      ( Bool( False ) )
import Data.Function  ( ($), (&), flip )
import Data.Functor   ( (<$>) )
import Data.Maybe     ( Maybe( Just, Nothing ), fromMaybe )
import Data.String    ( String )
import System.IO      ( IO )
import Text.Show      ( show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( toText )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.Setter  ( (.~) )

-- tasty -------------------------------

import Test.Tasty          ( TestTree, defaultIngredients, testGroup )
import Test.Tasty.Options  ( singleOption )
import Test.Tasty.Runners  ( defaultMainWithIngredients, parseTestPattern
                           , tryIngredients )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- text --------------------------------

import Data.Text  ( Text, concat, intercalate, unpack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath                ( AbsDir, absdir, pcList )
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

pcl ∷ Text → [Text] → Text
pcl n ps =
  concat [ n, " ["
         , intercalate "," ((\ p → "[pathComponent|" ⊕ p ⊕ "|]") <$> ps), "]" ]

pclad ∷ [Text] → String
pclad = unpack ∘ pcl "absDirFromList"
  
absDirShowTests ∷ TestTree
absDirShowTests =
  testGroup "show"
            [ testCase "root"  $ "absDirFromList []"    @=? show root
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

absDirAsPCListTests ∷ TestTree
absDirAsPCListTests =
  testGroup "asPCList"  [ absDirAsPCListGetterTests, absDirAsPCListSetterTests ]

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

absDirTests ∷ TestTree
absDirTests =
  testGroup "AbsDir" [ absDirShowTests, absDirPrintableTests
                     , absDirAsPCListTests ]

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
