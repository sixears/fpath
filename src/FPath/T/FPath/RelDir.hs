{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath.RelDir
  ( tests )
where

-- base --------------------------------

import Data.Either      ( Either( Left, Right  ) )
import Data.Function    ( ($), (&) )
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

import Data.Textual  ( toString, toText )

-- fluffy ------------------------------

import NonEmptyContainers.SeqNE  ( (⪬) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens    ( (⊣), (⊥), (⊢) )
import Data.MoreUnicode.Monoid  ( ф )
import Data.MoreUnicode.Tasty   ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath                   ( RelDir, parseRelDir', seq, reldir )
import FPath.Error.FPathError  ( FPathError( FPathAbsE, FPathComponentE
                                           , FPathNotADirE )
                               )
import FPath.Error.FPathComponentError
                               ( FPathComponentError( FPathComponentEmptyE
                                                    , FPathComponentIllegalCharE
                                                    )
                               )
import FPath.PathComponent     ( pc )

import FPath.T.Common          ( doTest, doTestR, doTestS )
import FPath.T.FPath.TestData  ( r0, r1, r2, r3 )

--------------------------------------------------------------------------------

relDirParseRelDirTests ∷ TestTree
relDirParseRelDirTests =
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

relDirQuasiQuotesTests ∷ TestTree
relDirQuasiQuotesTests =
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

relDirIsMonoSeqTests ∷ TestTree
relDirIsMonoSeqTests = testGroup "IsMonoSeq" [ relDirIsMonoSeqGetterTests
                                             , relDirIsMonoSeqSetterTests ]
relDirConstructionTests ∷ TestTree
relDirConstructionTests = testGroup "construction" [ relDirParseRelDirTests
                                                   , relDirQuasiQuotesTests ]
tests ∷ TestTree
tests =
  testGroup "RelDir" [ relDirConstructionTests
                     , relDirIsListTests
                     , relDirIsMonoSeqTests
                     , relDirShowTests, relDirPrintableTests
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
