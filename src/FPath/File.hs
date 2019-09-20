{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE ViewPatterns     #-}

module FPath.File
  ( File(..)

  , tests
  )
where

-- base --------------------------------

import Data.Bifunctor  ( first )
import Data.Bool       ( Bool( False, True ) )
import Data.Either     ( Either( Right ) )
import Data.Eq         ( Eq )
import Data.Function   ( ($), (&), const )
import Data.Maybe      ( Maybe( Just, Nothing ) )
import Data.String     ( String )
import Data.Typeable   ( Proxy( Proxy ), TypeRep, typeRep )
import System.Exit     ( ExitCode )
import System.IO       ( IO )
import Text.Show       ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- lens --------------------------------

import Control.Lens.Iso     ( Iso', iso )
import Control.Lens.Prism   ( Prism', prism' )

-- more-unicode-symbols ----------------

import Data.MoreUnicode.Functor    ( (⊳) )
import Data.MoreUnicode.Lens       ( (⊣), (⫣), (⊢), (##) )
import Data.MoreUnicode.Natural    ( ℕ )
import Data.MoreUnicode.Semigroup  ( (◇) )
import Data.MoreUnicode.Tasty      ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( head, null )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir            ( absdir )
import FPath.AbsFile           ( AbsFile, AsAbsFile( _AbsFile ), absfile )
import FPath.Dir               ( Dir( DirA, DirR ) )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.FileLike          ( FileLike( (⊙), addExt, dir, dirfile, file, ext
                                         , splitExt, updateExt ) )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( PathComponent, pc, toUpper )
import FPath.RelDir            ( reldir )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile, relfile )
import FPath.T.FPath.TestData  ( af1, af2, af3, af4, rf1, rf2, rf3, rf4 )

-------------------------------------------------------------------------------

data File = FileA AbsFile | FileR RelFile
  deriving (Eq, Show)

instance AsAbsFile File where
  _AbsFile ∷ Prism' File AbsFile
  _AbsFile = prism' FileA (\ case (FileA d) → Just d; _ → Nothing)

instance AsRelFile File where
  _RelFile ∷ Prism' File RelFile
  _RelFile = prism' FileR (\ case (FileR d) → Just d; _ → Nothing)

instance DirTypeC File where
  type DirType File = Dir

instance FileLike File where

  dirfile ∷ Iso' File (Dir, PathComponent)
  dirfile = iso (\ case FileA a → first DirA (a ⊣ dirfile)
                        FileR r → first DirR (r ⊣ dirfile))
                (\ case (DirA a, c) → FileA ((a,c) ⫣ dirfile)
                        (DirR r, c) → FileR ((r,c) ⫣ dirfile))

fileLikeFileTests ∷ TestTree
fileLikeFileTests =
  let (~~) ∷ File → PathComponent → File
      f ~~ d' = f & file ⊢ d'
   in testGroup "file"
                [ testCase "rf3"       $ [pc|r.mp3|] ≟ FileR rf3 ⊣ file
                , testCase "rf4"       $ [pc|.x|]    ≟ FileR rf4 ⊣ file
                , testCase "af3"       $ [pc|r.mp3|] ≟ FileA af3 ⊣ file
                , testCase "af4"       $ [pc|.x|]    ≟ FileA af4 ⊣ file

                , testCase "af3 → a0"  $
                    FileA [absfile|/p/q/foo|] ≟ FileA af3 ~~ [pc|foo|]
                , testCase "af2 → a1"  $
                    FileA af2 ≟ FileA af2 ~~ [pc|p.x|]
                , testCase "rf1 → a2"  $
                    FileR [relfile|.z|] ≟ FileR rf1 ~~ [pc|.z|]
                ]

fileLikeDirTests ∷ TestTree
fileLikeDirTests =
  let (~~) ∷ File → Dir → File
      f ~~ d' = f & dir ⊢ d'
   in testGroup "dir"
                [ testCase "rf3"      $ DirR [reldir|p/q/|]     ≟ FileR rf3 ⊣ dir
                , testCase "rf4"      $ DirR [reldir|./|]   ≟ FileR rf4 ⊣ dir
                , testCase "af3"      $ DirA [absdir|/p/q/|] ≟ FileA af3 ⊣ dir
                , testCase "af4"      $ DirA [absdir|/|]     ≟ FileA af4 ⊣ dir

                , testCase "rf3 → a0" $
                    FileR [relfile|s/r.mp3|] ≟ FileA af3 ~~ DirR [reldir|s/|]
                , testCase "af2 → a1" $
                    FileR rf2                ≟ FileR rf2 ~~ DirR [reldir|r/|]
                , testCase "af1 → a2" $
                    FileA [absfile|/p.x|]    ≟ FileA af2 ~~ DirA [absdir|/|]
                , testCase "af1 → a2" $
                    FileA [absfile|/q/p/.x|] ≟ FileR rf4 ~~ DirA [absdir|/q/p/|]
                ]

fileLikeAddExtTests ∷ TestTree
fileLikeAddExtTests =
  testGroup "addExt"
    [ testCase "foo.bar" $
        FileA [absfile|/foo.bar|]     ≟ addExt (FileA [absfile|/foo|]) [pc|bar|]
    , testCase "r.e.bar" $
        FileA [absfile|/r.e.bar|]     ≟ FileA af1 ⊙ [pc|bar|]
    , testCase "f.o.b.r" $
        FileR [relfile|p/q/r.mp3.b.r|]≟ FileR rf3 ⊙ [pc|b.r|]
    ]

fileLikeSplitExtTests ∷ TestTree
fileLikeSplitExtTests =
  testGroup "splitExt"
    [ testCase "foo/bar" $
        (FileR [relfile|foo/bar|], Nothing) ≟ splitExt (FileR [relfile|foo/bar|])
    , testCase "r/p.x"   $
        (FileR [relfile|r/p|],Just [pc|x|]) ≟ splitExt (FileR rf2)
    , testCase "f.x/g.y" $
          (FileA [absfile|/f.x/g|], Just [pc|y|])
        ≟ splitExt (FileA [absfile|/f.x/g.y|])
    , testCase "f.x/g"   $
        (FileA [absfile|/f.x/g|], Nothing) ≟ splitExt (FileA [absfile|/f.x/g|])
    ]

fileLikeExtGetterTests ∷ TestTree
fileLikeExtGetterTests =
  testGroup "getter" [ testCase "foo.z/bar.x" $
                         Just [pc|x|] ≟ ext(FileR [relfile|foo.z/bar.x|])
                     , testCase "foo/bar" $
                         Nothing ≟ ext (FileA [absfile|/foo/bar|])
                     , testCase "g/f.b.x.baz"  $
                         Just [pc|baz|] ≟ ext (FileA [absfile|/g/f.b.x.baz|])
                     ]

fileLikeExtSetterTests ∷ TestTree
fileLikeExtSetterTests =
  testGroup "setter"
    [ testCase "foo.bar -> foo.baz" $
          FileR [relfile|p/foo.baz|]
        ≟ FileR (updateExt (const [pc|baz|]) [relfile|p/foo.bar|])
    , testCase "/foo.x/bar -> /foo.x/bar" $
          FileA [absfile|/foo.x/bar|]
        ≟ FileA (updateExt (const [pc|baz|]) [absfile|/foo.x/bar|])
    , testCase "foo -> foo" $
        FileA [absfile|/foo|] ≟ FileA (updateExt (const [pc|baz|]) [absfile|/foo|])
    , testCase "g/foo. -> g/foo." $
          FileA [absfile|/g/foo.|]
        ≟ FileA (updateExt (const [pc|baz|]) [absfile|/g/foo.|])
    ]

fileLikeExtAdjusterTests ∷ TestTree
fileLikeExtAdjusterTests =
  testGroup "adjuster"
    [ testCase ".baz -> .BAR" $
        FileR [relfile|g/fo.BA|] ≟ FileR (updateExt toUpper [relfile|g/fo.ba|])
    , testCase ".x.b -> .x.B" $
        FileR [relfile|f.x.B|]   ≟ FileR (updateExt toUpper [relfile|f.x.b|])
    , testCase ".x -> .xy"    $
        FileA [absfile|/f.xy|] ≟ FileA (updateExt (◇ [pc|y|]) [absfile|/f.x|])
    , testCase ".    -> ."    $
        FileA [absfile|/fo.|]  ≟ FileA (updateExt (◇ [pc|y|]) [absfile|/fo.|])
    ]

fileLikeTests ∷ TestTree
fileLikeTests =
  testGroup "FileLike" [ fileLikeFileTests, fileLikeDirTests
                       , fileLikeAddExtTests, fileLikeSplitExtTests
                       , fileLikeExtGetterTests, fileLikeExtSetterTests
                       , fileLikeExtAdjusterTests
                       ]

------------------------------------------------------------

fileT ∷ TypeRep
fileT = typeRep (Proxy ∷ Proxy File)

instance Parseable File where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η File
  parse (toText → t) =
    case null t of
      True → __FPathEmptyE__ fileT
      False → case head t of
                '/' → FileA ⊳ parse t
                _   → FileR ⊳ parse t

parseFileTests ∷ TestTree
parseFileTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parse @File @FPathError t
   in testGroup "parseFile"
                [ success [absfile|/etc|]  _AbsFile "/etc"
                , success [relfile|etc|]   _RelFile "etc"
                ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "FPath.File" [ parseFileTests, fileLikeTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
