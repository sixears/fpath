{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns        #-}

module FPath.IO
  ( PResolvable( pResolveDir, pResolveDir', pResolve, pResolve' )
  , getCwd, getCwd', inDir, inDirT

  , tests
  )
where

-- base --------------------------------

import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Either             ( Either( Left, Right ) )
import Data.Functor            ( fmap )
import Data.Function           ( ($), const )
import Data.List               ( reverse )
import Data.String             ( String )
import GHC.Exts                ( toList )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import Data.Sequence  ( Seq( Empty ), breakr, fromList )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- directory ---------------------------

import System.Directory  ( withCurrentDirectory )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask, bracket )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣), (##) )
import Data.MoreUnicode.Monad    ( (≫), (⪼) )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.Tasty    ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- temporary ---------------------------

import System.IO.Temp  ( getCanonicalTemporaryDirectory, withSystemTempDirectory
                       )

-- text --------------------------------

import Data.Text  ( Text, last )

-- unix --------------------------------

import System.Posix.Directory  ( changeWorkingDirectory, getWorkingDirectory )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Abs               ( Abs( AbsD, AbsF ), absT )
import FPath.AbsDir            ( AbsDir,  parseAbsDir, __parseAbsDirP__ )
import FPath.AbsFile           ( AbsFile, absfileT )
import FPath.AppendableFPath   ( (⫻) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Basename          ( basename )
import FPath.Dirname           ( dirname )
import FPath.Error.FPathError  ( AsFPathError, FPathIOError
                               , _FPathEmptyE
                               , __FPathEmptyE__, __FPathNotAFileE__
                               )
import FPath.RelFile           ( parseRelFile, relfile )
import FPath.T.Common          ( doTest, doTestR, doTestS )

--------------------------------------------------------------------------------

{- | Current working directory -}
getCwd ∷ (AsIOError ε, AsFPathError ε, MonadError ε μ, MonadIO μ) ⇒ μ AbsDir
getCwd = let addSlash "" = ""
             addSlash t@(last → '/') = t
             addSlash t = t ⊕ "/"
          in asIOError getWorkingDirectory ≫ parseAbsDir ∘ addSlash ∘ toText

getCwd' ∷ (MonadIO μ, MonadError FPathIOError μ) ⇒ μ AbsDir
getCwd' = getCwd

getCwdTests ∷ TestTree
getCwdTests =
  let getCwd_ ∷ IO (Either FPathIOError AbsDir)
      getCwd_ = ѥ getCwd

      inTmp = inSystemTempDirectory "FPath.IO.getCwdTests"
   in testCase "getCwd" $ inTmp $ \ d → getCwd_ ≫ \ cwd → Right d ≟ cwd

----------------------------------------

_inDir ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, Printable τ) ⇒ τ → IO α → μ α
_inDir (toString → d) = asIOError ∘ withCurrentDirectory d

_inDirT ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, Printable τ) ⇒
          τ → ExceptT ε IO α → μ α
_inDirT d io = join $ _inDir d (ѥ io)

{- | Perform IO in the context of a given directory. -}
inDir ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒ AbsDir → IO α → μ α
inDir d = _inDir (d ## filepath)

{- | Perform MonadError-IO in the context of a given directory.  This is for
     when you have IO already bound with errors; e.g.,
     `As*Error ε, MonadError ε μ` on the input IO.
 -}
inDirT ∷ (MonadIO μ,AsIOError ε,MonadError ε μ) ⇒ AbsDir → ExceptT ε IO α → μ α
inDirT d io = join $ inDir d (ѥ io)

------------------------------------------------------------

{- | Things which are physically resolvable -}
class PResolvable α where
  {- | Given a path, which might well be relative and include '..' and/or '.',
       physically resolve that to an α by starting at a given `AbsDir`.
       This involves trying to @chdir@ to the target directory; so will only
       work for directories that you have permission to @chdir@ into (for files,
       you need to be able to @chdir@ into the parent directory).
   -}
  pResolveDir ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 MonadIO μ) ⇒
                AbsDir → τ → μ α

  pResolveDir' ∷ (Printable τ, MonadError FPathIOError μ, MonadIO μ) ⇒
                AbsDir → τ → μ α
  pResolveDir' = pResolveDir

  {- | `pResolveDir`, taking the current working directory as the starting point
   -}
  pResolve ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε  μ,
              MonadIO μ) ⇒
             τ → μ α
  pResolve f = getCwd ≫ \ d → pResolveDir d f

  pResolve' ∷ (Printable τ, MonadError FPathIOError μ, MonadIO μ) ⇒
                τ → μ α
  pResolve' = pResolve

{- | Physically resolve the whole path, thus every directory (including the last
     must) exist.  Treats the lack of a trailing '/' on the input stringlike
     kindly; that is, even without a trailing '/', it is considered as a
     directory.
 -}
instance PResolvable AbsDir where
  pResolveDir ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 MonadIO μ)⇒
                AbsDir → τ → μ AbsDir
  pResolveDir d (toText → f) = inDirT d $ _inDirT f getCwd

pResolveAbsDirTests ∷ TestTree
pResolveAbsDirTests =
  let tName   = "FPath.IO.pResolveTests.AbsDir"
      inTmp   = inSystemTempDirectory tName
      withTmp ∷ (MonadIO μ, MonadMask μ) ⇒ (AbsDir → μ α) → μ α
      withTmp = withSystemTempDirectory tName ∘ (∘ __parseAbsDirP__)

      pResolve_ ∷ Text → IO (Either FPathIOError AbsDir)
      pResolve_ = ѥ ∘ pResolve

      pResolveDir_ ∷ AbsDir → Text → IO (Either FPathIOError AbsDir)
      pResolveDir_ d = ѥ ∘ pResolveDir d

      getTmpdir ∷ IO AbsDir
      getTmpdir = __parseAbsDirP__ ⊳ getCanonicalTemporaryDirectory
   in testGroup "AbsDir"
        [ testCase "inTmp ./" $ inTmp $ \ d → pResolve_ "./" ≫ (Right d ≟)
        , testCase "inTmp . (forgiveness of pResolve wrt trailing /)" $
            inTmp $ \ d → pResolve_ "."  ≫ (Right d ≟)
        , testCase "inTmp .." $
            inTmp ∘ const $
                    getTmpdir ≫ \ tmpdir → pResolve_ ".."  ≫ (Right tmpdir ≟)

        , testCase "inTmp ../ (dirname)" $
            inTmp $ \ d → pResolve_ "../" ≫ ((Right (d ⊣ dirname) ≟))
        , testCase "inTmp ../ (basename)" $
            inTmp $ \ d → pResolve_ "../" ≫ ((Right d ≟) ∘ fmap (⫻ basename d))

        , testCase "withTmp ./" $
            withTmp $ \ d → pResolveDir_ d "./" ≫ (Right d ≟)
        , testCase "withTmp ." $
            withTmp $ \ d → pResolveDir_ d "." ≫ (Right d ≟)
        , testCase "withTmp .." $
            withTmp $ \ d → getTmpdir ≫ \ tmpdir →
                      pResolveDir_ d ".." ≫ (Right tmpdir ≟)

        ]

----------------------------------------

{- | Physically resolve every directory up to and including the dirname of the
     input stringlike; and then tacks the file basename onto the end.  Treats a
     trailing '/' as a dir, and this fails.
 -}
instance PResolvable AbsFile where
  pResolveDir ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 MonadIO μ)⇒
                AbsDir → τ → μ AbsFile
  pResolveDir d (toString → f) =
    -- we can't simply use parseRelFile, etc., here, as we want to accept
    -- paths with '..' and '.' in them (and resolve them)
    case breakr (≡ '/') $ fromList f of
      -- first element of tuple is suffix of seq (a little counterintuitively)
      (Empty, Empty) → -- f was empty
                       __FPathEmptyE__    absfileT
      (Empty, _    ) → -- f had a trailing /
                       __FPathNotAFileE__ absfileT (toText f)


      (_, Empty    ) → -- just a file, no dir part
                       do c ∷ AbsDir ← pResolveDir d ("."∷Text)
                          (c ⫻) ⊳ parseRelFile f

      (x    , y    ) → -- dir + file
                       do c ← pResolveDir d (toList y)
                          (c ⫻) ⊳ parseRelFile (toList x)

pResolveAbsFileTests ∷ TestTree
pResolveAbsFileTests =
  let tName   = "FPath.IO.pResolveTests.AbsFile"
      inTmp   = inSystemTempDirectory tName
      withTmp ∷ (MonadIO μ, MonadMask μ) ⇒ (AbsDir → μ α) → μ α
      withTmp = withSystemTempDirectory tName ∘ (∘ __parseAbsDirP__)

      pResolve_ ∷ Text → IO (Either FPathIOError AbsFile)
      pResolve_ = ѥ ∘ pResolve

      pResolveDir_ ∷ AbsDir → Text → IO (Either FPathIOError AbsFile)
      pResolveDir_ d = ѥ ∘ pResolveDir d

   in testGroup "AbsFile"
        [ testCase "inTmp '' x" $
            inTmp $ \ d → pResolve_ "x" ≫ (Right (d ⫻ [relfile|x|] ∷ AbsFile) ≟)
        , testCase "withTmp '' x" $
            withTmp $ \ d → pResolveDir_ d "x" ≫
                          (Right (d ⫻ [relfile|x|] ∷ AbsFile) ≟)
        , testCase "inTmp ./ x" $
            inTmp $ \ d → pResolve_ "./x" ≫
                          (Right (d ⫻ [relfile|x|] ∷ AbsFile) ≟)
        , testCase "withTmp ./ x" $
            withTmp $ \ d → pResolveDir_ d "./x" ≫
                          (Right (d ⫻ [relfile|x|] ∷ AbsFile) ≟)
        , testCase "inTmp ../ x" $
            inTmp $ \ d → pResolve_ "../x" ≫
                          (Right (d ⊣ dirname ⫻ [relfile|x|] ∷ AbsFile) ≟)
        , testCase "withTmp ../ x" $
            withTmp $ \ d → pResolveDir_ d "../x" ≫
                          (Right (d ⊣ dirname ⫻ [relfile|x|] ∷ AbsFile) ≟)
        ]


{- | Given a path, which might well relative include '..' and/or '.', physically
     resolve that to an Abs.  Relative paths are contextual to the cwd.
     Input with a trailing '/', "/.", or "/.."; or the special cases "." and
     ".." are resolved to directories; without are resolved to files.  Empty
     input strings cause a failure.
 -}
instance PResolvable Abs where
  pResolveDir ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 MonadIO μ)⇒
                AbsDir → τ → μ Abs
  pResolveDir _ (toString → [])                       = __FPathEmptyE__ absT
  pResolveDir d t@(toString → ".")                    = AbsD ⊳ pResolveDir d t
  pResolveDir d t@(toString → "..")                   = AbsD ⊳ pResolveDir d t
  pResolveDir d t@(reverse ∘ toString → '/' : _)      = AbsD ⊳ pResolveDir d t
  pResolveDir d t@(reverse ∘ toString → '.' : '/' : _)= AbsD ⊳ pResolveDir d t
  pResolveDir d t@(reverse ∘ toString → '.':'.':'/':_)= AbsD ⊳ pResolveDir d t
  pResolveDir d t                                     = AbsF ⊳ pResolveDir d t


pResolveAbsTests ∷ TestTree
pResolveAbsTests =
  let tName   = "FPath.IO.pResolveTests.Abs"
      withTmp ∷ (MonadIO μ, MonadMask μ) ⇒ (AbsDir → μ α) → μ α
      withTmp = withSystemTempDirectory tName ∘ (∘ __parseAbsDirP__)

      pResolveDir_ ∷ AbsDir → Text → IO (Either FPathIOError Abs)
      pResolveDir_ d = ѥ ∘ pResolveDir d

   in testGroup "Abs"
        [ testCase "withTmp ''" $
            withTmp $ \ d → pResolveDir_ d "" ≫ (Left (_FPathEmptyE absT) ≟)
        , testCase "withTmp ./" $
            withTmp $ \ d → pResolveDir_ d "./" ≫ (Right (AbsD d) ≟)
        , testCase "withTmp ." $
            withTmp $ \ d → pResolveDir_ d "." ≫ (Right (AbsD d) ≟)
        , testCase "withTmp .." $
            withTmp $ \ d → pResolveDir_ d ".." ≫ (Right (AbsD (d ⊣ dirname)) ≟)
        , testCase "withTmp ../" $
            withTmp $ \ d → pResolveDir_ d "../" ≫(Right (AbsD (d ⊣ dirname)) ≟)
        , testCase "withTmp ../." $
            withTmp $ \ d → pResolveDir_ d "../."≫(Right (AbsD (d ⊣ dirname)) ≟)
        , testCase "withTmp ./../." $
            withTmp $ \ d →
                      pResolveDir_ d "./../."≫(Right (AbsD (d ⊣ dirname)) ≟)
        , testCase "withTmp .././." $
            withTmp $ \ d →
                      pResolveDir_ d ".././."≫(Right (AbsD (d ⊣ dirname)) ≟)

        , testCase "withTmp ''" $
            withTmp $ \ d → pResolveDir_ d "" ≫ (Left (_FPathEmptyE absT) ≟)
        , testCase "withTmp '' x" $
            withTmp $ \ d → pResolveDir_ d "x" ≫
                          (Right (AbsF (d ⫻ [relfile|x|])) ≟)
        , testCase "withTmp ./ x" $
            withTmp $ \ d → pResolveDir_ d "./x" ≫
                          (Right (AbsF (d ⫻ [relfile|x|])) ≟)
        , testCase "withTmp ../ x" $
            withTmp $ \ d → pResolveDir_ d "../x" ≫
                          (Right (AbsF (d ⊣ dirname ⫻ [relfile|x|])) ≟)
        ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

inSystemTempDirectory ∷ String → (AbsDir → IO α) → IO α
inSystemTempDirectory t io =
  withSystemTempDirectory t $ \ d →
    bracket (getWorkingDirectory ≫  \ o → changeWorkingDirectory d ⪼ return o)
            changeWorkingDirectory
            (\ _ → io $ __parseAbsDirP__ d)

pResolveTests ∷ TestTree
pResolveTests = testGroup "pResolve" [ pResolveAbsDirTests, pResolveAbsFileTests
                                     , pResolveAbsTests ]

tests ∷ TestTree
tests = testGroup "FPath.IO" [ getCwdTests, pResolveTests ]

_test ∷ IO ()
_test = doTest tests

--------------------

_tests ∷ String → IO ()
_tests = doTestS tests

_testr ∷ String → ℕ → IO ()
_testr = doTestR tests

-- that's all, folks! ----------------------------------------------------------
