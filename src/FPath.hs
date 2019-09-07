{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
--{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE PatternSynonyms   #-}
--{-# LANGUAGE QuasiQuotes       #-}
--{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns      #-}

-- {-# LANGUAGE MultiParamTypeClasses #-}

-- TODO

-- sort out naming of Dir, DirOrRel, HasAbsOrRel, etc., etc.
--   maybe FAbs, FRel, etc., for the compound types, maybe _Dir, _Rel for the dirOrRel fns...; also choose in which files to define them, consistently.  Maybe AbsPath -> Abs; RelPath -> Rel; leaving Path for .. things
-- unify DirType & FDirType ?
-- OptParse helpers

-- the only tests that each module should export are 'tests' (with a no-doc flag on them)
-- add toEither for {Abs,Rel}(Path?) {Dir,File}

-- add AsAbs(Path), AsRel(Path); to allow for lenses on FPath; also AsDir & AsRel
-- add toDir, toFile to convert, e.g., "files" returned by readlink to dirs

-- unify implementations to one ("Internals") impl., with newtype encapsulations
--    for each of {Abs,Rel}{Dir,File,Path}; Internals would be something like
--    `Internals Seq|SeqNE FileComponent|None Basis=/|./ AllowPathBits(.|..)`
--    NOTE: ABSDIR IS JUST A GROUNDED RELDIR; that is, AbsDir is just a RelDir,
--    grounded at /

-- try using type-level depth, to allow for non-maybe Parent whenever depth>1.

-- implement Paths

module FPath
  ( AbsDir, AbsFile, AbsPath, Dir, File, NonRootAbsDir, RelDir, RelFile, RelPath
  , AsFilePath( filepath )

  , AbsOrRel(..), HasAbsOrRel( absOrRel, isAbs, isRel )
  , DirOrFile(..), HasDirOrFile( dirOrFile, isDir, isFile )

  , module FPath.AppendableFPath

  -- file functions
  , (⊙), addExt, dir, dirfile, file, ext, splitExt

  -- quasi-quoters
  , absdir, absfile, absdirN, reldir, relfile

  , nonRootAbsDir

  , parseAbsDir  , parseAbsDir'  , __parseAbsDir__  , __parseAbsDir'__
  , parseAbsFile , parseAbsFile' , __parseAbsFile__ , __parseAbsFile'__
  , parseAbsDirN , parseAbsDirN' , __parseAbsDirN__ , __parseAbsDirN'__
  , parseAbsPath , parseAbsPath' , __parseAbsPath__ , __parseAbsPath'__
  , parseDir     , parseDir'     , __parseDir__     , __parseDir'__
  , parseFile    , parseFile'    , __parseFile__    , __parseFile'__
  , parseFPath   , parseFPath'   , __parseFPath__   , __parseFPath'__
  , parseRelDir  , parseRelDir'  , __parseRelDir__  , __parseRelDir'__
  , parseRelFile , parseRelFile' , __parseRelFile__ , __parseRelFile'__
  , seq, seqNE

  , root
  , stripDir, stripDir'

  , getCwd        , getCwd'
    {-
parseDirAbs -- parse a text, if it's relative, make it abs to given cwd

  , resolveDirCwd , resolveDirCwd' , resolveDirCwd_
  , resolveFileCwd, resolveFileCwd', resolveFileCwd_
-}

  , tests
  )
where

-- base --------------------------------

import Control.Monad   ( return )
import Data.Bifunctor  ( first )
import Data.Bool       ( Bool( False, True ) )
import Data.Either     ( Either( Right ), either )
import Data.Eq         ( Eq )
import Data.Function   ( ($), (&), const, id )
import Data.Functor    ( fmap )
import Data.Maybe      ( Maybe( Just, Nothing ), maybe )
import Data.String     ( String )
import Data.Typeable   ( Proxy( Proxy ), TypeRep, typeRep )
import System.IO       ( IO )
import Text.Show       ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- directory ---------------------------

{-

import System.Directory  ( withCurrentDirectory )

import qualified  System.FilePath.Lens  as  FPLens

-}

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( Iso', iso )
import Control.Lens.Prism   ( Prism', prism' )

-- more-unicode-symbols ----------------

import Data.MoreUnicode.Function   ( (⅋) )
import Data.MoreUnicode.Functor    ( (⊳) )
import Data.MoreUnicode.Lens       ( (⊣), (⫣), (⊢), (⊧), (⩼), (##) )
import Data.MoreUnicode.Natural    ( ℕ )
import Data.MoreUnicode.Semigroup  ( (◇) )
import Data.MoreUnicode.Tasty      ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import qualified  NonEmptyContainers.SeqNE           as  SeqNE
import qualified  NonEmptyContainers.SeqConversions  as  SeqConversions

import NonEmptyContainers.SeqConversions   ( FromMonoSeq( fromSeq )
                                           , IsMonoSeq( seq ) )
import NonEmptyContainers.SeqNEConversions ( IsMonoSeqNonEmpty( seqNE )
                                           , fromSeqNE )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- text --------------------------------

import Data.Text  ( head, null )

{-

-- unix --------------------------------

import System.Posix.Directory  ( getWorkingDirectory )

-}

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

{-

import Fluffy.IO.Error       ( AsIOError )
import Fluffy.MonadError     ( fromRight, mapMError, splitMError )
import Fluffy.MonadError.IO  ( asIOError )
import Fluffy.MonadIO        ( MonadIO, eitherIOThrowT, liftIO )
import Fluffy.Path.Error     ( AsPathError, IOPathError, PathError
                             , pathError, pathError' )
import Fluffy.Text           ( last )

-}

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------


import FPath.AbsDir            ( AbsDir, AsAbsDir( _AbsDir)
                               , AsNonRootAbsDir( _NonRootAbsDir ), NonRootAbsDir
                               , ToAbsDir( toAbsDir )

                               , absdir, absdirN, absdirT, nonRootAbsDir
                               , parseAbsDir, parseAbsDir'
                               , __parseAbsDir'__, __parseAbsDir__
                               , parseAbsDirN, parseAbsDirN'
                               , __parseAbsDirN'__, __parseAbsDirN__
                               , root
                               )
import FPath.AbsFile           ( AbsFile, AsAbsFile( _AbsFile )
                               , absfile, absfileT
                               , parseAbsFile, parseAbsFile'
                               , __parseAbsFile'__, __parseAbsFile__
                               )
import FPath.AbsOrRel          ( AbsOrRel( Abs, Rel )
                               , HasAbsOrRel( absOrRel, isAbs, isRel ) )
import FPath.AbsPath           ( AbsPath, parseAbsPath, parseAbsPath'
                               , __parseAbsPath__, __parseAbsPath'__
                               )
import FPath.AppendableFPath   ( AppendableFPath( (⫻) ), (</>) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.DirOrFile         ( DirOrFile( Dir, File )
                               , HasDirOrFile( dirOrFile, isDir, isFile ) )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathError  ( AsFPathError, AsFPathNotAPrefixError
                               , FPathError, FPathNotAPrefixError
                               , __FPathEmptyE__, __FPathNotAPrefixError__
                               )
import FPath.FileLike          ( FileLike( (⊙), addExt, dir, dirfile, file, ext
                                         , splitExt, updateExt ) )
import FPath.FPath             ( parseFPath, parseFPath'
                               , __parseFPath__, __parseFPath'__ )
import FPath.IO                ( getCwd, getCwd' )
import FPath.PathComponent     ( PathComponent, pc, toUpper )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir
                               , parseRelDir, parseRelDir', __parseRelDir'__
                               , __parseRelDir__, reldir, reldirT
                               )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile
                               , parseRelFile, parseRelFile', __parseRelFile'__
                               , __parseRelFile__, relfile, relfileT
                               )
import FPath.RelPath           ( RelPath )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.T.Common          ( doTest, doTestR, doTestS )
import FPath.T.FPath.TestData  ( af1, af2, af3, af4, rf1, rf2, rf3, rf4 )
import FPath.Util              ( __ERROR'__ )

-------------------------------------------------------------------------------

data Dir = DirA AbsDir | DirR RelDir
  deriving (Eq, Show)

instance AsAbsDir Dir where
  _AbsDir ∷ Prism' Dir AbsDir
  _AbsDir = prism' DirA (\ case (DirA d) → Just d; _ → Nothing)

instance AsNonRootAbsDir Dir where
  _NonRootAbsDir ∷ Prism' Dir NonRootAbsDir
  _NonRootAbsDir = prism' (DirA ∘ toAbsDir)
                          (\ case (DirA d) → d ⩼ _NonRootAbsDir; _ → Nothing)

instance AsRelDir Dir where
  _RelDir ∷ Prism' Dir RelDir
  _RelDir = prism' DirR (\ case (DirR d) → Just d; _ → Nothing)

instance HasAbsOrRel Dir where
  absOrRel (DirA _) = Abs
  absOrRel (DirR _) = Rel

------------------------------------------------------------

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

dirT ∷ TypeRep
dirT = typeRep (Proxy ∷ Proxy Dir)

parseDir ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η Dir
parseDir (toText → t) =
  case null t of
    True → __FPathEmptyE__ dirT
    False → case head t of
              '/' → DirA ⊳ parseAbsDir  t
              _   → DirR ⊳ parseRelDir t

parseDir' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η Dir
parseDir' = parseDir

__parseDir__ ∷ Printable τ ⇒ τ → Dir
__parseDir__ = either __ERROR'__ id ∘ parseDir'

__parseDir'__ ∷ String → Dir
__parseDir'__ = __parseDir__

parseDirTests ∷ TestTree
parseDirTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parseDir' t
   in testGroup "parseDir"
                [ success [absdir|/|]     _AbsDir "/"
                , success [absdir|/etc/|] _AbsDir "/etc/"
                , success [reldir|./|]    _RelDir "./"
                , success [reldir|etc/|]  _RelDir "etc/"
                ]

------------------------------------------------------------

fileT ∷ TypeRep
fileT = typeRep (Proxy ∷ Proxy File)

parseFile ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η File
parseFile (toText → t) =
  case null t of
    True → __FPathEmptyE__ fileT
    False → case head t of
              '/' → FileA ⊳ parseAbsFile  t
              _   → FileR ⊳ parseRelFile t

parseFile' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η File
parseFile' = parseFile

__parseFile__ ∷ Printable τ ⇒ τ → File
__parseFile__ = either __ERROR'__ id ∘ parseFile'

__parseFile'__ ∷ String → File
__parseFile'__ = __parseFile__

parseFileTests ∷ TestTree
parseFileTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parseFile' t
   in testGroup "parseFile"
                [ success [absfile|/etc|]  _AbsFile "/etc"
                , success [relfile|etc|]   _RelFile "etc"
                ]

------------------------------------------------------------

------------------------------------------------------------

{- | type-level conversion to absolute version of a type; no-op on
     already-absolute types -}
class HasAbsify α where
  type Absify α

instance HasAbsify RelFile where
  type Absify RelFile = AbsFile

instance HasAbsify RelDir where
  type Absify RelDir = AbsDir

instance HasAbsify AbsFile where
  type Absify AbsFile = AbsFile

instance HasAbsify AbsDir where
  type Absify AbsDir = AbsDir

------------------------------------------------------------

{- | type-level conversion to relative version of a type; no-op on
     already-relative types -}

------------------------------------------------------------

class (DirTypeC π, RelTypeC π) ⇒ Strippable π where
  {- | "unresolve" a path; that is, if an absolute directory is a prefix of an
       absolute (file|directory), then strip off that prefix to leave a relative
       (file|directory).  Note that a file will always keep its filename; but a
       directory might result in a relative dir of './'.
   -}
  stripDir ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                DirType π → π → η (RelType π)
  stripDir' ∷ MonadError FPathNotAPrefixError η ⇒
                 DirType π → π → η (RelType π)
  stripDir' = stripDir

instance Strippable AbsFile where
  stripDir ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                AbsDir → AbsFile → η RelFile
  stripDir d'@(view seq → d) f'@(view seqNE → f) =
    maybe (__FPathNotAPrefixError__ absfileT (toText d') (toText f'))
          (return ∘ fromSeqNE)
          (SeqNE.stripProperPrefix d f)

instance Strippable AbsDir where
  stripDir ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                AbsDir → AbsDir → η RelDir
  stripDir d'@(view seq → d) f'@(view seq → f) =
    maybe (__FPathNotAPrefixError__ absdirT (toText d') (toText f'))
          (return ∘ fromSeq)
          (SeqConversions.stripPrefix d f)

instance Strippable RelFile where
  stripDir ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                RelDir → RelFile → η RelFile
  stripDir d'@(view seq → d) f'@(view seqNE → f) =
    maybe (__FPathNotAPrefixError__ relfileT (toText d') (toText f'))
          (return ∘ fromSeqNE)
          (SeqNE.stripProperPrefix d f)

instance Strippable RelDir where
  stripDir ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                RelDir → RelDir → η RelDir
  stripDir d'@(view seq → d) f'@(view seq → f) =
    maybe (__FPathNotAPrefixError__ reldirT (toText d') (toText f'))
          (return ∘ fromSeq)
          (SeqConversions.stripPrefix d f)

------------------------------------------------------------

-- parseAsAbsDir - parse a path as an AbsDir; if it's not relative, parse it relative to here; if it lacks a trailing '/', add one.
-- fns for Optparse

{-

-- | given a FilePath, which might well include .. and/or ., convert that to
--   an AbsDir, relative to the cwd if it is relative.  Note that this performs
--   IO since the only safe way to do this is to cd to that dir and pwd.  Thus,
--   e.g., permissions may spoil this operation.
resolveDir ∷ (MonadIO μ, AsIOError ε, AsPathError ε, MonadError ε μ) ⇒
               AbsDir → Text → μ AbsDir
resolveDir dir fn =
  let inDir = withCurrentDirectory (toFPath dir)
      inFn  = withCurrentDirectory (toString fn)
   in liftIO (inDir (inFn (splitMError getCwd))) >>= fromRight

resolveDir' ∷ (MonadIO μ, MonadError IOPathError μ) ⇒
               AbsDir → Text → μ AbsDir
resolveDir' = resolveDir

resolveDir_ ∷ MonadIO μ ⇒ AbsDir → Text → μ AbsDir
resolveDir_ dir fn = eitherIOThrowT $ resolveDir' dir fn

-}

----------------------------------------

{-

resolveDirCwd ∷ (MonadIO μ, AsIOError ε, AsPathError ε, MonadError ε μ) ⇒
                 Text → μ AbsDir
resolveDirCwd f = getCwd >>= \d → resolveDir d f

resolveDirCwd' ∷ (MonadIO μ, MonadError IOPathError μ) ⇒ Text → μ AbsDir
resolveDirCwd' = resolveDirCwd

resolveDirCwd_ ∷ MonadIO μ ⇒ Text → μ AbsDir
resolveDirCwd_ = eitherIOThrowT ∘ resolveDirCwd'

-}

----------------------------------------

{-

-- | resolve a FilePath to an AbsFile by checking the filesystem.  See
--  `resolveDir`.
resolveFile ∷ (MonadIO μ, AsIOError ε, AsPathError ε, MonadError ε μ)⇒
               AbsDir → Text → μ AbsFile
resolveFile dir fn = do
  let fn' = toString fn
  dirpath  ← resolveDir dir (toText $ fn' ^. FPLens.directory)
  fileName ← parseRelFile (toText $ fn' ^. FPLens.filename)
  return $ dirpath </> fileName

resolveFile' ∷ (MonadIO μ, MonadError IOPathError μ) ⇒
                AbsDir → Text → μ AbsFile
resolveFile' = resolveFile

resolveFile_ ∷ MonadIO μ ⇒ AbsDir → Text → μ AbsFile
resolveFile_ dir fn = eitherIOThrowT $ resolveFile' dir fn

-}

----------------------------------------

{-

resolveFileCwd ∷ (MonadIO μ, AsIOError ε, AsPathError ε, MonadError ε μ) ⇒
                  Text → μ AbsFile
resolveFileCwd f = getCwd >>= \d → resolveFile d f

resolveFileCwd' ∷ (MonadIO μ, MonadError IOPathError μ) ⇒ Text → μ AbsFile
resolveFileCwd' = resolveFileCwd

resolveFileCwd_ ∷ MonadIO μ ⇒ Text → μ AbsFile
resolveFileCwd_ = eitherIOThrowT ∘ resolveFileCwd'

-}

----------------------------------------

{-

rel2abs ∷ (MonadError ε μ, AsPathError ε) ⇒ AbsDir → Text → μ AbsDir
rel2abs _ t@(pathIsAbsolute → True) = parseAbsDir t
rel2abs d t                          = (Path.</>) d ⊳ (parseRelDir t)

-}
----------------------------------------

{-

_ASSERT ∷ Show χ ⇒ Either χ α → α
_ASSERT (Right r) = r
_ASSERT (Left e)  = error $ show e

_ASSERT' ∷ Maybe α → α
_ASSERT' (Just j) = j
_ASSERT' Nothing  = error "Nothing!"

-}

--------------------

----------------------------------------

{-

parseDir ∷ (AsPathError ε, MonadError ε μ) ⇒
             Text → μ (Either AbsDir RelDir)
parseDir t = if pathIsAbsolute t
              then Left  ⊳ parseAbsDir t
              else Right ⊳ parseRelDir t

parseDir' ∷ MonadError PathError μ ⇒ Text → μ (Either AbsDir RelDir)
parseDir' = parseDir

-}

----------------------------------------

{-

parseFile ∷ (AsPathError ε, MonadError ε μ) ⇒
             Text → μ (Either AbsFile RelFile)
parseFile t = if pathIsAbsolute t
              then Left  ⊳ parseAbsFile t
              else Right ⊳ parseRelFile t

parseFile' ∷ MonadError PathError μ ⇒ Text → μ (Either AbsFile RelFile)
parseFile' = parseFile

-}

----------------------------------------

{-
parseFileAbs ∷ (AsPathError ε, MonadError ε μ) ⇒
                AbsDir → Text → μ AbsFile
parseFileAbs d = fmap (either id (d </>))  ∘ parseFile

parseFileAbs' ∷ MonadError PathError μ ⇒ AbsDir → Text → μ AbsFile
parseFileAbs' = parseFileAbs

-}

----------------------------------------

{-

parseDirAbs ∷ (AsPathError ε, MonadError ε μ) ⇒
                AbsDir → Text → μ AbsDir
parseDirAbs d = fmap (either id (d </>))  ∘ parseDir

parseDirAbs' ∷ MonadError PathError μ ⇒ AbsDir → Text → μ AbsDir
parseDirAbs' = parseDirAbs

-}

----------------------------------------

{-

setFileExtension ∷ (AsPathError ε, MonadError ε μ) ⇒
                    Text → Path β File → μ (Path β File)
setFileExtension e fn =
  mapMError (pathError ([fmt|setFileExtension: (%t)|] e) (toFilePath fn)) $
      Path.setFileExtension (toString e) fn


setFileExtension' ∷ MonadError PathError μ ⇒
                     Text → Path β File → μ (Path β File)
setFileExtension' = setFileExtension

setFileExtension_ ∷ Text → Path β File → Path β File
setFileExtension_ e = _ASSERT ∘ setFileExtension' e

-}

----------------------------------------

{-

-- | The extension of _any_ path - not just files.  Dirs can have extensions
--   too!, e.g., .d directories (though admittedly they are an
--   order-of-magnitude more rare)
ext ∷ MyPath π ⇒ π → Text
--   root (/) is considered to have an extension of ""
ext = maybe "" pack ∘ (Path.fileExtension ⊳) ∘ toFile

extension ∷ MyPath π ⇒ Lens' π Text
extension = lens ext (flip setExt)

-}

----------------------------------------

{-

-- | WARNING this lens is partial, you cannot get the filename of the root
--   directory.  I would love to find a better way to handle this.

-- | > filename ∷ MyPath (Path β τ) ⇒ Lens' (Path β τ) (Path Rel τ)
filename ∷ MyPath π ⇒
            Lens π (Path (AbsOrRel π) τ) (Path Rel (FileType π)) (Path Rel τ)

filename = lens getFilename (flip setFilename)

-}

----------------------------------------

{-

-- | WARNING this lens is partial, you cannot get the parent of the root
--   directory, or a relative file/dir without a parent.  I would love to find a
--   better way to handle this.

parent ∷ (FileType (Path α τ) ~ τ, AbsOrRel (Path α τ) ~ α,
           MyPath (Path α τ)) ⇒
          Lens (Path α τ) (Path β τ) (Path α Dir) (Path β Dir)

parent = lens getParent_ (flip setParent)

-}

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "FPath" [ parseDirTests, parseFileTests
                          , fileLikeTests
                          ]

--------------------

_test ∷ IO ()
_test = doTest tests

_tests ∷ String → IO ()
_tests = doTestS tests

_testr ∷ String → ℕ → IO ()
_testr = doTestR tests


-- that's all, folks! ----------------------------------------------------------
