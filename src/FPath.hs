{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
--{-# LANGUAGE QuasiQuotes       #-}
--{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns      #-}

{-# LANGUAGE MultiParamTypeClasses #-}

-- unify implementations to one ("Internals") impl., with newtype encapsulations
--    for each of {Abs,Rel}{Dir,File,Path}; Internals would be something like
--    `Internals Seq|SeqNE FileComponent|None Basis=/|./ AllowPathBits(.|..)`

-- try using type-level depth, to allow for non-maybe Parent whenever depth>1.

module FPath
  ( AbsDir, AbsFile, AbsPath, Dir, File, NonRootAbsDir, RelDir, RelFile, RelPath
  , AsFilePath( filepath )

  , (⫻), (</>)

  -- quasi-quoters
  , absdir, absfile, absdirN, reldir, relfile

  -- List/NonEmpty/Seq conversion fns
  , toSeq

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
    {-
  , MyPath( AbsOrRel, FileType, toFile, toFile_
          , getFilename, getParent, setFilename )

  , pathIsAbsolute
  , rel2abs

  , getCwd        , getCwd'        , getCwd_
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
import Data.Function   ( ($), (&), id )
import Data.Functor    ( fmap )
import Data.Maybe      ( Maybe( Just, Nothing ), maybe )
import Data.String     ( String )
import Data.Typeable   ( Proxy( Proxy ), TypeRep, typeRep )
import Text.Show       ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

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
import Control.Lens.Review  ( re )

-- more-unicode-symbols ----------------

import Data.MoreUnicode.Function   ( (⅋) )
import Data.MoreUnicode.Functor    ( (⊳) )
import Data.MoreUnicode.Lens       ( (⊣), (⫣), (⊢), (⊧), (⩼), (##) )
import Data.MoreUnicode.Semigroup  ( (◇) )
import Data.MoreUnicode.Tasty      ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import qualified  NonEmptyContainers.SeqNE           as  SeqNE
import qualified  NonEmptyContainers.SeqConversions  as  SeqConversions

import NonEmptyContainers.SeqConversions
                                 ( FromMonoSeq( fromSeq ), IsMonoSeq( seq )
                                 , ToMonoSeq( toSeq ) )
import NonEmptyContainers.SeqNE  ( (⪡) )
import NonEmptyContainers.SeqNEConversions
                                 ( IsMonoSeqNonEmpty( seqNE ), fromSeqNE )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- text --------------------------------

import Data.Text  ( head, last, null )

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
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.DirType           ( HasDirType( DirType ) )
import FPath.Error.FPathError  ( AsFPathError, AsFPathNotAPrefixError
                               , FPathError, FPathNotAPrefixError
                               , __FPathEmptyE__, __FPathNotAPrefixError__
                               )
import FPath.Fileish           ( Fileish( FDirType, (⊙), addExt
                                        , dir, dirfile, file, ext, splitExt ) )
import FPath.PathComponent     ( PathComponent, pc, toUpper )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir
                               , parseRelDir, parseRelDir', __parseRelDir'__
                               , __parseRelDir__, reldir, reldirT
                               )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile
                               , parseRelFile, parseRelFile', __parseRelFile'__
                               , __parseRelFile__, relfile, relfileT
                               )
import FPath.RelType           ( HasRelType( RelType ) )
import FPath.T.FPath.TestData  ( af1, af2, af3, af4, rf1, rf2, rf3, rf4 )
import FPath.Util              ( __ERROR'__ )

-------------------------------------------------------------------------------

data AbsPath = AbsD AbsDir | AbsF AbsFile
  deriving (Eq, Show)

instance AsAbsDir AbsPath where
  _AbsDir ∷ Prism' AbsPath AbsDir
  _AbsDir = prism' AbsD (\ case (AbsD d) → Just d; _ → Nothing)

instance AsNonRootAbsDir AbsPath where
  _NonRootAbsDir ∷ Prism' AbsPath NonRootAbsDir
  _NonRootAbsDir = prism' (AbsD ∘ toAbsDir)
                          (\ case (AbsD d) → d ⩼ _NonRootAbsDir; _ → Nothing)

instance AsAbsFile AbsPath where
  _AbsFile ∷ Prism' AbsPath AbsFile
  _AbsFile = prism' AbsF (\ case (AbsF f) → Just f; _ → Nothing)

------------------------------------------------------------

data RelPath = RelD RelDir | RelF RelFile
  deriving (Eq, Show)

instance AsRelDir RelPath where
  _RelDir ∷ Prism' RelPath RelDir
  _RelDir = prism' RelD (\ case (RelD d) → Just d; _ → Nothing)

instance AsRelFile RelPath where
  _RelFile ∷ Prism' RelPath RelFile
  _RelFile = prism' RelF (\ case (RelF f) → Just f; _ → Nothing)

------------------------------------------------------------

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

------------------------------------------------------------

data File = FileA AbsFile | FileR RelFile
  deriving (Eq, Show)

instance AsAbsFile File where
  _AbsFile ∷ Prism' File AbsFile
  _AbsFile = prism' FileA (\ case (FileA d) → Just d; _ → Nothing)

instance AsRelFile File where
  _RelFile ∷ Prism' File RelFile
  _RelFile = prism' FileR (\ case (FileR d) → Just d; _ → Nothing)

instance Fileish File where
  type FDirType File = Dir

  dirfile ∷ Iso' File (Dir, PathComponent)
  dirfile = iso (\ case FileA a → first DirA (a ⊣ dirfile)
                        FileR r → first DirR (r ⊣ dirfile))
                (\ case (DirA a, c) → FileA ((a,c) ⫣ dirfile)
                        (DirR r, c) → FileR ((r,c) ⫣ dirfile))

fileishFileTests ∷ TestTree
fileishFileTests =
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

fileishDirTests ∷ TestTree
fileishDirTests =
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

fileishAddExtTests ∷ TestTree
fileishAddExtTests =
  testGroup "addExt"
    [ testCase "foo.bar" $
        FileA [absfile|/foo.bar|]     ≟ addExt (FileA [absfile|/foo|]) [pc|bar|]
    , testCase "r.e.bar" $
        FileA [absfile|/r.e.bar|]     ≟ FileA af1 ⊙ [pc|bar|]
    , testCase "f.o.b.r" $
        FileR [relfile|p/q/r.mp3.b.r|]≟ FileR rf3 ⊙ [pc|b.r|]
    ]

fileishSplitExtTests ∷ TestTree
fileishSplitExtTests =
  testGroup "splitExt"
    [ testCase "foo/bar" $
        Nothing ≟ splitExt (FileR [relfile|foo/bar|])
    , testCase "r/p.x"   $
        Just (FileR [relfile|r/p|],[pc|x|]) ≟ splitExt (FileR rf2)
    , testCase "f.x/g.y" $
          Just (FileA [absfile|/f.x/g|], [pc|y|])
        ≟ splitExt (FileA [absfile|/f.x/g.y|])
    , testCase "f.x/g"   $
        Nothing ≟ splitExt (FileA [absfile|/f.x/g|])
    ]

fileishExtGetterTests ∷ TestTree
fileishExtGetterTests =
  testGroup "getter" [ testCase "foo.z/bar.x" $
                         Just [pc|x|] ≟ FileR [relfile|foo.z/bar.x|]   ⊣ ext
                     , testCase "foo/bar" $
                         Nothing ≟ FileA [absfile|/foo/bar|]   ⊣ ext
                     , testCase "g/f.b.x.baz"  $
                         Just [pc|baz|] ≟ FileA [absfile|/g/f.b.x.baz|] ⊣ ext
                     ]

fileishExtSetterTests ∷ TestTree
fileishExtSetterTests =
  testGroup "setter"
    [ testCase "foo.bar -> foo.baz" $
          FileR [relfile|p/foo.baz|]
        ≟ FileR [relfile|p/foo.bar|] ⅋ ext ⊢ Just [pc|baz|]
    , testCase "p/foo.x -> ''"   $
        FileR [relfile|p/foo|]     ≟ FileR [relfile|p/foo.x|] ⅋ ext ⊢ Nothing
    , testCase "foo/bar.bar -> foo.x/bar.baz" $
          FileA [absfile|/foo.x/bar.baz|]
        ≟ FileA [absfile|/foo.x/bar|] ⅋ ext ⊢ Just [pc|baz|]
    , testCase "foo -> foo.baz" $
        FileA [absfile|/foo.baz|] ≟ FileA [absfile|/foo|] ⅋ ext ⊢ Just [pc|baz|]
    , testCase "g/foo. -> g/foo..baz" $
          FileA [absfile|/g/foo..baz|]
        ≟ FileA [absfile|/g/foo.|] ⅋ ext ⊢ Just [pc|baz|]
    ]

fileishExtAdjusterTests ∷ TestTree
fileishExtAdjusterTests =
  testGroup "adjuster"
    [ testCase ".baz -> .BAR" $
        FileR [relfile|g/fo.BA|] ≟ FileR [relfile|g/fo.ba|] ⅋ ext ⊧ fmap toUpper
    , testCase ".x.b -> .x.B" $
        FileR [relfile|f.x.B|]   ≟ FileR [relfile|f.x.b|]   ⅋ ext ⊧ fmap toUpper
    , testCase ".x -> .xy"    $
        FileA [absfile|/f.xy|] ≟ FileA [absfile|/f.x|] ⅋ ext ⊧ fmap (◇ [pc|y|])
    , testCase ".    -> ."    $
        FileA [absfile|/fo.|]  ≟ FileA [absfile|/fo.|] ⅋ ext ⊧ fmap (◇ [pc|y|])
    ]

fileishTests ∷ TestTree
fileishTests =
  testGroup "Fileish" [ fileishFileTests, fileishDirTests
                      , fileishAddExtTests, fileishSplitExtTests
                      , fileishExtGetterTests, fileishExtSetterTests
                      , fileishExtAdjusterTests
                      ]

------------------------------------------------------------

abspathT ∷ TypeRep
abspathT = typeRep (Proxy ∷ Proxy AbsPath)

parseAbsPath ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η AbsPath
parseAbsPath (toText → t) =
  case null t of
    True → __FPathEmptyE__ abspathT
    False → case last t of
              '/' → AbsD ⊳ parseAbsDir  t
              _   → AbsF ⊳ parseAbsFile t

parseAbsPath' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η AbsPath
parseAbsPath' = parseAbsPath

__parseAbsPath__ ∷ Printable τ ⇒ τ → AbsPath
__parseAbsPath__ = either __ERROR'__ id ∘ parseAbsPath'

__parseAbsPath'__ ∷ String → AbsPath
__parseAbsPath'__ = __parseAbsPath__

----------------------------------------

parseAbsPathTests ∷ TestTree
parseAbsPathTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parseAbsPath' t
   in testGroup "parseAbsPath"
                [ success [absdir|/|]           _AbsDir "/"
                , success [absdir|/etc/|]       _AbsDir "/etc/"
                , success [absfile|/etc/group|] _AbsFile "/etc/group"
                ]

----------------------------------------

relpathT ∷ TypeRep
relpathT = typeRep (Proxy ∷ Proxy RelPath)

parseRelPath ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η RelPath
parseRelPath (toText → t) =
  case null t of
    True → __FPathEmptyE__ relpathT
    False → case last t of
              '/' → RelD ⊳ parseRelDir  t
              _   → RelF ⊳ parseRelFile t

parseRelPath' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η RelPath
parseRelPath' = parseRelPath

__parseRelPath__ ∷ Printable τ ⇒ τ → RelPath
__parseRelPath__ = either __ERROR'__ id ∘ parseRelPath'

__parseRelPath'__ ∷ String → RelPath
__parseRelPath'__ = __parseRelPath__

parseRelPathTests ∷ TestTree
parseRelPathTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parseRelPath' t
   in testGroup "parseRelPath"
                [ success [reldir|./|]         _RelDir "./"
                , success [reldir|etc/|]       _RelDir "etc/"
                , success [relfile|etc/group|] _RelFile "etc/group"
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

data FPath = FAbsD AbsDir | FAbsF AbsFile | FRelD RelDir | FRelF RelFile
  deriving (Eq, Show)

instance AsAbsDir FPath where
  _AbsDir ∷ Prism' FPath AbsDir
  _AbsDir = prism' FAbsD (\ case (FAbsD d) → Just d; _ → Nothing)

instance AsAbsFile FPath where
  _AbsFile ∷ Prism' FPath AbsFile
  _AbsFile = prism' FAbsF (\ case (FAbsF f) → Just f; _ → Nothing)

instance AsNonRootAbsDir FPath where
  _NonRootAbsDir ∷ Prism' FPath NonRootAbsDir
  _NonRootAbsDir = prism' (FAbsD ∘ toAbsDir)
                          (\ case (FAbsD d) → d ⩼ _NonRootAbsDir; _ → Nothing)

instance AsRelDir FPath where
  _RelDir ∷ Prism' FPath RelDir
  _RelDir = prism' FRelD (\ case (FRelD d) → Just d; _ → Nothing)

instance AsRelFile FPath where
  _RelFile ∷ Prism' FPath RelFile
  _RelFile = prism' FRelF (\ case (FRelF f) → Just f; _ → Nothing)

------------------------------------------------------------

fpathT ∷ TypeRep
fpathT = typeRep (Proxy ∷ Proxy FPath)

parseFPath ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η FPath
parseFPath (toText → t) =
  case null t of
    True → __FPathEmptyE__ fpathT
    False → case (head t, last t) of
              ('/','/') → FAbsD ⊳ parseAbsDir  t
              ('/',_  ) → FAbsF ⊳ parseAbsFile t
              (_  ,'/') → FRelD ⊳ parseRelDir  t
              (_  ,_  ) → FRelF ⊳ parseRelFile t

parseFPath' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η FPath
parseFPath' = parseFPath

__parseFPath__ ∷ Printable τ ⇒ τ → FPath
__parseFPath__ = either __ERROR'__ id ∘ parseFPath'

__parseFPath'__ ∷ String → FPath
__parseFPath'__ = __parseFPath__

parseFPathTests ∷ TestTree
parseFPathTests =
  let success d f t = testCase t $ Right (d ## f) ≟ parseFPath' t
   in testGroup "parseFPath"
                [ success [absdir|/|]      _AbsDir  "/"
                , success [absdir|/etc/|]  _AbsDir  "/etc/"
                , success [absfile|/quux|] _AbsFile "/quux"
                , success [reldir|foo/|]   _RelDir  "foo/"
                , success [relfile|bar|]   _RelFile "bar"
                ]

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

class AppendableFPath γ where
  (⫻) ∷ DirType γ → RelType γ → γ

instance AppendableFPath AbsDir where
  d ⫻ f = (d ⊣ seq ⊕ f ⊣ seq) ⊣ re seq

instance AppendableFPath AbsFile where
  d ⫻ f = (d ⊣ seq ⪡ f ⊣ seqNE) ⊣ re seqNE

instance AppendableFPath RelDir where
  d ⫻ f = (d ⊣ seq ⊕ f ⊣ seq) ⊣ re seq

instance AppendableFPath RelFile where
  d ⫻ f = (d ⊣ seq ⪡ f ⊣ seqNE) ⊣ re seqNE

-- | non-unicode synonym of `(⫻)`
(</>) ∷ AppendableFPath γ ⇒ DirType γ → RelType γ → γ
(</>) = (⫻)

------------------------------------------------------------

class HasRelType π ⇒ Strippable π where
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

{-
class Show π ⇒ MyPath π where
  -- | convert to a File; / goes to Nothing
  toFile      ∷ π → Maybe (Path (AbsOrRel π) File)

  toFile_     ∷ π → Path (AbsOrRel π) File
  toFile_ = _ASSERT' ∘ toFile

-}

------------------------------------------------------------

{-

-- | current working directory
getCwd ∷ (MonadIO μ, AsIOError ε, AsPathError ε, MonadError ε μ) ⇒ μ AbsDir
getCwd = (asIOError getWorkingDirectory) >>= parseAbsDir ∘ toText

getCwd' ∷ (MonadIO μ, MonadError IOPathError μ) ⇒ μ AbsDir
getCwd' = getCwd

getCwd_ ∷ MonadIO μ ⇒ μ AbsDir
getCwd_ = eitherIOThrowT getCwd'

-}

----------------------------------------

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
tests = testGroup "FPath" [ parseAbsPathTests, parseRelPathTests
                          , parseDirTests, parseFileTests, parseFPathTests
                          , fileishTests
                          ]

-- that's all, folks! ----------------------------------------------------------
