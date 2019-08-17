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

-- toFile only works if not / and not ./ .  Adjust RelDir to distinguish ./ so
-- we can mark this at the type level

module FPath
  ( AbsDir, AbsFile, NonRootAbsDir, RelDir, RelFile {- AbsPath(..) -}
  , AsFilePath( filepath )

  -- quasi-quoters
  , absdir, absfile, absdirN, reldir, relfile

  -- List/NonEmpty/Seq conversion fns
  , toSeq

  , nonRootAbsDir

  , parseAbsDir  , parseAbsDir'  , __parseAbsDir__  , __parseAbsDir'__
  , parseAbsFile , parseAbsFile' , __parseAbsFile__ , __parseAbsFile'__
  , parseAbsDirN , parseAbsDirN' , __parseAbsDirN__ , __parseAbsDirN'__
  , parseAbsPath , parseAbsPath' , __parseAbsPath__ , __parseAbsPath'__
  , parseRelDir  , parseRelDir'  , __parseRelDir__  , __parseRelDir'__
  , parseRelFile , parseRelFile' , __parseRelFile__ , __parseRelFile'__
  , seq, seqNE

  , root
  , toDir
  , resolve, stripPrefix, stripPrefix'
    {-
  , MyPath( AbsOrRel, FileType, toFile, toFile_
          , getFilename, getParent, setFilename, setExt )

  , (~<.>), (<.>~)
  , pathIsAbsolute
  , rel2abs

  , extension, filename, parent

  , parseRelPath, parseRelPath', parseRelPath_
  , stripDir    , stripDir'    , stripDir_

  , getCwd        , getCwd'        , getCwd_
  , resolveDir    , resolveDir'    , resolveDir_
  , resolveDirCwd , resolveDirCwd' , resolveDirCwd_
  , resolveFile   , resolveFile'   , resolveFile_
  , resolveFileCwd, resolveFileCwd', resolveFileCwd_

  , parseDir, parseDir'
  , parseFile, parseFile'
  , parseFileAbs, parseFileAbs'
  , parseDirAbs, parseDirAbs'
-}
  )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Bool      ( Bool( False, True ) )
import Data.Either    ( either )
import Data.Eq        ( Eq )
import Data.Function  ( id )
import Data.Maybe     ( Maybe( Just, Nothing ), maybe )
import Data.String    ( String )
import Data.Typeable  ( Proxy( Proxy ), TypeRep, typeRep )
import Text.Show      ( Show )

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
import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.Review  ( re )

-- more-unicode-symbols ----------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣), (⩼) )

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

-- text --------------------------------

import Data.Text  ( last, null )

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
import FPath.Error.FPathError  ( AsFPathError, AsFPathNotAPrefixError
                               , FPathError, FPathNotAPrefixError
                               , __FPathEmptyE__, __FPathNotAPrefixError__
                               )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir
                               , parseRelDir, parseRelDir'
                               , __parseRelDir'__, __parseRelDir__, reldir )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile
                               , parseRelFile, parseRelFile', __parseRelFile'__
                               , __parseRelFile__, relfile
                               )
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

class File π δ where
  toDir ∷ π → δ

instance File AbsFile AbsDir where
  toDir ∷ AbsFile → AbsDir
  toDir = fromSeq ∘ toSeq

instance File RelFile RelDir where
  toDir ∷ RelFile → RelDir
  toDir = fromSeq ∘ toSeq

------------------------------------------------------------

class HasAbsify α where
  type Absify α

instance HasAbsify RelFile where
  type Absify RelFile = AbsFile

instance HasAbsify RelDir where
  type Absify RelDir = AbsDir

class Relative π {- τ -} where
  resolve ∷ AbsDir → π → Absify π

  {- | "unresolve" a path; that is, if an absolute directory is a prefix of an
       absolute (file|directory), then strip off that prefix to leave a relative
       (file|directory).  Note that a file will always keep its filename; but a
       directory might result in a relative dir of './'.
   -}
  stripPrefix ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                AbsDir → Absify π → η π
  stripPrefix' ∷ MonadError FPathNotAPrefixError η ⇒ AbsDir → Absify π → η π
  stripPrefix' = stripPrefix


instance Relative RelFile where
  resolve ∷ AbsDir → RelFile → AbsFile
  resolve d f = (d ⊣ seq ⪡ f ⊣ seqNE) ⊣ re seqNE

  stripPrefix ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                AbsDir → AbsFile → η RelFile
  stripPrefix d'@(view seq → d) f'@(view seqNE → f) =
    maybe (__FPathNotAPrefixError__ absfileT (toText d') (toText f'))
          (return ∘ fromSeqNE)
          (SeqNE.stripProperPrefix d f)

instance Relative RelDir where
  resolve ∷ AbsDir → RelDir → AbsDir
  resolve d f = (d ⊣ seq ⊕ f ⊣ seq) ⊣ re seq

  stripPrefix ∷ (AsFPathNotAPrefixError ε, MonadError ε η) ⇒
                AbsDir → AbsDir → η RelDir
  stripPrefix d'@(view seq → d) f'@(view seq → f) =
    maybe (__FPathNotAPrefixError__ absdirT (toText d') (toText f'))
          (return ∘ fromSeq)
          (SeqConversions.stripPrefix d f)


------------------------------------------------------------


-- </>

{-
class Show π ⇒ MyPath π where
  -- | convert to a File; / goes to Nothing
  toFile      ∷ π → Maybe (Path (AbsOrRel π) File)

  toFile_     ∷ π → Path (AbsOrRel π) File
  toFile_ = _ASSERT' ∘ toFile

  resolve     ∷ AbsDir → π → Path Abs (FileType π)

  -- | throws if used on /
  setFilename ∷ Path Rel τ → π → Path (AbsOrRel π) τ

  -- er, what if input is '/'?  Currently partial, NO LIKE
  getFilename ∷ π → Path Rel (FileType π)

  getParent   ∷ π → Maybe (Path (AbsOrRel π) Dir)

  getParent_  ∷ π → Path (AbsOrRel π) Dir
  getParent_ p =
    fromMaybe (error $ [fmt|cannot getParent of %s|] (show p))
              (getParent p)

  setExt      ∷ Text → π → π

setParent ∷ (FileType (Path α τ) ~ τ, AbsOrRel (Path α τ) ~ α,
              MyPath (Path α τ)) ⇒
             Path β Dir → Path α τ → Path β τ
setParent p o = p </> view filename o

relParent ∷ Path Rel τ → Maybe (Path Rel Dir)
relParent f = let p = Path.parent (rootDir </> f)
               in if rootDir == p
                  then Nothing
                  else Just $ _ASSERT (stripDir' rootDir p)

relSetFilename ∷ (MyPath π, AbsOrRel π ~ Rel) ⇒ Path Rel τ → π → Path Rel τ
relSetFilename fn old = maybe fn (</> fn) (getParent old){- case parent old of
                          Nothing → fn
                          Just p  → p </> fn -}




instance MyPath (Path Abs Dir) where
  type FileType (Path Abs Dir) = Dir
  type AbsOrRel (Path Abs Dir) = Abs
  isRoot    d               = d == rootDir
  toDir                     = parseAbsDir_ ∘ pack ∘ toFilePath
  toFile    d               = case dropWhileEnd (== '/') $ pack (toFilePath d) of
                                "" → Nothing
                                f  → Just $ parseAbsFile_ f
  resolve                   = const
  getFilename d             = if rootDir == d
                              then error "cannot getFilename on /"
                              else Path.dirname d
  setFilename fn old        = maybe (error "cannot setFilename on /")
                                    (</> fn) (getParent old)
  getParent d | (d == rootDir) = Nothing
              | True           = Just $ Path.parent d

  -- | setting an extension (e.g., ".ext") on "/" would result in a subdir of
  --   "/" (e.g., "/.ext/"), which is contrary to the general property of setExt
  --   that
  --
  --   > getParent (setExt e f) = getParent f
  setExt e d = case toFile d of
                 Nothing → case e of
                              "" → rootDir
                              _  → error $
                                       "cannot setExt '" <> unpack e <> "' to /"
                 Just d' → toDir (setFileExtension_ e d')

instance MyPath (Path Abs File) where
  type FileType (Path Abs File) = File
  type AbsOrRel (Path Abs File) = Abs
  isRoot _           = False
  toDir              = parseAbsDir_ ∘ pack ∘ toFilePath
  toFile             = Just
  resolve _ f        = f
  setFilename fn old =
    maybe (error $ [fmt|no parent found for '%s'|] (toFilePath old))
          (</> fn)
          (getParent old)
  getFilename        = Path.filename
  getParent d        = Just $ Path.parent d
  setExt             = setFileExtension_

instance MyPath (Path Rel Dir) where
  type FileType (Path Rel Dir) = Dir
  type AbsOrRel (Path Rel Dir) = Rel
  isRoot _    = False
  toDir       = id
  toFile      = Just ∘ parseRelFile_ ∘ dropWhileEnd (== '/') ∘ pack ∘ toFilePath
  resolve d f = (Path.</>) d f
  getFilename = Path.dirname
  getParent   = relParent
  setFilename = relSetFilename

  setExt e d = toDir $ setFileExtension_ e (_ASSERT' $ toFile d)

instance MyPath (Path Rel File) where
  type FileType (Path Rel File) = File
  type AbsOrRel (Path Rel File) = Rel
  isRoot _    = False
  toDir       = parseRelDir_ ∘ pack ∘ toFilePath
  toFile      = Just
  resolve d f = (Path.</>) d f
  getFilename = Path.filename
  getParent   = relParent
  setFilename = relSetFilename
  setExt      = setFileExtension_

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

-- | Strip directory from path, making it relative to that directory.
--   Throws Couldn'tStripPrefixDir if directory is not a parent of the path.
--   See `Path.stripDir`

stripDir ∷ (AsPathError ε, MonadError ε μ) ⇒
            Path β Dir → Path β τ  → μ (Path Rel τ)
stripDir path dir =
  mapMError (pathError "stripDir" (toFilePath path))
            (Path.stripProperPrefix path dir)

stripDir' ∷ (MonadError PathError μ) ⇒
             Path β Dir → Path β τ  → μ (Path Rel τ)
stripDir' = stripDir

stripDir_ ∷ Path β Dir → Path β τ  → Path Rel τ
stripDir_ p = _ASSERT ∘ stripDir' p

-}

----------------------------------------

{-

rootDir ∷ AbsDir
rootDir = [absdir|/|]

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

{-

parseAbsFile ∷ (AsPathError ε, MonadError ε μ, Printable τ) ⇒
                τ → μ (Path Abs File)
parseAbsFile fn =
  mapMError (pathError "parseAbsFile" fn) $ Path.parseAbsFile (toString fn)

parseAbsFile' ∷ (MonadError PathError μ, Printable τ) ⇒ τ → μ (Path Abs File)
parseAbsFile' = parseAbsFile

-- | PARTIAL: use only when you're sure that the value will parse as an
--            absolute file
parseAbsFile_ ∷ Printable τ ⇒ τ → AbsFile
parseAbsFile_ = _ASSERT ∘ parseAbsFile'

-}

----------------------------------------

{-

parseAbsDir ∷ (AsPathError ε, MonadError ε μ, Printable τ) ⇒
               τ → μ (Path Abs Dir)
parseAbsDir fn =
  mapMError (pathError "parseAbsDir" fn) $ Path.parseAbsDir (toString fn)

parseAbsDir' ∷ (MonadError PathError μ, Printable τ) ⇒ τ → μ (Path Abs Dir)
parseAbsDir' = parseAbsDir

-- | PARTIAL: use only when you're sure that the value will parse as a
--            absolute dir
parseAbsDir_ ∷ Printable τ ⇒ τ → AbsDir
parseAbsDir_ = _ASSERT ∘ parseAbsDir'

-}

----------------------------------------

{-

parseRelFile ∷ (AsPathError ε, MonadError ε μ, Printable τ) ⇒ τ → μ RelFile
parseRelFile fn =
  mapMError (pathError "parseRelFile" fn) $ Path.parseRelFile (toString fn)

parseRelFile' ∷ (MonadError PathError μ, Printable τ) ⇒ τ → μ RelFile
parseRelFile' = parseRelFile

-- | PARTIAL: use only when you're sure that the value will parse as a
--            relative file
parseRelFile_ ∷ Printable τ ⇒ τ → RelFile
parseRelFile_ = _ASSERT ∘ parseRelFile'

-}

----------------------------------------

{-

parseRelDir ∷ (AsPathError ε, MonadError ε μ, Printable τ) ⇒ τ → μ RelDir
parseRelDir fn =
  mapMError (pathError "parseRelDir" fn) $ Path.parseRelDir (toString fn)

parseRelDir' ∷ (MonadError PathError μ, Printable τ) ⇒ τ → μ RelDir
parseRelDir' = parseRelDir

-- | PARTIAL: use only when you're sure that the value will parse as a
--            relative dir
parseRelDir_ ∷ Printable τ ⇒ τ → RelDir
parseRelDir_ = _ASSERT ∘ parseRelDir'

-}

----------------------------------------

{-

-- | is path non-empty and beginning with '/'?
pathIsAbsolute ∷ Text → Bool
pathIsAbsolute (uncons → Nothing)       = False
pathIsAbsolute (uncons → Just ('/', _)) = True
pathIsAbsolute _                         = False

-}

----------------------------------------

{-

-- | *SET* an extension on a file; this is a partial function, it will fail
--   if you give it an extension that causes the filename not to parse (e.g.,
--   with a / at the end).
(~<.>) ∷ Path β File → Text → Path β File
fn ~<.> e = let pe = pathError' "~<.>" (toFPath fn)
                fn' = mapMError pe $ Path.setFileExtension (toString e) fn
            in case fn' of
                 Right r → r
                 Left  l → error (show l)

(<.>~) ∷ Path β File → Text → Path β File
(<.>~) = (~<.>)

-}

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

-- that's all, folks! ----------------------------------------------------------
