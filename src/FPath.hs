{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath
  ( AbsDir, NonRootAbsDir {- AbsFile, AbsPath(..) -}
  , AsFilePath( filepath )

  -- quasi-quoters
  , absdir, absdirN

  -- constructor fns
  , fromList, fromListNE

  , nonRootAbsDir

  , parent, parentMay
  , parseAbsDir, parseAbsDir', __parseAbsDir__, __parseAbsDir'__
  , pcSeq

  , root
    {-
  , AsFilePath( toFPath )
  , MyPath( AbsOrRel, FileType, isRoot, resolve, toDir, toFile, toFile_
          , getFilename, getParent, setFilename, setExt )
  , RelDir, RelFile

  , AsPathError, PathError
  , Path, Abs, Dir, File, Rel

  , (~<.>), (<.>~)
  , pathIsAbsolute
  , rel2abs
  , rootDir

  , extension, filename, parent

  , parseAbsDir , parseAbsDir' , parseAbsDir_
  , parseAbsFile, parseAbsFile', parseAbsFile_
  , parseAbsPath, parseAbsPath', parseAbsPath_
  , parseRelDir , parseRelDir' , parseRelDir_
  , parseRelFile, parseRelFile', parseRelFile_
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

import Prelude  ( undefined )

-- base --------------------------------

import qualified  Data.Foldable       as  Foldable
import qualified  Data.List.NonEmpty  as  NonEmpty


import Control.Applicative  ( (*>) )
import Control.Monad        ( return )
import Data.Either          ( Either( Left, Right ), either )
import Data.Eq              ( Eq )
import Data.Foldable        ( Foldable, concat, foldl', foldl1, foldr, foldr1 )
import Data.Function        ( ($), id )
import Data.Functor         ( (<$>), fmap )
import Data.List            ( reverse )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), TypeRep, typeRep )
import System.IO            ( FilePath )
import Text.Show            ( Show( show ) )

{-

import Control.Monad  ( (>>=), return )
import Data.Bool      ( Bool( False, True ) )
import Data.Either    ( Either( Left, Right ), either )
import Data.Eq        ( (==) )
import Data.Function  ( ($), const, flip, id )
import Data.Functor   ( (<$>), fmap )
import Data.Maybe     ( Maybe( Just, Nothing ), fromMaybe, maybe )
import Data.Monoid    ( (<>) )
import Prelude        ( error )
import System.IO      ( FilePath )

-}

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

import Data.Sequence  ( Seq( Empty, (:<|), (:|>) ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

{-

-- directory ---------------------------

import System.Directory  ( withCurrentDirectory )

-}

-- lens --------------------------------

import Control.Lens.Getter   ( (^.), view )
import Control.Lens.Iso      ( Iso', from, iso )
import Control.Lens.Lens     ( Lens', lens )
import Control.Lens.Prism    ( Prism', prism' )

{-

import qualified  System.FilePath.Lens  as  FPLens

-}

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element
                             , MonoFoldable( ofoldMap, ofoldl', ofoldr
                                           , ofoldr1Ex, ofoldl1Ex', otoList )
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.List     ( (⋮) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsers -----------------------------

import Text.Parser.Char         ( char )
import Text.Parser.Combinators  ( endBy )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ), shrinkList )
import Test.QuickCheck.Gen        ( Gen )

-- text --------------------------------

import Data.Text  ( Text, breakOnEnd, unsnoc )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

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

import FPath.Error.FPathError  ( AsFPathError, FPathError( FPathRootDirE )
                               , __FPathComponentE__, __FPathEmptyE__
                               , __FPathNonAbsE__, __FPathNotADirE__
                               )
import FPath.PathComponent     ( PathComponent, parsePathC )
import FPath.Util              ( QuasiQuoter
                               , __ERROR'__, mkQuasiQuoterExp )

-------------------------------------------------------------------------------

------------------------------------------------------------
--                         AbsDir                         --
------------------------------------------------------------

data RootDir = RootDir
  deriving Eq

data NonRootAbsDir = NonRootAbsDir PathComponent AbsDir
  deriving (Eq, Show)

type instance Element NonRootAbsDir = PathComponent

data AbsDir = AbsRootDir | AbsNonRootDir NonRootAbsDir
  deriving Eq

type instance Element AbsDir = PathComponent

nonRootAbsDir ∷ Prism' AbsDir NonRootAbsDir
nonRootAbsDir = prism' AbsNonRootDir go
                where go AbsRootDir        = Nothing
                      go (AbsNonRootDir d) = Just d

{-

data RelDir = RelDir PathComponent (Maybe RelDir)

data AbsFile = AbsFile PathComponent AbsDir

data RelFile = RelFile PathComponent (Maybe RelDir)

-}

----------------------------------------
--               ToSeq                --
----------------------------------------

class ToSeq α where
  toSeq ∷ α → Seq (Element α)

instance ToSeq AbsDir where
  toSeq AbsRootDir = Empty
  toSeq (AbsNonRootDir (NonRootAbsDir pc ad)) = ad ^. pcSeq :|> pc

----------------------------------------
--               ToList               --
----------------------------------------

class ToList α where
  toList ∷ α → [Element α]

instance ToList AbsDir where
  toList = Foldable.toList ∘ toSeq

----------------------------------------
--              FromSeq               --
----------------------------------------

class FromSeq α where
   fromSeq ∷ Seq (Element α) → α

instance FromSeq AbsDir where
  fromSeq xs = go (Seq.reverse $ xs)
               where go Empty = AbsRootDir
                     go (y :<| ys) = AbsNonRootDir (NonRootAbsDir y (go ys))

----------------------------------------
--              FromList              --
----------------------------------------

class FromList α where
   fromList ∷ Foldable ψ ⇒ ψ (Element α) → α

instance FromList AbsDir where
  fromList xs = fromSeq (Seq.fromList $ Foldable.toList xs)

----------------------------------------
--          FromListNonEmpty          --
----------------------------------------

class FromListNonEmpty α where
   fromListNE ∷ NonEmpty (Element α) → α

instance FromListNonEmpty NonRootAbsDir where
  fromListNE (NonEmpty.reverse → x :| xs) =
    NonRootAbsDir x (fromList $ reverse xs)

----------------------------------------
--            MonoFoldable            --
----------------------------------------

instance MonoFoldable AbsDir where
  ofoldMap   = undefined

  ofoldl' ∷ (α → PathComponent → α) → α → AbsDir → α
  ofoldl' f init d = foldl' f init (toList d)

  ofoldr ∷ (PathComponent → α → α) → α → AbsDir → α
  ofoldr f init d = foldr f init (toList d)

  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → AbsDir
            → PathComponent
  ofoldr1Ex f d  = foldr1 f (toList d)

  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → AbsDir
             → PathComponent
  ofoldl1Ex' f d = foldl1 f (toList d)

----------------------------------------
--              IsPCSeq               --
----------------------------------------

class IsPCSeq α where
{-
  toPCSeq ∷ α → Seq PathComponent
  setPCSeq ∷ Seq PathComponent → α
-}

  pcSeq ∷ Iso' α (Seq PathComponent)
--  pcSeq = iso toPCSeq setPCSeq

instance IsPCSeq AbsDir where
{-
  setPCSeq xs = go (Seq.reverse xs)
              where go Empty = AbsRootDir
                    go (y :<| ys) = AbsNonRootDir (NonRootAbsDir y (go ys))
  toPCSeq AbsRootDir = Empty
  toPCSeq (AbsNonRootDir (NonRootAbsDir pc ad)) = ad ^. pcSeq :|> pc
-}

  pcSeq = iso toSeq fromSeq

----------------------------------------
--                Show                --
----------------------------------------

instance Show AbsDir where
  show ad = [fmt|((^. from pcList) [%L])|] (show ⊳ toList ad)

----------------------------------------
--             Printable              --
----------------------------------------

instance Printable AbsDir where
  print = P.string ∘ ("/" ⊕ ) ∘ concat ∘ fmap ((⊕ "/") ∘ toString) ∘ otoList

----------------------------------------
--              Textual               --
----------------------------------------

instance Textual AbsDir where
  textual = fromList ⊳ (char '/' *> endBy textual (char '/'))

----------------------------------------
--              Arbitrary             --
----------------------------------------

instance Arbitrary AbsDir where
  arbitrary = fromList ⊳ (arbitrary ∷ Gen [PathComponent])
  -- "standard" definition for lists:
  shrink = fmap fromList ∘ shrinkList shrink ∘ toList

----------------------------------------
--            HasAbsOrRel             --
----------------------------------------

-- data Rel = Rel
data Abs = Abs

type family DirType α where
  DirType Abs = AbsDir

class HasAbsOrRel α where
  type AbsOrRel α

instance HasAbsOrRel AbsDir where
  type AbsOrRel AbsDir = Abs

instance HasAbsOrRel NonRootAbsDir where
  type AbsOrRel NonRootAbsDir = Abs

----------------------------------------
--             HasParent              --
----------------------------------------

class HasAbsOrRel α ⇒ HasParent α where
  parent ∷ Lens' α (DirType (AbsOrRel α))

instance HasParent NonRootAbsDir where
  parent = lens (\ (NonRootAbsDir _ d) → d)
                (\ (NonRootAbsDir p _) d → NonRootAbsDir p d)

----------------------------------------
--           HasMaybeParent           --
----------------------------------------

class HasAbsOrRel α ⇒ HasMaybeParent α where
  parentMay ∷ Lens' α (Maybe (DirType (AbsOrRel α)))

instance HasMaybeParent AbsDir where
  parentMay =
    let getParent ∷ AbsDir → Maybe AbsDir
        getParent AbsRootDir                          = Nothing
        getParent (AbsNonRootDir (NonRootAbsDir _ d)) = Just d

        setParent ∷ AbsDir → Maybe AbsDir → AbsDir
        setParent AbsRootDir (Just d) = d
        setParent AbsRootDir Nothing  = AbsRootDir
        setParent (AbsNonRootDir (NonRootAbsDir p _)) (Just d) =
          AbsNonRootDir (NonRootAbsDir p d)
        setParent (AbsNonRootDir (NonRootAbsDir p _)) Nothing =
          AbsNonRootDir (NonRootAbsDir p AbsRootDir)

     in lens getParent setParent

----------------------------------------
--             AsFilePath             --
----------------------------------------

class AsFilePath α where
  filepath ∷ Prism' FilePath α

instance AsFilePath AbsDir where
  filepath = prism' toString fromString

-- list non-empty pattern match
-- seq unicode pattern match
-- more-unicode

------------------------------------------------------------
--                     Quasi-Quoting                      --
------------------------------------------------------------

absdirT ∷ TypeRep
absdirT = typeRep (Proxy ∷ Proxy AbsDir)

nrabsdirT ∷ TypeRep
nrabsdirT = typeRep (Proxy ∷ Proxy NonRootAbsDir)

parseAbsDir ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η AbsDir
parseAbsDir (toText → "") = __FPathEmptyE__ absdirT
parseAbsDir (toText → t) =
  let mkAbsDir ∷ (AsFPathError ε, MonadError ε η) ⇒ Text → η AbsDir
      mkAbsDir x = do
        let (p,s) = breakOnEnd ("/") x
            mkCompE ce = __FPathComponentE__ ce absdirT t
        s' ← either mkCompE return $ parsePathC s
        p' ← go p
        return ∘ AbsNonRootDir $ NonRootAbsDir s' p'

      go ∷ (AsFPathError ε, MonadError ε η) ⇒ Text → η AbsDir
      go x = case unsnoc x of
               Nothing → __FPathNonAbsE__ absdirT t
               Just ("", '/') → return AbsRootDir
               Just (x', '/') → mkAbsDir x'
               _              → __FPathNotADirE__ absdirT t

   in go t

parseAbsDir' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η AbsDir
parseAbsDir' = parseAbsDir

__parseAbsDir__ ∷ Printable τ ⇒ τ → AbsDir
__parseAbsDir__ = either __ERROR'__ id ∘ parseAbsDir'

__parseAbsDir'__ ∷ String → AbsDir
__parseAbsDir'__ = __parseAbsDir__

__parseAbsDirN__ ∷ String → NonRootAbsDir
__parseAbsDirN__ s = case parseAbsDir' s of
                       Left e → __ERROR'__ e
                       Right AbsRootDir → __ERROR'__ $ FPathRootDirE nrabsdirT
                       Right (AbsNonRootDir nr) → nr

{- | quasi-quoter for AbsDir -}
absdir ∷ QuasiQuoter
absdir = mkQuasiQuoterExp "absdir" (\ s → ⟦ __parseAbsDir'__ s ⟧)

{- | quasi-quoter for NonRootAbsDir -}

absdirN ∷ QuasiQuoter
absdirN = mkQuasiQuoterExp "absdirN" (\ s → ⟦ __parseAbsDirN__ s ⟧)

----------------------------------------
--             constants              --
----------------------------------------

root ∷ AbsDir
root = AbsRootDir

------------------------------------------------------------

{-
class AsFilePath ρ where
  toFPath ∷ ρ → FilePath

instance AsFilePath (Path β τ) where
  toFPath = toFilePath

-}
------------------------------------------------------------

{-
data AbsPath = AbsD AbsDir | AbsF AbsFile
  deriving Show

instance AsFilePath AbsPath where
  toFPath (AbsD d) = toFilePath d
  toFPath (AbsF f) = toFilePath f


parseAbsPath ∷ (Printable τ, AsPathError ε, MonadError ε μ) ⇒ τ → μ AbsPath
parseAbsPath (toText → t) =
    case last t of
      Just '/' → case parseAbsDir t of
                    Left err → throwError err
                    Right d  → return $ AbsD d
      _        → -- includes the 'Nothing' case, but that's fine, parseAbsFile
                  -- will throw an AsPathError on that
                  case parseAbsFile t of
                    Left err → throwError err
                    Right f  → return $ AbsF f

parseAbsPath' ∷ (Printable τ, MonadError PathError μ) ⇒ τ → μ AbsPath
parseAbsPath' = parseAbsPath

-- | PARTIAL: use only when you're sure that the value will parse as an
--            absolute path
parseAbsPath_ ∷ Printable τ ⇒ τ → AbsPath
parseAbsPath_ = _ASSERT ∘ parseAbsPath'

-}

------------------------------------------------------------

{-
class Show π ⇒ MyPath π where
  type FileType π
  type AbsOrRel π

  isRoot      ∷ π → Bool

  toDir       ∷ π → Path (AbsOrRel π) Dir

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
resolveDir ∷ (MonadIO μ, AsIOError ε, AsPathError ε, MonadError ε μ)⇒
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
