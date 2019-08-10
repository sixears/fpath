{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.AbsDir
  ( AbsDir, NonRootAbsDir

  -- quasi-quoters
  , absdir, absdirN

  , nonRootAbsDir
  , root

  , parseAbsDir , parseAbsDir' , __parseAbsDir__ , __parseAbsDir'__
  , parseAbsDirN, parseAbsDirN', __parseAbsDirN__, __parseAbsDirN'__
  )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( return )
import Data.Either          ( Either( Left, Right ), either )
import Data.Eq              ( Eq )
import Data.Foldable        ( concat )
import Data.Function        ( ($), id )
import Data.Functor         ( fmap )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Exts             ( IsList( fromList, toList ), Item )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (∅), (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

import Data.Sequence  ( Seq( (:<|) ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

-- lens --------------------------------

import Control.Lens.Cons   ( unsnoc )
import Control.Lens.Iso    ( iso )
import Control.Lens.Lens   ( lens )
import Control.Lens.Prism  ( Prism', prism' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFunctor( omap ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋫) )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Monad        ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import qualified  NonEmptyContainers.SeqConversions  as  SeqConversions

import NonEmptyContainers.IsNonEmpty  ( FromNonEmpty( fromNonEmpty )
                                      , IsNonEmpty( nonEmpty )
                                      , ToNonEmpty( toNonEmpty )
                                      , defaultNonEmpty
                                      )
import NonEmptyContainers.SeqConversions
                                    ( FromMonoSeq( fromSeq ), IsMonoSeq( seq )
                                    , ToMonoSeq( toSeq ) )
import NonEmptyContainers.SeqNE     ( SeqNE( (:⫸) ), pattern (:⪬), (⪪), (⪫), onEmpty' )
import NonEmptyContainers.SeqNEConversions
                                    ( FromMonoSeqNonEmpty( fromSeqNE )
                                    , IsMonoSeqNonEmpty( seqNE )
                                    , ToMonoSeqNonEmpty( toSeqNE, toSeq_ ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char )
import Text.Parser.Combinators  ( endBy, endByNonEmpty )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- text --------------------------------

import Data.Text  ( Text, breakOnEnd )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.DirType           ( DirType )
import FPath.Error.FPathError  ( AsFPathError, FPathError( FPathRootDirE )
                               , __FPathComponentE__, __FPathEmptyE__
                               , __FPathAbsE__, __FPathNonAbsE__
                               , __FPathNotADirE__
                               )
import FPath.HasAbsOrRel       ( Abs, HasAbsOrRel( AbsOrRel ) )
import FPath.HasParent         ( HasParent( parent ),HasParentMay( parentMay ) )
import FPath.PathComponent     ( PathComponent, parsePathC )
import FPath.Util              ( QuasiQuoter
                               , __ERROR'__, mkQuasiQuoterExp )

-------------------------------------------------------------------------------

{- | The root directory, i.e., '/' -}
data RootDir = RootDir
  deriving (Eq, Show)

{- | A non-root absolute directory, e.g., /etc. -}
-- a non-root dir is a path component appended to a (possibly-root) absolute
-- directory
data NonRootAbsDir = NonRootAbsDir PathComponent AbsDir
  deriving (Eq, Show)

{- | An absolute directory is either the root directory, or a non-root absolute
     directory -}
data AbsDir = AbsRootDir | AbsNonRootDir NonRootAbsDir
  deriving (Eq, Show)

----------------------------------------

type instance Element NonRootAbsDir = PathComponent
type instance Element AbsDir = PathComponent

----------------------------------------

instance MonoFunctor NonRootAbsDir where
  omap ∷ (PathComponent → PathComponent) → NonRootAbsDir → NonRootAbsDir
  omap f (NonRootAbsDir p a) = NonRootAbsDir (f p) (omap f a)

instance MonoFunctor AbsDir where
  omap ∷ (PathComponent → PathComponent) → AbsDir → AbsDir
  omap _ AbsRootDir = AbsRootDir
  omap f (AbsNonRootDir d) = AbsNonRootDir (omap f d)

nonRootAbsDir ∷ Prism' AbsDir NonRootAbsDir
nonRootAbsDir = prism' AbsNonRootDir go
                where go AbsRootDir        = Nothing
                      go (AbsNonRootDir d) = Just d

----------------------------------------

instance FromMonoSeqNonEmpty NonRootAbsDir where
  fromSeqNE ∷ SeqNE PathComponent → NonRootAbsDir
  fromSeqNE (ps :⫸ p) = NonRootAbsDir p (onEmpty' AbsRootDir fromSeq ps)
  fromSeqNE _ = error "failed to unsnoc SeqNE"

instance FromMonoSeqNonEmpty AbsDir where
  fromSeqNE = AbsNonRootDir ∘ fromSeqNE

----------------------------------------

instance ToMonoSeqNonEmpty NonRootAbsDir where
  toSeqNE (NonRootAbsDir p d) =  -- /etc/pki/tls → NonRootAbsDir tls /etc/pki/
    case toSeq d of  -- toSeq d → [ etc/, pki/ ]
      Seq.Empty → pure p -- ncons p Seq.Empty
      x :⪬ xs  → (x ⪪ xs) :⫸ p

----------------------------------------

instance IsMonoSeqNonEmpty NonRootAbsDir where
  seqNE = iso toSeqNE fromSeqNE

----------------------------------------

instance FromMonoSeq AbsDir where
  fromSeq xs = go (Seq.reverse $ xs)
               where go Seq.Empty = AbsRootDir
                     go (y :<| ys) = AbsNonRootDir (NonRootAbsDir y (go ys))

----------------------------------------

instance ToMonoSeq AbsDir where
  toSeq AbsRootDir = (∅)
  toSeq (AbsNonRootDir (NonRootAbsDir pc ad)) = ad ⊣ seq ⪫ pc

instance ToMonoSeq NonRootAbsDir where
  toSeq = toSeq_

----------------------------------------

instance IsMonoSeq AbsDir where
  seq = iso toSeq fromSeq

----------------------------------------

instance IsList AbsDir where
  type instance Item AbsDir = PathComponent
  fromList = fromSeq ∘ Seq.fromList
  toList   = toList ∘ toSeq

----------------------------------------

instance FromNonEmpty NonRootAbsDir where
  fromNonEmpty = fromSeqNE ∘ fromNonEmpty

----------------------------------------

instance ToNonEmpty NonRootAbsDir where
  toNonEmpty   = toNonEmpty ∘ toSeqNE

----------------------------------------

instance IsNonEmpty NonRootAbsDir where
  nonEmpty = defaultNonEmpty

----------------------------------------

{- | Convert a sequence of printable elements to strings by intercalating '/'
     characters; with a post-fact string transformation for extra bells (e.g.,
     a prefix '/' character -}
pDir ∷ (P.Printer ρ, ToMonoSeq α, Printable (Element α)) ⇒
       (String → String) → α → ρ
pDir f =  P.string ∘ f ∘ concat ∘ fmap ((⊕ "/") ∘ toString) ∘ toSeq

instance Printable AbsDir where
  print = pDir ("/" ⊕)

instance Printable NonRootAbsDir where
  print = pDir ("/" ⊕)

----------------------------------------

instance AsFilePath AbsDir where
  filepath = prism' toString fromString

instance AsFilePath NonRootAbsDir where
  filepath = prism' toString fromString

----------------------------------------

instance Textual AbsDir where
  textual = SeqConversions.fromList ⊳ (char '/' ⋫ endBy textual (char '/'))

instance Textual NonRootAbsDir where
  textual =
    fromSeqNE ∘ fromNonEmpty ⊳ (char '/' ⋫ endByNonEmpty textual (char '/'))

----------------------------------------

instance Arbitrary AbsDir where
  arbitrary = fromSeq ⊳ arbitrary
  shrink = fromSeq ⩺ shrink ∘ toSeq

instance Arbitrary NonRootAbsDir where
  arbitrary = fromSeqNE ⊳ arbitrary
  shrink = fromSeqNE ⩺ shrink ∘ toSeqNE

----------------------------------------

type instance DirType Abs = AbsDir

instance HasAbsOrRel AbsDir where
  type AbsOrRel AbsDir = Abs

instance HasAbsOrRel NonRootAbsDir where
  type AbsOrRel NonRootAbsDir = Abs

----------------------------------------

instance HasParent NonRootAbsDir where
  parent = lens getParent setParent
           where getParent (NonRootAbsDir _ d) = d
                 setParent (NonRootAbsDir p _) d = NonRootAbsDir p d


----------------------------------------

instance HasParentMay AbsDir where
  parentMay = lens getParentMay setParentMay
              where getParentMay AbsRootDir                          = Nothing
                    getParentMay (AbsNonRootDir (NonRootAbsDir _ d)) = Just d

                    setParentMay AbsRootDir (Just d) = d
                    setParentMay AbsRootDir Nothing  = AbsRootDir
                    setParentMay (AbsNonRootDir (NonRootAbsDir p _)) (Just d) =
                      AbsNonRootDir (NonRootAbsDir p d)
                    setParentMay (AbsNonRootDir (NonRootAbsDir p _)) Nothing =
                      AbsNonRootDir (NonRootAbsDir p AbsRootDir)

----------------------------------------

instance HasParentMay NonRootAbsDir where
  parentMay = lens getParentMay setParentMay
              where getParentMay (NonRootAbsDir _ d) = Just d

                    setParentMay (NonRootAbsDir p _) (Just d) =
                      NonRootAbsDir p d
                    setParentMay (NonRootAbsDir p _) Nothing  =
                      NonRootAbsDir p AbsRootDir

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

parseAbsDirN ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒
               τ → η NonRootAbsDir
parseAbsDirN t = do
--  d ← parseAbsDir t
  parseAbsDir t ≫ \ case
    AbsRootDir → __FPathAbsE__ nrabsdirT (toText t)
    AbsNonRootDir d' → return d'

parseAbsDirN' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η NonRootAbsDir
parseAbsDirN' = parseAbsDirN

__parseAbsDirN__ ∷ String → NonRootAbsDir
__parseAbsDirN__ s = case parseAbsDir' s of
                       Left e → __ERROR'__ e
                       Right AbsRootDir → __ERROR'__ $ FPathRootDirE nrabsdirT
                       Right (AbsNonRootDir nr) → nr

__parseAbsDirN'__ ∷ String → NonRootAbsDir
__parseAbsDirN'__ = __parseAbsDirN__

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

-- that's all, folks! ----------------------------------------------------------
