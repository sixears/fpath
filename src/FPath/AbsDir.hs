{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns        #-}

module FPath.AbsDir
  ( AbsDir, AsAbsDir( _AbsDir ), AsNonRootAbsDir( _NonRootAbsDir )
  , NonRootAbsDir, ToAbsDir( toAbsDir )

  , absdirT
  -- quasi-quoters
  , absdir, absdirN

  , nonRootAbsDir
  , root

  , parseAbsDirP , parseAbsDirP'  , __parseAbsDirP__ -- , __parseAbsDirP'__

  , tests
  )
where

import Prelude  ( error )

-- base --------------------------------

import qualified  Data.List.NonEmpty  as  NonEmpty

import Control.Applicative  ( pure )
import Control.Monad        ( mapM, return )
import Data.Either          ( Either( Left, Right ), either )
import Data.Eq              ( Eq )
import Data.Foldable        ( concat, foldl', foldl1, foldMap, foldr, foldr1 )
import Data.Function        ( ($), (&), const, id )
import Data.Functor         ( fmap )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( Maybe( Just, Nothing ), maybe )
import Data.Monoid          ( Monoid )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Exts             ( IsList( fromList, toList ), Item )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (∅), (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

-- lens --------------------------------

import Control.Lens.Cons    ( unsnoc )
import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( iso )
import Control.Lens.Lens    ( Lens', lens )
import Control.Lens.Prism   ( Prism', prism' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFoldable( ofoldl', ofoldl1Ex'
                                                    , ofoldMap, ofoldr
                                                    , ofoldr1Ex, otoList )
                             , MonoFunctor( omap )
                             )
import Data.NonNull          ( fromNullable )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋫) )
import Data.MoreUnicode.Function     ( (⅋) )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊣), (⊢) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ф )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import qualified  NonEmptyContainers.SeqConversions  as  SeqConversions
import qualified  NonEmptyContainers.SeqNE           as  SeqNE

import NonEmptyContainers.IsNonEmpty  ( FromMonoNonEmpty( fromNonEmpty )
                                      , IsMonoNonEmpty( nonEmpty )
                                      , ToMonoNonEmpty( toNonEmpty )
                                      , defaultNonEmpty
                                      )
import NonEmptyContainers.SeqConversions
                                    ( FromMonoSeq( fromSeq ), IsMonoSeq( seq )
                                    , ToMonoSeq( toSeq ) )
import NonEmptyContainers.SeqNE     ( SeqNE( (:⫸) ), (⫸), (⪪) )
import NonEmptyContainers.SeqNEConversions
                                    ( FromMonoSeqNonEmpty( fromSeqNE )
                                    , IsMonoSeqNonEmpty( seqNE )
                                    , ToMonoSeqNonEmpty( toSeqNE, toSeq_ ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char )
import Text.Parser.Combinators  ( endBy, endByNonEmpty )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( Text, last, empty, splitOn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Basename          ( Basename( basename, updateBasename ) )
import FPath.Dirname           ( HasDirname( dirname ) )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathComponentError
                               ( FPathComponentError( FPathComponentEmptyE
                                                    , FPathComponentIllegalCharE
                                                    ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError( FPathNotADirE
                                                         , FPathComponentE
                                                         , FPathEmptyE
                                                         , FPathNonAbsE
                                                         , FPathRootDirE
                                                         )
                               , __FPathComponentE__
                               , __FPathEmptyE__
                               , __FPathNonAbsE__
                               , __FPathNotADirE__
                               , __FPathRootDirE__
                               , mapTypeRepE
                               )
import FPath.Parent            ( HasParent( parent ), HasParentMay( parentMay ) )
import FPath.Parseable         ( Parseable( parse, parse', __parse'__ ) )
import FPath.PathComponent     ( PathComponent, parsePathC, pc, stub )
import FPath.RelDir            ( RelDir, reldir )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.Util              ( QuasiQuoter
                               , __ERROR'__, mkQuasiQuoterExp )

-------------------------------------------------------------------------------

{- | A non-root absolute directory, e.g., /etc. -}
-- a non-root dir is a path component appended to a (possibly-root) absolute
-- directory
newtype NonRootAbsDir = NonRootAbsDir (SeqNE PathComponent)
  deriving (Eq, Show)

type instance Element NonRootAbsDir = PathComponent

--------------------

instance DirTypeC AbsDir where
  type DirType AbsDir = AbsDir

--------------------

instance RelTypeC AbsDir where
  type RelType AbsDir = RelDir

--------------------

instance RelTypeC NonRootAbsDir where
  type RelType NonRootAbsDir = RelDir

--------------------

{- | An absolute directory is either the root directory, or a non-root absolute
     directory -}
data AbsDir = AbsRootDir | AbsNonRootDir NonRootAbsDir
  deriving (Eq, Show)

absNonRootDir ∷ SeqNE PathComponent → AbsDir
absNonRootDir = AbsNonRootDir ∘ NonRootAbsDir

type instance Element AbsDir = PathComponent

------------------------------------------------------------

class AsNonRootAbsDir α where
  _NonRootAbsDir ∷ Prism' α NonRootAbsDir

instance AsNonRootAbsDir NonRootAbsDir where
  _NonRootAbsDir = id

instance AsNonRootAbsDir AbsDir where
  _NonRootAbsDir =
    prism' AbsNonRootDir (\ case (AbsNonRootDir n) → Just n
                                 AbsRootDir        → Nothing)
--------------------

instance DirTypeC NonRootAbsDir where
  type DirType NonRootAbsDir = AbsDir

--------------------

class AsAbsDir α where
  _AbsDir ∷ Prism' α AbsDir

instance AsAbsDir AbsDir where
  _AbsDir = id

--------------------

class ToAbsDir α where
  toAbsDir ∷ α → AbsDir

instance ToAbsDir AbsDir where
  toAbsDir = id

instance ToAbsDir NonRootAbsDir where
  toAbsDir = AbsNonRootDir -- \ x → x ⊣ re _NonRootAbsDir

------------------------------------------------------------

instance MonoFunctor NonRootAbsDir where
  omap ∷ (PathComponent → PathComponent) → NonRootAbsDir → NonRootAbsDir
--  omap f (NonRootAbsDir p a) = NonRootAbsDir (f p) (omap f a)
  omap f (NonRootAbsDir ps) = NonRootAbsDir (omap f ps)

instance MonoFunctor AbsDir where
  omap ∷ (PathComponent → PathComponent) → AbsDir → AbsDir
  omap _ AbsRootDir = AbsRootDir
  omap f (AbsNonRootDir d) = AbsNonRootDir (omap f d)

nonRootAbsDir ∷ Prism' AbsDir NonRootAbsDir
nonRootAbsDir = prism' AbsNonRootDir go
                where go AbsRootDir        = Nothing
                      go (AbsNonRootDir d) = Just d

----------------------------------------

instance MonoFoldable AbsDir where
  otoList ∷ AbsDir → [PathComponent]
  otoList = toList
  ofoldl' ∷ (α → PathComponent → α) → α → AbsDir → α
  ofoldl' f x r = foldl' f x (toList r)

  ofoldr ∷ (PathComponent → α → α) → α → AbsDir → α
  ofoldr f x r = foldr f x (toList r)
  ofoldMap ∷ Monoid ν => (PathComponent → ν) → AbsDir → ν
  ofoldMap f r = foldMap f (toList r)
  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → AbsDir
            → PathComponent
  ofoldr1Ex f r = foldr1 f (toList r)
  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → AbsDir
             → PathComponent
  ofoldl1Ex' f r = foldl1 f (toList r)

----------------------------------------

instance MonoFoldable NonRootAbsDir where
  otoList ∷ NonRootAbsDir → [PathComponent]
  otoList = NonEmpty.toList ∘ toNonEmpty
  ofoldl' ∷ (α → PathComponent → α) → α → NonRootAbsDir → α
  ofoldl' f x r = foldl' f x (toNonEmpty r)

  ofoldr ∷ (PathComponent → α → α) → α → NonRootAbsDir → α
  ofoldr f x r = foldr f x (toNonEmpty r)
  ofoldMap ∷ Monoid ν => (PathComponent → ν) → NonRootAbsDir → ν
  ofoldMap f r = foldMap f (toNonEmpty r)
  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → NonRootAbsDir
            → PathComponent
  ofoldr1Ex f r = foldr1 f (toNonEmpty r)
  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → NonRootAbsDir
             → PathComponent
  ofoldl1Ex' f r = foldl1 f (toNonEmpty r)

----------------------------------------

instance FromMonoSeqNonEmpty NonRootAbsDir where
  fromSeqNE ∷ SeqNE PathComponent → NonRootAbsDir
  fromSeqNE = NonRootAbsDir

instance FromMonoSeqNonEmpty AbsDir where
  fromSeqNE = AbsNonRootDir ∘ fromSeqNE

----------------------------------------

instance ToMonoSeqNonEmpty NonRootAbsDir where
  toSeqNE (NonRootAbsDir ps) = ps

----------------------------------------

instance IsMonoSeqNonEmpty NonRootAbsDir where
  seqNE = iso toSeqNE fromSeqNE

----------------------------------------

instance FromMonoSeq AbsDir where
  fromSeq xs = case fromNullable xs of
                 Nothing → AbsRootDir
                 Just ps → fromSeqNE (toSeqNE ps)

----------------------------------------

instance ToMonoSeq AbsDir where
  toSeq AbsRootDir = (∅)
  toSeq (AbsNonRootDir n) = toSeq n

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

instance FromMonoNonEmpty NonRootAbsDir where
  fromNonEmpty = fromSeqNE ∘ fromNonEmpty

----------------------------------------

instance ToMonoNonEmpty NonRootAbsDir where
  toNonEmpty   = toNonEmpty ∘ toSeqNE

----------------------------------------

instance IsMonoNonEmpty NonRootAbsDir where
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

instance HasParent NonRootAbsDir where
  parent = lens getParent setParent
           where getParent ∷ NonRootAbsDir → AbsDir
                 getParent (SeqNE.unsnoc ∘ toSeqNE → (ps,_)) = fromSeq ps
                 setParent ∷ NonRootAbsDir → AbsDir → NonRootAbsDir
                 setParent (SeqNE.unsnoc ∘ toSeqNE → (_, p)) d =
                   fromSeqNE (toSeq d ⫸ p)

----------------------------------------

nonAbsRootGetParentMay ∷ NonRootAbsDir → Maybe AbsDir
nonAbsRootGetParentMay = Just ∘ view parent


nonAbsRootSetParentMay  ∷ NonRootAbsDir → Maybe AbsDir → NonRootAbsDir
nonAbsRootSetParentMay (SeqNE.unsnoc ∘ toSeqNE → (_,p)) d =
  fromSeqNE $ (maybe ф toSeq d) ⫸ p

instance HasParentMay AbsDir where
  parentMay = lens getParentMay setParentMay
              where getParentMay ∷ AbsDir → Maybe AbsDir
                    getParentMay AbsRootDir                          = Nothing
                    getParentMay (AbsNonRootDir n) = nonAbsRootGetParentMay n

                    setParentMay ∷ AbsDir → Maybe AbsDir → AbsDir
                    setParentMay AbsRootDir (Just d) = d
                    setParentMay AbsRootDir Nothing  = AbsRootDir
                    setParentMay (AbsNonRootDir n) d =
                      AbsNonRootDir $ nonAbsRootSetParentMay n d

----------------------------------------

instance HasParentMay NonRootAbsDir where
  parentMay = lens nonAbsRootGetParentMay nonAbsRootSetParentMay

------------------------------------------------------------
--                     Quasi-Quoting                      --
------------------------------------------------------------

absdirT ∷ TypeRep
absdirT = typeRep (Proxy ∷ Proxy AbsDir)

nrabsdirT ∷ TypeRep
nrabsdirT = typeRep (Proxy ∷ Proxy NonRootAbsDir)

instance Parseable AbsDir where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η AbsDir
  parse (toText → t) =
    let mkCompE ∷ (AsFPathError ε', MonadError ε' η')⇒FPathComponentError → η' α
        mkCompE ce = __FPathComponentE__ ce absdirT t
        eCompE ∷ (AsFPathError ε', MonadError ε' η') ⇒
                 Either FPathComponentError α → η' α
        eCompE = either mkCompE return
     in case unsnoc $ splitOn "/" t of
          Nothing            → error "error: splitOn always returns something"
          Just (("":xs), "") → case NonEmpty.nonEmpty xs of
                                 Nothing → return $ root
                                 Just ys → do
                                   ps ← eCompE $ mapM parsePathC ys
                                   return $ absNonRootDir (fromNonEmpty ps)

          Just ([],"")      → __FPathEmptyE__ absdirT
          Just (("":_), _)  → __FPathNotADirE__ absdirT t

          _                 → __FPathNonAbsE__ absdirT t

--------------------

parseAbsDirTests ∷ TestTree
parseAbsDirTests =
  let pamNUL      = "/etc/pam\0/"
      pamF        = "/etc/pam"
      illegalCE   = let fpcice = FPathComponentIllegalCharE '\0' "pam\0"
                     in FPathComponentE fpcice absdirT pamNUL
      emptyCompCE = FPathComponentE FPathComponentEmptyE absdirT "/etc//pam.d/"
      _parseAbsDir ∷ MonadError FPathError η ⇒ Text → η AbsDir
      _parseAbsDir = parse'
   in testGroup "parseAbsDir"
                [ testCase "root"  $ Right root   ≟ _parseAbsDir "/"
                , testCase "etc"   $ Right etc    ≟ _parseAbsDir "/etc/"
                , testCase "pam.d" $ Right pamd   ≟ _parseAbsDir "/etc/pam.d/"
                , testCase "wgm"   $ Right wgm    ≟ _parseAbsDir "/w/g/M/"
                , testCase "no trailing /" $
                      Left (FPathNotADirE absdirT pamF) ≟ _parseAbsDir pamF
                , testCase "empty" $
                      Left (FPathEmptyE absdirT)  ≟ _parseAbsDir ""
                , testCase "no leading /" $
                      Left (FPathNonAbsE absdirT "etc/") ≟ _parseAbsDir "etc/"
                , testCase "bad component" $
                      Left illegalCE ≟ _parseAbsDir pamNUL
                , testCase "empty component" $
                      Left emptyCompCE ≟ _parseAbsDir "/etc//pam.d/"
                ]

----------------------------------------

{- | Like `parseAbsDir`, but non-empty input that doesn't end in a '/' character
     has a '/' appended rather than failing as a non-file (thus, "permissive
     `parseAbsDir`" -}
parseAbsDirP ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η AbsDir
parseAbsDirP (toText → t) =
  let safeLast "" = Nothing
      safeLast s  = Just $ last s
   in case safeLast t of
        Nothing  → parse empty
        Just '/' → parse t
        _        → parse (t ⊕ "/")

parseAbsDirP' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η AbsDir
parseAbsDirP' = parseAbsDirP

__parseAbsDirP__ ∷ Printable τ ⇒ τ → AbsDir
__parseAbsDirP__ = either __ERROR'__ id ∘ parseAbsDirP'

__parseAbsDirP'__ ∷ String → AbsDir
__parseAbsDirP'__ = __parseAbsDirP__

--------------------

parseAbsDirPTests ∷ TestTree
parseAbsDirPTests =
  let pamNUL      = "/etc/pam\0/"
      illegalCE   = let fpcice = FPathComponentIllegalCharE '\0' "pam\0"
                     in FPathComponentE fpcice absdirT pamNUL
      emptyCompCE = FPathComponentE FPathComponentEmptyE absdirT "/etc//pam.d/"
      _parseAbsDirP ∷ MonadError FPathError η ⇒ Text → η AbsDir
      _parseAbsDirP = parseAbsDirP'
   in testGroup "parseAbsDirP"
                [ testCase "root"  $ Right root   ≟ _parseAbsDirP "/"
                , testCase "etc"   $ Right etc    ≟ _parseAbsDirP "/etc/"
                , testCase "etc"   $ Right etc    ≟ _parseAbsDirP "/etc"
                , testCase "pam.d" $ Right pamd   ≟ _parseAbsDirP "/etc/pam.d"
                , testCase "pam.d" $ Right pamd   ≟ _parseAbsDirP "/etc/pam.d/"
                , testCase "wgm"   $ Right wgm    ≟ _parseAbsDirP "/w/g/M/"
                , testCase "wgm"   $ Right wgm    ≟ _parseAbsDirP "/w/g/M"
                , testCase "empty" $
                      Left (FPathEmptyE absdirT)  ≟ _parseAbsDirP ""
                , testCase "no leading /" $
                      Left (FPathNonAbsE absdirT "etc/") ≟ _parseAbsDirP "etc/"
                , testCase "bad component" $
                      Left illegalCE ≟ _parseAbsDirP pamNUL
                , testCase "empty component" $
                      Left emptyCompCE ≟ _parseAbsDirP "/etc//pam.d/"
                ]

----------------------------------------

instance Parseable NonRootAbsDir where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η NonRootAbsDir
  parse t = do mapTypeRepE (const nrabsdirT) $ parse t ≫ \ case
                 AbsRootDir → __FPathRootDirE__ nrabsdirT
                 AbsNonRootDir d' → return d'

--------------------

parseAbsDirNTests ∷ TestTree
parseAbsDirNTests =
  let pamNUL      = "/etc/pam\0/"
      pamF        = "/etc/pam"
      illegalCE   = let fpcice = FPathComponentIllegalCharE '\0' "pam\0"
                     in FPathComponentE fpcice nrabsdirT pamNUL
      emptyCompCE = FPathComponentE FPathComponentEmptyE nrabsdirT "/etc//pam/"
      parseAbsDirN_ ∷ MonadError FPathError η ⇒ Text → η NonRootAbsDir
      parseAbsDirN_ = parse'
   in testGroup "parseAbsDirN"
                [ testCase "etc"   $ Right etcN    ≟ parseAbsDirN_ "/etc/"
                , testCase "pam.d" $ Right pamdN   ≟ parseAbsDirN_ "/etc/pam.d/"
                , testCase "wgm"   $ Right wgmN    ≟ parseAbsDirN_ "/w/g/M/"
                , testCase "root"  $
                      Left (FPathRootDirE nrabsdirT) ≟ parseAbsDirN_ "/"
                , testCase "no trailing /" $
                      Left (FPathNotADirE nrabsdirT pamF) ≟ parseAbsDirN_ pamF
                , testCase "empty" $
                      Left (FPathEmptyE nrabsdirT)  ≟ parseAbsDirN_ ""
                , testCase "no leading /" $
                      Left (FPathNonAbsE nrabsdirT "etc/") ≟ parseAbsDirN_ "etc/"
                , testCase "bad component" $
                      Left illegalCE ≟ parseAbsDirN_ pamNUL
                , testCase "empty component" $
                      Left emptyCompCE ≟ parseAbsDirN_ "/etc//pam/"
                ]

----------------------------------------

{- | Like `parseAbsDirN`, but non-empty input that doesn't end in a '/'
      character has a '/' appended rather than failing as a non-file (thus,
      "permissive `parseAbsDirN`" -}
parseAbsDirNP ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒
               τ → η NonRootAbsDir
parseAbsDirNP (toText → t) = do
  let safeLast "" = Nothing
      safeLast s  = Just $ last s
   in case safeLast t of
        Nothing  → parse empty
        Just '/' → parse t
        _        → parse (t ⊕ "/")

--------------------

parseAbsDirNP' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η NonRootAbsDir
parseAbsDirNP' = parseAbsDirNP

--------------------

__parseAbsDirNP__ ∷ String → NonRootAbsDir
__parseAbsDirNP__ s = case parse' s of
                       Left e → __ERROR'__ e
                       Right AbsRootDir → __ERROR'__ $ FPathRootDirE nrabsdirT
                       Right (AbsNonRootDir nr) → nr

__parseAbsDirNP'__ ∷ String → NonRootAbsDir
__parseAbsDirNP'__ = __parseAbsDirNP__

--------------------

parseAbsDirNPTests ∷ TestTree
parseAbsDirNPTests =
  let pamNUL      = "/etc/pam\0/"
      illegalCE   = let fpcice = FPathComponentIllegalCharE '\0' "pam\0"
                     in FPathComponentE fpcice nrabsdirT pamNUL
      emptyCompCE = FPathComponentE FPathComponentEmptyE nrabsdirT "/etc//pam/"
      parseAbsDirNP_ ∷ MonadError FPathError η ⇒ Text → η NonRootAbsDir
      parseAbsDirNP_ = parseAbsDirNP'
   in testGroup "parseAbsDirNP"
                [ testCase "etc"   $ Right etcN  ≟ parseAbsDirNP_ "/etc"
                , testCase "pam.d" $ Right pamdN ≟ parseAbsDirNP_ "/etc/pam.d"
                , testCase "wgm"   $ Right wgmN  ≟ parseAbsDirNP_ "/w/g/M"
                , testCase "root"  $
                      Left (FPathRootDirE nrabsdirT) ≟ parseAbsDirNP_ "/"
                , testCase "empty" $
                      Left (FPathEmptyE nrabsdirT)  ≟ parseAbsDirNP_ ""
                , testCase "no leading /" $
                        Left (FPathNonAbsE nrabsdirT "etc/")
                      ≟ parseAbsDirNP_ "etc/"
                , testCase "bad component" $
                      Left illegalCE ≟ parseAbsDirNP_ pamNUL
                , testCase "empty component" $
                      Left emptyCompCE ≟ parseAbsDirNP_ "/etc//pam/"
                ]

----------------------------------------

{- | quasi-quoter for AbsDir -}
absdir ∷ QuasiQuoter
absdir = mkQuasiQuoterExp "absdir" (\ s → ⟦ __parse'__ @AbsDir s ⟧)

{- | quasi-quoter for NonRootAbsDir -}

absdirN ∷ QuasiQuoter
absdirN = mkQuasiQuoterExp "absdirN" (\ s → ⟦ __parse'__ @NonRootAbsDir s ⟧)

----------------------------------------

instance HasDirname AbsDir where
  dirname ∷ Lens' AbsDir AbsDir
  dirname = lens (\ case AbsRootDir → AbsRootDir; AbsNonRootDir d → d ⊣ dirname)
                 (\ o d → case o of
                            AbsRootDir → d
                            AbsNonRootDir n → AbsNonRootDir $ n & parent ⊢ d
                 )

absDirDirnameTests ∷ TestTree
absDirDirnameTests =
  testGroup "AbsDir"
            [ testCase "root"  $ root ≟ root ⊣ dirname
            , testCase "pam.d" $ etc  ≟ pamd ⊣ dirname

            , testCase "root  -> etc"     $ etc ≟ root ⅋ dirname ⊢ etc
            , testCase "etc   -> etc"     $ etc ≟ etc ⅋ dirname ⊢ root
            , testCase "pam.d -> pam.d"   $ pamd ≟ pamd ⅋ dirname ⊢ etc
            , testCase "pam.d -> /pam.d"  $
                fromList [[pc|pam.d|]] ≟ pamd ⅋ dirname ⊢ root
            , testCase "/pam.d -> pam.d"  $
                pamd ≟ fromList [[pc|pam.d|]] ⅋ dirname ⊢ etc
            ]

--------------------

instance HasDirname NonRootAbsDir where
  dirname ∷ Lens' NonRootAbsDir AbsDir
  dirname = parent

nonRootAbsDirDirnameTests ∷ TestTree
nonRootAbsDirDirnameTests =
  testGroup "NonRootAbsDir"
            [ testCase "etcN"  $ root ≟ etcN ⊣ dirname
            , testCase "wgmN"  $ fromList [[pc|w|],[pc|g|]] ≟ wgmN ⊣ dirname

            , testCase "etc   -> etc"     $ etcN ≟ etcN ⅋ dirname ⊢ root
            , testCase "pam.d -> pam.d"   $ pamdN ≟ pamdN ⅋ dirname ⊢ etc
            , testCase "pam.d -> /pam.d"  $
                fromNonEmpty (pure [pc|pam.d|]) ≟ pamdN ⅋ dirname ⊢ root
            , testCase "/pam.d -> pam.d"  $
                pamdN ≟ fromNonEmpty (pure [pc|pam.d|]) ⅋ dirname ⊢ etc
            ]

dirnameTests ∷ TestTree
dirnameTests = testGroup "dirname" [ absDirDirnameTests
                                   , nonRootAbsDirDirnameTests ]

----------------------------------------

instance Basename AbsDir where
  basename ∷ AbsDir → RelDir
  basename AbsRootDir = fromList []
  basename (AbsNonRootDir n) = basename n

  updateBasename ∷ (PathComponent → PathComponent) → AbsDir → AbsDir
  updateBasename _ AbsRootDir = AbsRootDir
  updateBasename f (AbsNonRootDir n) = AbsNonRootDir (updateBasename f n)

absDirBasenameTests ∷ TestTree
absDirBasenameTests =
  testGroup "AbsDir"
            [ testCase "root"  $ [reldir|./|] ≟ basename root
            , testCase "pam.d" $ [reldir|pam.d/|]  ≟ basename pamd

            , testCase "root  -> root"    $
                root ≟ updateBasename (const [pc|etc|]) root
            , testCase "etc   -> etc"     $
                etc ≟ updateBasename (const [pc|etc|]) etc
            , testCase "pam.d -> pam.d"   $
                pamd ≟ updateBasename (const [pc|pam.d|]) pamd
            , testCase "pam.d -> pam.d"   $
                pamd ≟ updateBasename (const [pc|pam.d|])
                                      (fromList [[pc|etc|], [pc|pam|]])
            ]

--------------------

instance Basename NonRootAbsDir where
  basename ∷ NonRootAbsDir → RelDir
  basename (NonRootAbsDir (_ :⫸ p)) = fromList [p]
  basename _ = error "FPath.AbsDir.basename irrefutable pattern"

  updateBasename ∷ (PathComponent → PathComponent) → NonRootAbsDir
                 → NonRootAbsDir
  updateBasename f (NonRootAbsDir (ps :⫸ p)) = NonRootAbsDir (ps :⫸ f p)
  updateBasename _ _ = error "FPath.AbsDir.updateBasename irrefutable pattern"

nonRootAbsDirBasenameTests ∷ TestTree
nonRootAbsDirBasenameTests =
  let pam = fromNonEmpty $ [pc|etc|] :| [[pc|pam|]]
   in testGroup "NonRootAbsDir"
                [ testCase "etcN"  $ [reldir|etc/|] ≟ basename etcN
                , testCase "wgmN"  $ [reldir|M/|]   ≟ basename wgmN

                , testCase "etc   -> etc"   $
                    etcN  ≟ updateBasename (const [pc|etc|]) etcN
                , testCase "pam.d -> pam" $ pam ≟ updateBasename stub pamdN
                , testCase "/pam.d -> etc"  $
                    etcN ≟ updateBasename
                             (\ p → if p ≡ [pc|pam.d|] then [pc|etc|] else p)
                             (fromNonEmpty $ pure [pc|pam.d|])
                ]

--------------------

basenameTests ∷ TestTree
basenameTests = testGroup "basename" [ absDirBasenameTests
                                     , nonRootAbsDirBasenameTests ]

----------------------------------------
--             constants              --
----------------------------------------

root ∷ AbsDir
root = AbsRootDir

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

etc ∷ AbsDir
etc = fromSeq $ pure [pc|etc|]

pamd ∷ AbsDir
pamd = fromSeq $ [pc|etc|] ⪪ pure [pc|pam.d|]

wgm ∷ AbsDir
wgm = fromSeq $ [pc|w|] ⪪ [pc|g|] ⪪ pure [pc|M|]

etcN ∷ NonRootAbsDir
etcN = fromSeqNE $ pure [pc|etc|]

pamdN ∷ NonRootAbsDir
pamdN = fromSeqNE $ [pc|etc|] ⪪ pure [pc|pam.d|]

wgmN ∷ NonRootAbsDir
wgmN = fromSeqNE $ [pc|w|] ⪪ [pc|g|] ⪪ pure [pc|M|]

----------------------------------------

constructionTests ∷ TestTree
constructionTests = testGroup "construction" [ parseAbsDirTests
                                             , parseAbsDirNTests
                                             , parseAbsDirPTests
                                             , parseAbsDirNPTests
                                             ]

tests ∷ TestTree
tests = testGroup "FPath.AbsDir" [ constructionTests, dirnameTests
                                 , basenameTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
