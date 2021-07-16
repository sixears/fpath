module FPath.AbsFile
  ( AbsDir, AbsFile, AsAbsFile( _AbsFile )

  , absfile, absfileT

  , tests
  )
where

import Prelude  ( error )

-- base --------------------------------

import qualified  Data.List.NonEmpty  as  NonEmpty

import Control.Applicative  ( pure )
import Control.Monad        ( return )
import Data.Either          ( Either, either )
import Data.Eq              ( Eq )
import Data.Foldable        ( foldMap, foldl1, foldl', foldr, foldr1 )
import Data.Function        ( ($), (&), const, id )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Monoid          ( Monoid )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Exts             ( IsList( fromList, toList ) )
import GHC.Generics         ( Generic )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-default ------------------------

import Data.Default  ( def )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- lens --------------------------------

import Control.Lens.Cons   ( unsnoc )
import Control.Lens.Iso    ( iso )
import Control.Lens.Lens   ( Lens', lens )
import Control.Lens.Prism  ( Prism', prism' )

-- monaderror-io -----------------------

import MonadError  ( ѭ )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFoldable( ofoldl', ofoldl1Ex'
                                                    , ofoldMap, ofoldr
                                                    , ofoldr1Ex, otoList )
                             , MonoFunctor( omap )
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋫) )
import Data.MoreUnicode.Function     ( (⅋) )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊣), (⊩), (⊢) )
import Data.MoreUnicode.Monoid       ( ф )
import Data.MoreUnicode.Natural      ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import NonEmptyContainers.IsNonEmpty        ( FromMonoNonEmpty( fromNonEmpty )
                                            , IsMonoNonEmpty( nonEmpty )
                                            , ToMonoNonEmpty( toNonEmpty ) )
import NonEmptyContainers.SeqConversions    ( FromSeq( fromSeq )
                                            , ToSeq( toSeq )
                                            )
import NonEmptyContainers.SeqNE             ( pattern (:⪭), (⪭), (⋖) )
import NonEmptyContainers.SeqNEConversions  ( FromSeqNonEmpty( fromSeqNE )
                                            , IsSeqNonEmpty( seqNE )
                                            , ToSeqNonEmpty( toSeqNE
                                                               , toSeq_ )
                                            )

-- parsers -----------------------------

import Text.Parser.Char         ( char )
import Text.Parser.Combinators  ( sepByNonEmpty )

-- quasiquoting ------------------------

import QuasiQuoting  ( QuasiQuoter, mkQQ, exp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertListEq, runTestsP, runTestsReplay, runTestTree )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( Text, dropEnd, intercalate, length, splitOn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir            ( AbsDir, absdir, root )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.AsFilePath'       ( AsFilePath'( filepath' ) )
import FPath.Basename          ( Basename( basename, updateBasename ) )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathComponentError
                               ( FPathComponentError )
import FPath.Error.FPathError  ( AsFPathError, FPathError
                               , __FPathComponentE__, __FPathEmptyE__
                               , __FPathNonAbsE__, __FPathNotAFileE__
                               , mapTypeRepE, mapTextE
                               )
import FPath.FileLike          ( FileLike( dirfile ) )
import FPath.Dirname           ( Ancestors( ancestors )
                               , HasDirname( ancestors', dirname ) )
import FPath.Parent            ( HasParent( parent )
                               , HasParentMay( parentMay, parents ) )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( PathComponent, parsePathC, pc )
import FPath.RelFile           ( RelFile, relfile )
import FPath.RelType           ( RelTypeC( RelType ) )

-------------------------------------------------------------------------------

{- | an absolute file -}
data AbsFile = AbsFile AbsDir PathComponent
  deriving (Eq,Generic,Lift,NFData)

type instance Element AbsFile = PathComponent

--------------------

instance Show AbsFile where
  show r = [fmt|[absfile|%T%s]|] (toText r) "|"

--------------------

class AsAbsFile α where
  _AbsFile ∷ Prism' α AbsFile

instance AsAbsFile AbsFile where
  _AbsFile = id

--------------------

instance DirTypeC AbsFile where
  type DirType AbsFile = AbsDir

--------------------

instance RelTypeC AbsFile where
  type RelType AbsFile = RelFile

----------------------------------------

instance FileLike AbsFile where
  dirfile = iso (\ (AbsFile d f) → (d,f)) (\ (d,f) → AbsFile d f)

----------------------------------------

instance MonoFunctor AbsFile where
  omap ∷ (PathComponent → PathComponent) → AbsFile → AbsFile
  omap f (AbsFile ps b) = AbsFile (omap f ps) (f b)

----------------------------------------

instance MonoFoldable AbsFile where
  otoList ∷ AbsFile → [PathComponent]
  otoList (AbsFile ps f) = otoList ps ⊕ [f]

  ofoldl' ∷ (α → PathComponent → α) → α → AbsFile → α
  ofoldl' f x r = foldl' f x (toNonEmpty r)

  ofoldr ∷ (PathComponent → α → α) → α → AbsFile → α
  ofoldr f x r = foldr f x (toNonEmpty r)

  ofoldMap ∷ Monoid ν => (PathComponent → ν) → AbsFile → ν
  ofoldMap f r = foldMap f (toNonEmpty r)

  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → AbsFile
            → PathComponent
  ofoldr1Ex f r = foldr1 f (toNonEmpty r)

  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → AbsFile
             → PathComponent
  ofoldl1Ex' f r = foldl1 f (toNonEmpty r)

----------------------------------------

instance FromSeqNonEmpty AbsFile where
  fromSeqNE (ps :⪭ f) = AbsFile (fromSeq ps) f
  fromSeqNE _         = error "AbsFile.fromSeqNE pattern match can't get here"

----------------------------------------

instance ToSeqNonEmpty AbsFile where
  toSeqNE (AbsFile ps f) = toSeq ps ⪭ f

instance ToSeq AbsFile where
  toSeq = toSeq_

----------------------------------------

instance IsSeqNonEmpty AbsFile where
  seqNE = iso toSeqNE fromSeqNE

----------------------------------------

instance FromMonoNonEmpty AbsFile where
  fromNonEmpty (x :| xs) = case NonEmpty.nonEmpty xs of
                             Nothing  → AbsFile (fromList ф) x
                             Just xs' → let pcs = x : NonEmpty.init xs'
                                            f   = NonEmpty.last xs'
                                         in AbsFile (fromList pcs) f

instance ToMonoNonEmpty AbsFile where
  toNonEmpty (AbsFile ps f) = NonEmpty.fromList $ toList ps ⊕ [f]

instance IsMonoNonEmpty AbsFile where
  nonEmpty = iso toNonEmpty fromNonEmpty

----------------------------------------

instance Printable AbsFile where
  print r = P.text $ "/" ⊕ intercalate "/" (toText ⊳ otoList r)

----------------------------------------

instance AsFilePath AbsFile where
  filepath = prism' toString fromString

instance AsFilePath' AbsFile where
  filepath' = filepath

----------------------------------------

instance Textual AbsFile where
  textual = fromNonEmpty ⊳ (char '/' ⋫ sepByNonEmpty textual (char '/'))

----------------------------------------

instance Arbitrary AbsFile where
  arbitrary = fromSeqNE ⊳ arbitrary
  shrink = fromSeqNE ⩺ shrink ∘ toSeqNE

----------------------------------------

instance HasParent AbsFile where
  parent = lens (\ (AbsFile p _) → p) (\ (AbsFile _ f) p → AbsFile p f)

----------------------------------------

instance HasParentMay AbsFile where
  parentMay = lens (\ (AbsFile p _) → Just p)
                   (\ (AbsFile _ f) md → case md of
                                           Just  d → AbsFile d f
                                           Nothing → AbsFile root f
                   )

----------

parentsTests ∷ TestTree
parentsTests =
  let check t d ps = assertListEq t ps (parents d)
   in testGroup "parents" $
        [ check "/r.e"       af1 [root]
        , check "/r/p.x"     af2 [root,fromSeq (pure [pc|r|])]
        , check "/p/q/r.mp3" af3 [root,[absdir|/p/|], [absdir|/p/q/|]]
        , check "/.x"        af4 [root]
        ]

----------------------------------------

instance Basename AbsFile where
  basename ∷ AbsFile → RelFile
  basename (AbsFile _ n) = fromNonEmpty (pure n)

  updateBasename ∷ (PathComponent → PathComponent) → AbsFile → AbsFile
  updateBasename f (AbsFile d n) = AbsFile d (f n)

basenameTests ∷ TestTree
basenameTests =
  testGroup "basename"
            [ testCase "af1" $ [relfile|r.e|]  ≟ basename af1

            , testCase "af1 -> af4"    $
                af4 ≟ updateBasename (const [pc|.x|]) af1
            , testCase "af2 -> x.p"     $
                  fromSeqNE ([pc|r|] ⋖ [[pc|x.p|]])
                ≟ updateBasename (const [pc|x.p|]) af2
            , testCase "pam.d -> pam.d"   $
                  fromSeqNE ([pc|p|] ⋖ [[pc|q|], [pc|r|]])
                ≟ updateBasename (const [pc|r|]) af3
            , testCase "af4 -> af1"   $
                af1 ≟ updateBasename (const [pc|r.e|]) af4
            ]

----------------------------------------

instance Ancestors AbsFile where
  ancestors ∷ AbsFile → NonEmpty AbsDir
  ancestors fp = let d = fp ⊣ dirname
                  in d :| ancestors' d

ancestorsTests ∷ TestTree
ancestorsTests =
  testGroup "ancestors"
            [ testCase "af1" $ pure root  @=? ancestors af1
            , testCase "af2"  $
                ([absdir|/r/|] :| [root]) @=? ancestors af2
            , testCase "af3" $
                ([absdir|/p/q/|] :| [[absdir|/p/|], root]) @=? ancestors af3
            , testCase "af4"  $ pure root @=? ancestors af4
            ]

instance HasDirname AbsFile where
  dirname ∷ Lens' AbsFile AbsDir
  dirname = lens (\ (AbsFile d _) → d) (\ (AbsFile _ f) d → AbsFile d f)

  ancestors' ∷ AbsFile → [AbsDir]
  ancestors' = toList ∘ ancestors

dirnameTests ∷ TestTree
dirnameTests =
  testGroup "dirname"
            [ testCase "af1" $ [absdir|/|]     ≟ af1 ⊣ dirname
            , testCase "af2" $ [absdir|/r/|]   ≟ af2 ⊣ dirname
            , testCase "af3" $ [absdir|/p/q/|] ≟ af3 ⊣ dirname
            , testCase "af4" $ [absdir|/|]     ≟ af4 ⊣ dirname

            , testCase "af1" $ af1 ≟ af1 ⅋ dirname ⊢ [absdir|/|]
            , testCase "af1 ← /r/" $
                fromSeqNE ([pc|r|]⋖[[pc|r.e|]]) ≟ af1 ⅋ dirname ⊢ [absdir|/r/|]
            , testCase "af3 ← /" $
                fromSeqNE ([pc|r.mp3|]⋖[]) ≟ af3 ⅋ dirname ⊢ [absdir|/|]
            , testCase "af3 ← /r/" $
                fromSeqNE ([pc|r|]⋖[[pc|r.mp3|]]) ≟ af3 ⅋ dirname ⊢ [absdir|/r/|]
            ]

ancestors'Tests ∷ TestTree
ancestors'Tests =
  testGroup "ancestors'"
            [ testCase "af1" $ [root]  @=? ancestors' af1
            , testCase "af2"  $
                [[absdir|/r/|],root] @=? ancestors' af2
            , testCase "af3" $
                [[absdir|/p/q/|],[absdir|/p/|],root]  @=? ancestors' af3
            , testCase "af4"  $ [root] @=? ancestors' af4
            ]

------------------------------------------------------------
--                     Quasi-Quoting                      --
------------------------------------------------------------

absfileT ∷ TypeRep
absfileT = typeRep (Proxy ∷ Proxy AbsFile)

{- | try to parse a `Textual` as an absolute file -}
instance Parseable AbsFile where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η AbsFile
  parse (toText → t) =
    let mkCompE    ∷ (AsFPathError ε', MonadError ε' η') ⇒
                     FPathComponentError → η' α
        mkCompE ce = __FPathComponentE__ ce absfileT t
        eCompE     ∷ (AsFPathError ε'', MonadError ε'' η'') ⇒
                     Either FPathComponentError α → η'' α
        eCompE = either mkCompE return
        -- map the type marker on FPathError
        mapFPCE ∷ (AsFPathError ε', MonadError ε' η') ⇒
                  (Text → Text) → Either FPathError α → η' α
        mapFPCE f = mapTextE f ∘ mapTypeRepE (const absfileT)
     in case unsnoc $ splitOn "/" t of
          Nothing           → error "error: splitOn always returns something"
          Just ([],"")      → __FPathEmptyE__ absfileT
          Just (_, "")      → __FPathNotAFileE__ absfileT t
          Just ([""], f)    → do f' ← eCompE $ parsePathC f
                                 return $ AbsFile (fromSeq ф) f'
          Just (("":_), f)  → do f' ← eCompE $ parsePathC f
                                 ps' ← mapFPCE (⊕ f) $
                                         parse (dropEnd (length f) t)
                                 return $ AbsFile ps' f'
          Just (_, _)      → __FPathNonAbsE__ absfileT t

{- | quasi-quotation -}
absfileQQ ∷ String → Maybe ExpQ
absfileQQ = (\ d → ⟦d⟧) ⩺ (ѭ ∘ parse @AbsFile @FPathError)

absfile ∷ QuasiQuoter
absfile = mkQQ "AbsFile" $ def & exp ⊩ absfileQQ

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

af1 ∷ AbsFile
af1 = fromSeqNE $ pure [pc|r.e|]

af2 ∷ AbsFile
af2 = fromSeqNE $ [pc|r|] ⋖ [[pc|p.x|]]

af3 ∷ AbsFile
af3 = fromSeqNE $ [pc|p|] ⋖ [[pc|q|], [pc|r.mp3|]]

af4 ∷ AbsFile
af4 = fromSeqNE $ pure [pc|.x|]

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath.AbsFile" [ basenameTests, dirnameTests, parentsTests
                                  , ancestorsTests, ancestors'Tests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
