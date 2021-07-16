{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

module FPath.RelDir
  ( AsRelDir( _RelDir ), RelDir

  , reldir, reldirT
  , parseRelDirP

  , tests
  )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( mapM, return )
import Data.Bool            ( otherwise )
import Data.Either          ( Either( Left, Right ), either )
import Data.Eq              ( Eq )
import Data.Foldable        ( concat, foldl', foldl1, foldMap, foldr, foldr1 )
import Data.Function        ( (&), ($), const, id )
import Data.Functor         ( fmap )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Monoid          ( Monoid )
import Data.Semigroup       ( Semigroup )
import Data.String          ( String )
import Data.Typeable        ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Exts             ( IsList( fromList, toList ), Item )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

import Data.Sequence  ( Seq( Empty, (:|>) ), (<|) )

-- data-default ------------------------

import Data.Default  ( def )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

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

import Data.MoreUnicode.Applicative  ( (∤), (⋪) )
import Data.MoreUnicode.Function     ( (⅋) )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊣), (⊩), (⊢) )
import Data.MoreUnicode.Monoid       ( ф )
import Data.MoreUnicode.Natural      ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import qualified  NonEmptyContainers.SeqNE  as  SeqNE

import NonEmptyContainers.SeqConversions    ( FromSeq( fromSeq )
                                            , IsSeq( seq )
                                            , ToSeq( toSeq ) )
import NonEmptyContainers.SeqNE             ( pattern(:⪭), (⪫), (⋖) )
import NonEmptyContainers.SeqNEConversions  ( FromSeqNonEmpty( fromSeqNE ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char, string )
import Text.Parser.Combinators  ( endBy )

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
import Language.Haskell.TH.Syntax  ( Exp( AppE, ConE, VarE )
                                   , Lift( lift, liftTyped ), TExp( TExp ) )

-- text --------------------------------

import Data.Text  ( Text, empty, last, splitOn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath   ( AsFilePath( filepath ) )
import FPath.AsFilePath'  ( AsFilePath'( filepath' ), exterminate, terminate )
import FPath.Basename     ( Basename( basename, updateBasename ) )
import FPath.Dirname      ( HasDirname( ancestors', dirname ) )
import FPath.DirType      ( DirTypeC( DirType ) )

import FPath.Error.FPathComponentError
                               ( FPathComponentError
                               , fPathComponentEmptyE
                               , fPathComponentIllegalCharE
                               )
import FPath.Error.FPathError  ( AsFPathError, FPathError( FPathComponentE )
                               , __FPathComponentE__, __FPathEmptyE__
                               , __FPathAbsE__, __FPathNotADirE__
                               , _FPathComponentE
                               , fPathAbsE, fPathEmptyE
                               , fPathNotADirE
                               )
import FPath.Parent            ( HasParentMay( parentMay, parents ) )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( PathComponent, parsePathC, pc, toUpper )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.Util              ( __ERROR'__ )

-------------------------------------------------------------------------------

{- | a relative directory -}
newtype RelDir = RelDir (Seq PathComponent)
  deriving newtype (Eq, Monoid, Semigroup)

instance Show RelDir where
  show r = [fmt|[reldir|%T%s]|] (toText r) "|"

instance Lift RelDir where
  liftTyped (RelDir ps) = do
    xs ← lift $ toList ps
    return ∘ TExp $ AppE (ConE 'RelDir) (AppE (VarE 'fromList) xs)

type instance Element RelDir = PathComponent

--------------------

class AsRelDir α where
  _RelDir ∷ Prism' α RelDir

instance AsRelDir RelDir where
  _RelDir = id

--------------------

instance DirTypeC RelDir where
  type DirType RelDir = RelDir

--------------------

instance RelTypeC RelDir where
  type RelType RelDir = RelDir

------------------------------------------------------------

instance MonoFunctor RelDir where
  omap ∷ (PathComponent → PathComponent) → RelDir → RelDir
  omap f (RelDir ps) = RelDir (omap f ps)

----------------------------------------

instance MonoFoldable RelDir where
  otoList ∷ RelDir → [PathComponent]
  otoList (RelDir ps) = toList ps
  ofoldl' ∷ (α → PathComponent → α) → α → RelDir → α
  ofoldl' f x r = foldl' f x (toList r)

  ofoldr ∷ (PathComponent → α → α) → α → RelDir → α
  ofoldr f x r = foldr f x (toList r)
  ofoldMap ∷ Monoid ν => (PathComponent → ν) → RelDir → ν
  ofoldMap f r = foldMap f (toList r)
  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → RelDir
            → PathComponent
  ofoldr1Ex f r = foldr1 f (toList r)
  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → RelDir
             → PathComponent
  ofoldl1Ex' f r = foldl1 f (toList r)

----------------------------------------

instance FromSeqNonEmpty RelDir where
  fromSeqNE = RelDir ∘ SeqNE.toSeq

----------------------------------------

instance FromSeq RelDir where
  fromSeq = RelDir

----------------------------------------

instance ToSeq RelDir where
  toSeq (RelDir ps) = ps

----------------------------------------

instance IsSeq RelDir where
  seq = iso toSeq fromSeq

----------------------------------------

instance IsList RelDir where
  type instance Item RelDir = PathComponent
  fromList = RelDir ∘ Seq.fromList
  toList   = toList ∘ toSeq

----------------------------------------

{- | Convert a sequence of printable elements to strings by intercalating '/'
     characters; with a post-fact string transformation for extra bells (e.g.,
     a prefix '/' character -}
pDir ∷ (P.Printer ρ, ToSeq α, Printable (Element α)) ⇒ (String → String) → α → ρ
pDir f =  P.string ∘ f ∘ concat ∘ fmap ((⊕ "/") ∘ toString) ∘ toSeq

instance Printable RelDir where
  print (RelDir ps) | ps ≡ ф = "./"
                    | otherwise = pDir id ps

----------------------------------------

instance AsFilePath RelDir where
  filepath = prism' toString fromString

--------------------

instance AsFilePath' RelDir where
  filepath' = prism' (exterminate ∘ toString)
                     (fromString ∘ terminate)

----------------------------------------

instance Textual RelDir where
  textual = return (fromList []) ⋪ (string "./")
          ∤ fromList ⊳ (endBy textual (char '/'))

----------------------------------------

instance Arbitrary RelDir where
  arbitrary = fromSeq ⊳ arbitrary
  shrink = fromSeq ⩺ shrink ∘ toSeq

----------------------------------------

instance HasParentMay RelDir where
  parentMay = lens getParentMay setParentMay
              where getParentMay ∷ RelDir → Maybe RelDir
                    getParentMay (RelDir ps) = case unsnoc ps of
                                                 Just (p,_) → Just $ RelDir p
                                                 Nothing → Nothing

                    setParentMay (RelDir ps) par =
                      case unsnoc ps of
                        Just (_, d) → case par of
                                        Just (RelDir p) → RelDir $ p ⪫ d
                                        Nothing         → RelDir $ pure d
                        Nothing     → case par of
                                        Just r → r
                                        Nothing → RelDir Seq.Empty

----------

parentsTests ∷ TestTree
parentsTests =
  let check t d ps = assertListEq t ps (parents d)
   in testGroup "parents" $
        [ check "./"     r0 []
        , check "r/"     r1 [ fromSeq ф ]
        , check "r/p/"   r2 [ fromSeq ф, fromSeq (pure [pc|r|]) ]
        , check "p/q/r/" r3 [ fromSeq ф, fromSeq (pure [pc|p|])
                            , fromSeqNE $ [pc|p|] ⋖ [[pc|q|]] ]
        ]

----------------------------------------

instance Basename RelDir where
  basename ∷ RelDir → RelDir
  basename (RelDir (_ Seq.:|> p)) = fromList [p]
  basename r@(RelDir Seq.Empty) = r

  updateBasename ∷ (PathComponent → PathComponent) → RelDir → RelDir
  updateBasename f (RelDir (ps Seq.:|> p)) = RelDir (ps :⪭ f p)
  updateBasename _ r@(RelDir Seq.Empty) = r

basenameTests ∷ TestTree
basenameTests =
  testGroup "basename"
    [ testGroup "basename"
            [ testCase "r0 (./) → ./"  $ fromList [] ≟ basename r0
            , testCase "r2 (./r/p) → p" $ fromList [[pc|p|]]  ≟ basename r2
            ]

    , testGroup "updateBasename"
        [ testCase "r0 (./) -> r0 (./)"    $
              r0 ≟ updateBasename (const [pc|r1|]) r0
        , testCase "r1 (r/) + r -> r1 (r/)"     $
            r1 ≟ updateBasename (const [pc|r|]) r1
        , testCase "r2 (./r/p) + p -> r2 (./r/p)"     $
            r2 ≟ updateBasename (const [pc|p|]) r2
        , testCase "r2 (./r/p) + q -> r/q"     $
            fromList [[pc|r|],[pc|q|]] ≟ updateBasename (const [pc|q|]) r2
        , testCase "r2 (./r/p) × toUpper -> r/P"   $
            fromList [[pc|r|],[pc|P|]] ≟ updateBasename toUpper r2
        ]
    ]

----------------------------------------

instance HasDirname RelDir where
  dirname ∷ Lens' RelDir RelDir
  dirname = lens (\ case RelDir Empty     → RelDir Empty
                         RelDir (s :|> _) → RelDir s)
                 (\ r (RelDir d) → case r of
                                     RelDir Empty     → RelDir d
                                     RelDir (_ :|> a) → RelDir (d :|> a)
                 )

  ancestors' ∷ RelDir → [RelDir]
  ancestors' (RelDir Empty) = []
  ancestors' fp             = (fp ⊣ dirname) : ancestors' (fp ⊣ dirname)

dirnameTests ∷ TestTree
dirnameTests =
  testGroup "dirname"
            [ testCase "r0" $ r0 ≟ r0 ⊣ dirname
            , testCase "r1" $ r0 ≟ r1 ⊣ dirname
            , testCase "r2" $ r1 ≟ r2 ⊣ dirname
            , testCase "r3" $ fromList [[pc|p|],[pc|q|]] ≟ r3 ⊣ dirname

            , testCase "r0 -> r0" $ r0 ≟ r0 ⅋ dirname ⊢ r0
            , testCase "r0 -> r1" $ r1 ≟ r0 ⅋ dirname ⊢ r1
            , testCase "r1 -> r0" $ r1 ≟ r1 ⅋ dirname ⊢ r0
            , testCase "r1 -> r1" $ fromList [[pc|r|],[pc|r|]] ≟
                                                r1 ⅋ dirname ⊢ r1

            , testCase "r0 -> r2" $ r2 ≟ r0 ⅋ dirname ⊢ r2
            , testCase "r2 -> r0" $ fromList [[pc|p|]] ≟ r2 ⅋ dirname ⊢ r0
            , testCase "r1 -> r2" $ fromList [[pc|r|],[pc|p|],[pc|r|]] ≟
                                                r1 ⅋ dirname ⊢ r2
            , testCase "r2 -> r1" $ r2 ≟ r2 ⅋ dirname ⊢ r1
            , testCase "r2 -> r2" $ fromList [[pc|r|],[pc|p|],[pc|p|]] ≟
                                    r2 ⅋ dirname ⊢ r2

            , testCase "r0 -> r3" $ r3 ≟ r0 ⅋ dirname ⊢ r3
            , testCase "r3 -> r0" $ fromList [[pc|r|]] ≟ r3 ⅋ dirname ⊢ r0
            , testCase "r1 -> r3" $ fromList [[pc|p|],[pc|q|],[pc|r|],[pc|r|]] ≟
                                    r1 ⅋ dirname ⊢ r3
            , testCase "r3 -> r1" $ fromList [[pc|r|],[pc|r|]] ≟
                                    r3 ⅋ dirname ⊢ r1
            , testCase "r2 -> r3" $ fromList [[pc|p|],[pc|q|],[pc|r|],[pc|p|]] ≟
                                    r2 ⅋ dirname ⊢ r3
            , testCase "r3 -> r2" $ fromList [[pc|r|],[pc|p|],[pc|r|]] ≟
                                    r3 ⅋ dirname ⊢ r2
            , testCase "r3 -> r3" $ fromList [[pc|p|],[pc|q|],[pc|r|],[pc|r|]] ≟
                                    r3 ⅋ dirname ⊢ r3
            ]

ancestors'Tests ∷ TestTree
ancestors'Tests =
  testGroup "ancestors'"
            [ testCase "r0" $ []            @=? ancestors' r0
            , testCase "r1" $ [r0]          @=? ancestors' r1
            , testCase "r2" $ [r1,r0]       @=? ancestors' r2
            , testCase "r3" $ [r3pq,r3p,r0] @=? ancestors' r3
            ]

------------------------------------------------------------
--                     Quasi-Quoting                      --
------------------------------------------------------------

reldirT ∷ TypeRep
reldirT = typeRep (Proxy ∷ Proxy RelDir)

instance Parseable RelDir where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η RelDir
  parse (toText → t) =
    case unsnoc $ splitOn "/" t of
      Nothing           → error "cannot happen: splitOn always returns something"
      Just (("":_), _)  → __FPathAbsE__ reldirT t
      Just ([],"")      → __FPathEmptyE__ reldirT
      Just (["."],"")   → return $ RelDir ф
      Just ((x:xs), "") → do
        let mkCompE ∷ (AsFPathError ε', MonadError ε' η') ⇒
                      FPathComponentError → η' α
            mkCompE ce = __FPathComponentE__ ce reldirT t
            eCompE ∷ (AsFPathError ε'', MonadError ε'' η'') ⇒
                     Either FPathComponentError α → η'' α
            eCompE = either mkCompE return

        p  ← eCompE $ parsePathC x
        ps ← eCompE $ mapM parsePathC xs
        return $ RelDir (p <| Seq.fromList ps)
      _                 → __FPathNotADirE__ reldirT t

--------------------

parseRelDirTests ∷ TestTree
parseRelDirTests =
  let pamF        = "etc/pam"
      illegalCE s t = let fpcice = fPathComponentIllegalCharE '\0' t
                       in FPathComponentE fpcice reldirT s
      badChar s p = testCase ("bad component " ⊕ toString s) $
                        Left (illegalCE s p) @=? parseRelDir_ s
      emptyCompCE t = FPathComponentE fPathComponentEmptyE reldirT t
      parseRelDir_ ∷ MonadError FPathError η ⇒ Text → η RelDir
      parseRelDir_ = parse
   in testGroup "parseRelDir"
                [ testCase "r0 (./)" $ Right r0 @=? parseRelDir_ "./"
                , testCase "r1" $ Right r1 @=? parseRelDir_ "r/"
                , testCase "r2" $ Right r2 @=? parseRelDir_ "r/p/"
                , testCase "r3" $ Right r3 @=? parseRelDir_ "p/q/r/"
                , testCase "no trailing /" $
                      Left (fPathNotADirE reldirT pamF) @=? parseRelDir_ pamF
                , testCase "leading /" $
                      Left (fPathAbsE reldirT "/r/") @=? parseRelDir_ "/r/"
                , badChar "x/\0/y/" "\0"
                , badChar "r/p\0/" "p\0"
                , badChar "\0r/p/" "\0r"
                , testCase "empty component" $
                      Left (emptyCompCE "r//p/") @=? parseRelDir_ "r//p/"
                ]

----------------------------------------

{- | Like `parseRelDir`, but non-empty input that doesn't end in a '/' character
     has a '/' appended rather than failing as a non-file (thus, "permissive
     `parseRelDir`" -}
parseRelDirP ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η RelDir
parseRelDirP (toText → t) =
  let safeLast "" = Nothing
      safeLast s  = Just $ last s
   in case safeLast t of
        Nothing  → parse empty
        Just '/' → parse t
        _        → parse (t ⊕ "/")

parseRelDirP' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η RelDir
parseRelDirP' = parseRelDirP

__parseRelDirP__ ∷ Printable τ ⇒ τ → RelDir
__parseRelDirP__ = either __ERROR'__ id ∘ parseRelDirP'

__parseRelDirP'__ ∷ String → RelDir
__parseRelDirP'__ = __parseRelDirP__

--------------------

parseRelDirPTests ∷ TestTree
parseRelDirPTests =
  let pamNUL      = "etc/pam\0/"
      illegalCE   = let fpcice = fPathComponentIllegalCharE '\0' "pam\0"
                     in FPathComponentE fpcice reldirT pamNUL
      emptyCompCE = _FPathComponentE fPathComponentEmptyE reldirT "etc//pam.d/"
      _parseRelDirP ∷ MonadError FPathError η ⇒ Text → η RelDir
      _parseRelDirP = parseRelDirP'
   in testGroup "parseRelDirP"
                [ testCase "r0" $ Right r0 @=? _parseRelDirP "."
                , testCase "r1" $ Right r1 @=? _parseRelDirP "r/"
                , testCase "r1" $ Right r1 @=? _parseRelDirP "r"
                , testCase "r2" $ Right r2 @=? _parseRelDirP "r/p/"
                , testCase "r2" $ Right r2 @=? _parseRelDirP "r/p"
                , testCase "r3" $ Right r3 @=? _parseRelDirP "p/q/r/"
                , testCase "r3" $ Right r3 @=? _parseRelDirP "p/q/r"
                , testCase "empty" $
                      Left (fPathEmptyE reldirT)  @=? _parseRelDirP ""
                , testCase "no leading /" $
                      Left (fPathAbsE reldirT "/etc/") @=? _parseRelDirP "/etc/"
                , testCase "bad component" $
                      Left illegalCE @=? _parseRelDirP pamNUL
                , testCase "empty component" $
                      Left emptyCompCE @=? _parseRelDirP "etc//pam.d/"
                ]

----------------------------------------

{- | quasi-quotation -}
reldirQQ ∷ String → Maybe ExpQ
reldirQQ = (\ d → ⟦d⟧) ⩺ (ѭ ∘ parse @RelDir @FPathError)

reldir ∷ QuasiQuoter
reldir = mkQQ "RelDir" $ def & exp ⊩ reldirQQ

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

r0 ∷ RelDir
r0 = fromSeq ф

r1 ∷ RelDir
r1 = fromSeqNE $ pure [pc|r|]

r2 ∷ RelDir
r2 = fromSeqNE $ [pc|r|] ⋖ [[pc|p|]]

r3 ∷ RelDir
r3 = fromSeqNE $ [pc|p|] ⋖ [[pc|q|], [pc|r|]]

r3p ∷ RelDir
r3p = fromSeqNE $ pure [pc|p|]

r3pq ∷ RelDir
r3pq = fromSeqNE $ [pc|p|] ⋖ [[pc|q|]]

----------------------------------------

constructionTests ∷ TestTree
constructionTests = testGroup "construction" [ parseRelDirTests
                                             , parseRelDirPTests
                                             ]

tests ∷ TestTree
tests = testGroup "FPath.RelDir"
                  [ constructionTests, basenameTests, dirnameTests
                  , parentsTests, ancestors'Tests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
