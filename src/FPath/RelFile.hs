{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.RelFile
  ( AsRelFile( _RelFile ), RelDir, RelFile

  , relfile, relfileT

  , tests
  )
where

import Base1T  hiding  ( toList )
import Prelude  ( error )

-- base --------------------------------

import qualified  Data.List.NonEmpty  as  NonEmpty

import Data.Foldable  ( foldMap )
import Data.Monoid    ( Monoid )
import Data.Typeable  ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Exts       ( IsList( toList ) )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ), fromString )

-- lens --------------------------------

import Control.Lens.Cons    ( unsnoc )
import Control.Lens.Iso     ( iso )

-- monaderror-io -----------------------

import MonadError  ( ѭ )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFoldable( ofoldl', ofoldl1Ex'
                                                    , ofoldMap, ofoldr
                                                    , ofoldr1Ex, otoList )
                             , MonoFunctor( omap )
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Function  ( (⅋) )
import Data.MoreUnicode.Lens      ( (⊩) )

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

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertListEq )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( Text, dropEnd, intercalate, length, splitOn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Basename          ( Basename( basename, updateBasename ) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.AsFilePath'       ( AsFilePath'( filepath' ) )
import FPath.Dirname           ( Ancestors( ancestors )
                               , HasDirname( ancestors', dirname ) )
import FPath.DirType           ( DirTypeC( DirType ) )

import FPath.Error.FPathComponentError  ( FPathComponentError )
import FPath.Error.FPathError  ( AsFPathError, FPathError
                               , __FPathComponentE__, __FPathEmptyE__
                               , __FPathAbsE__, __FPathNotAFileE__
                               , mapTypeRepE, mapTextE
                               )
import FPath.FileLike          ( FileLike( dirfile ) )
import FPath.Parent            ( HasParent( parent )
                               , HasParentMay( parentMay, parents ) )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( PathComponent, parsePathC, pc )
import FPath.RelDir            ( RelDir, reldir )
import FPath.RelType           ( RelTypeC( RelType ) )

-------------------------------------------------------------------------------

{- | a relative file -}
data RelFile = RelFile RelDir PathComponent
  deriving (Eq, Lift)

type instance Element RelFile = PathComponent

instance Show RelFile where
  show r = [fmt|[relfile|%T%s]|] (toText r) "|"

--------------------

instance Ord RelFile where
  a <= b = toText a ≤ toText b

--------------------

class AsRelFile α where
  _RelFile ∷ Prism' α RelFile

instance AsRelFile RelFile where
  _RelFile = id

--------------------

instance DirTypeC RelFile where
  type DirType RelFile = RelDir

--------------------

instance RelTypeC RelFile where
  type RelType RelFile = RelFile

----------------------------------------

instance FileLike RelFile where
  dirfile = iso (\ (RelFile d f) → (d,f)) (\ (d,f) → RelFile d f)

----------------------------------------

instance MonoFunctor RelFile where
  omap ∷ (PathComponent → PathComponent) → RelFile → RelFile
  omap f (RelFile ps b) = RelFile (omap f ps) (f b)

----------------------------------------

instance MonoFoldable RelFile where
  otoList ∷ RelFile → [PathComponent]
  otoList (RelFile ps f) = otoList ps ⊕ [f]

  ofoldl' ∷ (α → PathComponent → α) → α → RelFile → α
  ofoldl' f x r = foldl' f x (toNonEmpty r)

  ofoldr ∷ (PathComponent → α → α) → α → RelFile → α
  ofoldr f x r = foldr f x (toNonEmpty r)

  ofoldMap ∷ Monoid ν => (PathComponent → ν) → RelFile → ν
  ofoldMap f r = foldMap f (toNonEmpty r)

  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → RelFile
            → PathComponent
  ofoldr1Ex f r = foldr1 f (toNonEmpty r)

  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → RelFile
             → PathComponent
  ofoldl1Ex' f r = foldl1 f (toNonEmpty r)

----------------------------------------

instance FromSeqNonEmpty RelFile where
  fromSeqNE (ps :⪭ f) = RelFile (fromSeq ps) f
  fromSeqNE _         = error "RelFile.fromSeqNE pattern match can't get here"

----------------------------------------

instance ToSeqNonEmpty RelFile where
  toSeqNE (RelFile ps f) = toSeq ps ⪭ f

instance ToSeq RelFile where
  toSeq = toSeq_

----------------------------------------

instance IsSeqNonEmpty RelFile where
  seqNE = iso toSeqNE fromSeqNE

----------------------------------------

instance FromMonoNonEmpty RelFile where
  fromNonEmpty (x :| xs) = case NonEmpty.nonEmpty xs of
                             Nothing  → RelFile (fromList ф) x
                             Just xs' → let pcs = x : NonEmpty.init xs'
                                            f   = NonEmpty.last xs'
                                         in RelFile (fromList pcs) f

instance ToMonoNonEmpty RelFile where
  toNonEmpty (RelFile ps f) = NonEmpty.fromList $ toList ps ⊕ [f]

instance IsMonoNonEmpty RelFile where
  nonEmpty = iso toNonEmpty fromNonEmpty

----------------------------------------

instance Printable RelFile where
  print r = P.text $ intercalate "/" (toText ⊳ otoList r)

----------------------------------------

instance AsFilePath RelFile where
  filepath = prism' toString fromString

instance AsFilePath' RelFile where
  filepath' = filepath

----------------------------------------

instance Textual RelFile where
  textual = fromNonEmpty ⊳ (sepByNonEmpty textual (char '/'))

----------------------------------------

instance Arbitrary RelFile where
  arbitrary = fromSeqNE ⊳ arbitrary
  shrink = fromSeqNE ⩺ shrink ∘ toSeqNE

----------------------------------------

instance HasParent RelFile where
  parent = lens (\ (RelFile p _) → p) (\ (RelFile _ f) p → RelFile p f)

----------------------------------------

instance HasParentMay RelFile where
  parentMay = lens (\ (RelFile p _) → Just p)
                   (\ (RelFile _ f) md → case md of
                                           Just  d → RelFile d f
                                           Nothing → RelFile ф f
                   )

----------

parentsTests ∷ TestTree
parentsTests =
  let check t d ps = assertListEq t ps (parents d)
   in testGroup "parents" $
        [ check "r.e"       rf1 [fromSeq ф]
        , check "r/p.x"     rf2 [fromSeq ф, fromSeq (pure [pc|r|])]
        , check "p/q/r.mp3" rf3 [fromSeq ф, [reldir|p/|], [reldir|p/q/|]]
        , check ".x"        rf4 [fromSeq ф]
        ]

----------------------------------------

instance Basename RelFile where
  basename ∷ RelFile → RelFile
  basename (RelFile _ n) = fromNonEmpty (pure n)

  updateBasename ∷ (PathComponent → PathComponent) → RelFile → RelFile
  updateBasename f (RelFile d n) = RelFile d (f n)

basenameTests ∷ TestTree
basenameTests =
  testGroup "basename"
            [ testCase "rf1" $ fromSeqNE (pure [pc|r.e|])  ≟ basename rf1

            , testCase "rf1 -> rf4"    $
                rf4 ≟ updateBasename (const [pc|.x|]) rf1
            , testCase "rf2 -> x.p"     $
                  fromSeqNE ([pc|r|] ⋖ [[pc|x.p|]])
                ≟ updateBasename (const [pc|x.p|]) rf2
            , testCase "pam.d -> pam.d"   $
                  fromSeqNE ([pc|p|] ⋖ [[pc|q|], [pc|r|]])
                ≟ updateBasename (const [pc|r|]) rf3
            , testCase "rf4 -> rf1"   $
                rf1 ≟ updateBasename (const [pc|r.e|]) rf4
            ]


----------------------------------------

instance Ancestors RelFile where
  ancestors ∷ RelFile → NonEmpty RelDir
  ancestors fp = let d = fp ⊣ dirname
                  in d :| ancestors' d

ancestorsTests ∷ TestTree
ancestorsTests =
  testGroup "ancestors"
            [ testCase "rf1" $ pure [reldir|./|]  @=? ancestors rf1
            , testCase "rf2"  $
                ([reldir|r/|] :| [[reldir|./|]]) @=? ancestors rf2
            , testCase "rf3" $
                    ([reldir|p/q/|] :| [[reldir|p/|], [reldir|./|]])
                @=? ancestors rf3
            , testCase "rf4"  $ pure [reldir|./|] @=? ancestors rf4
            ]

instance HasDirname RelFile where
  dirname ∷ Lens' RelFile RelDir
  dirname = lens (\ (RelFile d _) → d) (\ (RelFile _ f) d → RelFile d f)

  ancestors' ∷ RelFile → [RelDir]
  ancestors' = toList ∘ ancestors

dirnameTests ∷ TestTree
dirnameTests =
  testGroup "dirname"
            [ testCase "rf1" $ [reldir|./|]     ≟ rf1 ⊣ dirname
            , testCase "rf2" $ [reldir|r/|]     ≟ rf2 ⊣ dirname
            , testCase "rf3" $ [reldir|p/q/|]   ≟ rf3 ⊣ dirname
            , testCase "rf4" $ [reldir|./|]     ≟ rf4 ⊣ dirname

            , testCase "rf1" $ rf1                ≟ rf1 ⅋ dirname ⊢ [reldir|./|]
            , testCase "rf1 ← /r/" $
                fromSeqNE ([pc|r|]⋖[[pc|r.e|]])   ≟ rf1 ⅋ dirname ⊢ [reldir|r/|]
            , testCase "rf3 ← /" $
                fromSeqNE ([pc|r.mp3|]⋖[])        ≟ rf3 ⅋ dirname ⊢ [reldir|./|]
            , testCase "rf3 ← /r/" $
                fromSeqNE ([pc|r|]⋖[[pc|r.mp3|]]) ≟ rf3 ⅋ dirname ⊢ [reldir|r/|]
            ]

ancestors'Tests ∷ TestTree
ancestors'Tests =
  testGroup "ancestors'"
            [ testCase "rf1" $ [[reldir|./|]]  @=? ancestors' rf1
            , testCase "rf2" $ [[reldir|r/|], [reldir|./|]] @=? ancestors' rf2
            , testCase "rf3" $
                [[reldir|p/q/|], [reldir|p/|], [reldir|./|]] @=? ancestors' rf3
            , testCase "rf4" $ pure [reldir|./|] @=? ancestors' rf4
            ]

------------------------------------------------------------
--                     Quasi-Quoting                      --
------------------------------------------------------------

relfileT ∷ TypeRep
relfileT = typeRep (Proxy ∷ Proxy RelFile)

instance Parseable RelFile where
  parse (toText → t) =
    let mkCompE    ∷ (AsFPathError ε', MonadError ε' η') ⇒
                     FPathComponentError → η' α
        mkCompE ce = __FPathComponentE__ ce relfileT t
        eCompE     ∷ (AsFPathError ε'', MonadError ε'' η'') ⇒
                     Either FPathComponentError α → η'' α
        eCompE = either mkCompE return
        -- map the type marker on FPathError
        mapFPCE ∷ (AsFPathError ε', MonadError ε' η') ⇒
                  (Text → Text) → Either FPathError α → η' α
        mapFPCE f = mapTextE f ∘ mapTypeRepE (const relfileT)
     in case unsnoc $ splitOn "/" t of
          Nothing           → error "error: splitOn always returns something"
          Just ([],"")      → __FPathEmptyE__ relfileT
          Just (_, "")      → __FPathNotAFileE__ relfileT t
          Just (("":_), _)  → __FPathAbsE__ relfileT t
          Just ([], f)      → do f' ← eCompE $ parsePathC f
                                 return $ RelFile (fromSeq ф) f'
          Just (_, f)       → do f' ← eCompE $ parsePathC f
                                 ps' ← mapFPCE (⊕ f) $
                                         parse (dropEnd (length f) t)
                                 return $ RelFile ps' f'

{- | quasi-quotation -}
relfileQQ ∷ String → Maybe ExpQ
relfileQQ = (\ f → ⟦f⟧) ⩺ (ѭ ∘ parse @RelFile @FPathError)

relfile ∷ QuasiQuoter
relfile = mkQQ "RelFile" $ def & exp ⊩ relfileQQ

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

rf1 ∷ RelFile
rf1 = fromSeqNE $ pure [pc|r.e|]

rf2 ∷ RelFile
rf2 = fromSeqNE $ [pc|r|] ⋖ [[pc|p.x|]]

rf3 ∷ RelFile
rf3 = fromSeqNE $ [pc|p|] ⋖ [[pc|q|], [pc|r.mp3|]]

rf4 ∷ RelFile
rf4 = fromSeqNE $ pure [pc|.x|]

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath.RelFile" [ basenameTests, dirnameTests, parentsTests
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
