{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.Rel
  ( AsRel( _Rel ), Rel(..), RelAs(..)

  , tests
  )
where

import Base1T  hiding  ( last )

-- base --------------------------------

import Data.Monoid    ( Monoid )
import Data.Typeable  ( Proxy( Proxy ), TypeRep, typeRep )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ), fromString )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFoldable( ofoldl', ofoldl1Ex'
                                                    , ofoldMap, ofoldr
                                                    , ofoldr1Ex, otoList )
                             , MonoFunctor( omap )
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Function  ( (⅋) )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqConversions  ( ToSeq( toSeq ) )

-- parsers -----------------------------

import Text.Parser.Combinators  ( try )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( Gen, oneof )

-- tasty-plus --------------------------

import TastyPlus  ( (≟) )

-- text --------------------------------

import Data.Text  ( last, null )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Basename          ( Basename( basename, updateBasename) )
import FPath.Dirname           ( HasDirname( ancestors', dirname ) )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( PathComponent, toUpper )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir, reldir )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile
                               , relfile )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.T.FPath.TestData  ( r0, r1, r2, r3, rf1, rf2, rf3, rf4 )

--------------------------------------------------------------------------------

data Rel = RelD RelDir | RelF RelFile
  deriving (Eq, Show)

--------------------

instance Ord Rel where
  a <= b = toText a ≤ toText b

--------------------

{-| Things that may convert to a `Rel` (but a `Rel` will always convert to);
    e.g., @FPath@. -}
-- we don't really need the Printable, AsFilePath requirements here; rather,
-- they should be true of all fpathish things, and including them here makes
-- many function type signatures simpler
class (Printable α, AsFilePath α) ⇒ AsRel α where
  _Rel ∷ Prism' α Rel

instance AsRel Rel where
  _Rel = id

----------------------------------------

instance AsRelDir Rel where
  _RelDir ∷ Prism' Rel RelDir
  _RelDir = prism' RelD (\ case (RelD d) → Just d; _ → Nothing)

instance AsRelFile Rel where
  _RelFile ∷ Prism' Rel RelFile
  _RelFile = prism' RelF (\ case (RelF f) → Just f; _ → Nothing)

----------------------------------------

instance Printable Rel where
  print (RelD f) = print f
  print (RelF f) = print f

instance Textual Rel where
  textual = try (RelD ⊳ textual) ∤ RelF ⊳ textual

----------------------------------------

instance AsFilePath Rel where
  filepath = prism' toString fromString

filepathTests ∷ TestTree
filepathTests =
  let nothin' = Nothing ∷ Maybe Rel
      fail s  = testCase s $ nothin' @=? s ⩼ filepath
   in testGroup "filepath"
            [ testCase "r0d" $ "./"     ≟ r0d ⫥ filepath
            , testCase "r1d" $ "r/"     ≟ r1d ⫥ filepath
            , testCase "r2d" $ "r/p/"   ≟ r2d ⫥ filepath
            , testCase "r3d" $ "p/q/r/" ≟ r3d ⫥ filepath

            , testCase "r0" $ Just r0d @=? "./"     ⩼ filepath
            , testCase "r1" $ Just r1d @=? "r/"     ⩼ filepath
            , testCase "r2" $ Just r2d @=? "r/p/"   ⩼ filepath
            , testCase "r3" $ Just r3d @=? "p/q/r/" ⩼ filepath

            , testCase "rf1" $ "r.e"       ≟ r1f ⫥ filepath
            , testCase "rf2" $ "r/p.x"     ≟ r2f ⫥ filepath
            , testCase "rf3" $ "p/q/r.mp3" ≟ r3f ⫥ filepath
            , testCase "rf4" $ ".x"        ≟ r4f ⫥ filepath

            , testCase "rf1" $ Just r1f @=? "r.e"       ⩼ filepath
            , testCase "rf2" $ Just r2f @=? "r/p.x"     ⩼ filepath
            , testCase "rf3" $ Just r3f @=? "p/q/r.mp3" ⩼ filepath
            , testCase "rf4" $ Just r4f @=? ".x"        ⩼ filepath

            , fail "/etc"
            , fail "/etc/pam.d/"
            , fail "/etc//pam.d/"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"

            , fail "/etc/pam.d"
            ]

----------------------------------------

relpathT ∷ TypeRep
relpathT = typeRep (Proxy ∷ Proxy Rel)

instance Parseable Rel where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η Rel
  parse (toText → t) =
    case null t of
      𝕿 → __FPathEmptyE__ relpathT
      𝕱 → case last t of
            '/' → RelD ⊳ parse  t
            _   → RelF ⊳ parse t

parseRelTests ∷ TestTree
parseRelTests =
  let success d f t = testCase t $ Right (d ⫥ f) @=? parse @Rel @FPathError t
   in testGroup "parseRel"
                [ success [reldir|./|]         _RelDir "./"
                , success [reldir|etc/|]       _RelDir "etc/"
                , success [relfile|etc/group|] _RelFile "etc/group"
                ]

--------------------

instance RelTypeC Rel where
  type RelType Rel = Rel

--------------------

instance DirTypeC Rel where
  type DirType Rel = RelDir

--------------------

instance Basename Rel where
  basename ∷ Rel → Rel
  basename (RelD d) = RelD (basename d)
  basename (RelF f) = RelF (basename f)

  updateBasename ∷ (PathComponent → PathComponent) → Rel → Rel
  updateBasename g (RelD d) = RelD (updateBasename g d)
  updateBasename g (RelF f) = RelF (updateBasename g f)

----------

basenameTests ∷ TestTree
basenameTests =
  testGroup "basename"
            [
              testCase "r0d" $ r0d                   ≟ basename r0d
            , testCase "r1d" $ r1d                   ≟ basename r1d
            , testCase "r2d" $ RelD [reldir|p/|]     ≟ basename r2d
            , testCase "r3d" $ r1d                   ≟ basename r3d
            , testCase "r1f" $ r1f                   ≟ basename r1f
            , testCase "r2f" $ RelF [relfile|p.x|]   ≟ basename r2f
            , testCase "r3f" $ RelF [relfile|r.mp3|] ≟ basename r3f
            , testCase "r4f" $ r4f                   ≟ basename r4f
            ]

----------

updateBasenameTests ∷ TestTree
updateBasenameTests =
  let
      test input expect =
        testCase (toString input) $ expect ≟ updateBasename toUpper input
   in testGroup "updateBasename"
            [ test r0d r0d
            , test r1d (RelD [reldir|R/|])
            , test r2d (RelD [reldir|r/P/|])
            , test r3d (RelD [reldir|p/q/R/|])
            , test r1f (RelF [relfile|R.E|])
            , test r2f (RelF [relfile|r/P.X|])
            , test r3f (RelF [relfile|p/q/R.MP3|])
            , test r4f (RelF [relfile|.X|])
            ]

--------------------

instance HasDirname Rel where
  dirname ∷ Lens' Rel RelDir
  dirname = lens (\ case (RelD d) → d ⊣ dirname
                         (RelF f) → f ⊣ dirname)
                 (\ r rd → case (r,rd) of
                             (RelD d, _) → RelD $ d ⅋ dirname ⊢ rd
                             (RelF f, _) → RelF $ f ⅋ dirname ⊢ rd
                 )

  ancestors' ∷ Rel → [RelDir]
  ancestors' (RelD d) = ancestors' d
  ancestors' (RelF f) = ancestors' f

----------

dirnameTests ∷ TestTree
dirnameTests =
  testGroup "dirname"
            [ testCase "r0d" $ [reldir|./|]   ≟ r0d ⊣ dirname
            , testCase "r1d" $ [reldir|./|]   ≟ r1d ⊣ dirname
            , testCase "r2d" $ [reldir|r/|]   ≟ r2d ⊣ dirname
            , testCase "r3d" $ [reldir|p/q/|] ≟ r3d ⊣ dirname

            , testCase "r1f" $ [reldir|./|]   ≟ r1f ⊣ dirname
            , testCase "r2f" $ [reldir|r/|]   ≟ r2f ⊣ dirname
            , testCase "r3f" $ [reldir|p/q/|] ≟ r3f ⊣ dirname
            , testCase "r4f" $ [reldir|./|]   ≟ r4f ⊣ dirname

            , testCase "r0d←./" $
                RelD [reldir|./|]   ≟ r0d ⅋ dirname ⊢ [reldir|./|]
            , testCase "r0d←/p/" $
                RelD [reldir|p/|]   ≟ r0d ⅋ dirname ⊢ [reldir|p/|]
            , testCase "r0d←/p/q/" $
                RelD [reldir|p/q/|] ≟ r0d ⅋ dirname ⊢ [reldir|p/q/|]

            , testCase "r1f←./" $
                RelF [relfile|r.e|]     ≟ r1f ⅋ dirname ⊢ [reldir|./|]
            , testCase "r1f←p/" $
                RelF [relfile|p/r.e|]   ≟ r1f ⅋ dirname ⊢ [reldir|p/|]
            , testCase "r1f←p/q/" $
                RelF [relfile|p/q/r.e|] ≟ r1f ⅋ dirname ⊢ [reldir|p/q/|]

            , testCase "r1d←./" $
                RelD [reldir|r/|]     ≟ r1d ⅋ dirname ⊢ [reldir|./|]
            , testCase "r1d←p/" $
                RelD [reldir|p/r/|]   ≟ r1d ⅋ dirname ⊢ [reldir|p/|]
            , testCase "r1d←p/q/" $
                RelD [reldir|p/q/r/|] ≟ r1d ⅋ dirname ⊢ [reldir|p/q/|]

            , testCase "r2f←./" $
                RelF [relfile|p.x|]     ≟ r2f ⅋ dirname ⊢ [reldir|./|]
            , testCase "r2f←p/" $
                RelF [relfile|p/p.x|]   ≟ r2f ⅋ dirname ⊢ [reldir|p/|]
            , testCase "r2f←p/q/" $
                RelF [relfile|p/q/p.x|] ≟ r2f ⅋ dirname ⊢ [reldir|p/q/|]

            , testCase "r2d←./" $
                RelD [reldir|p/|]   ≟ r2d ⅋ dirname ⊢ [reldir|./|]
            , testCase "r2d←p/" $
                RelD [reldir|p/p/|]   ≟ r2d ⅋ dirname ⊢ [reldir|p/|]
            , testCase "r2d←p/q/" $
                RelD [reldir|p/q/p/|] ≟ r2d ⅋ dirname ⊢ [reldir|p/q/|]

            , testCase "r3f←./" $
                RelF [relfile|r.mp3|]     ≟ r3f ⅋ dirname ⊢ [reldir|./|]
            , testCase "r3f←p/" $
                RelF [relfile|p/r.mp3|]   ≟ r3f ⅋ dirname ⊢ [reldir|p/|]
            , testCase "r3f←p/q/" $
                RelF [relfile|p/q/r.mp3|] ≟ r3f ⅋ dirname ⊢ [reldir|p/q/|]

            , testCase "r3d←./" $
                RelD [reldir|r/|]     ≟ r3d ⅋ dirname ⊢ [reldir|./|]
            , testCase "r3d←p/" $
                RelD [reldir|p/r/|]   ≟ r3d ⅋ dirname ⊢ [reldir|p/|]
            , testCase "r3d←p/q/" $
                RelD [reldir|p/q/r/|] ≟ r3d ⅋ dirname ⊢ [reldir|p/q/|]

            , testCase "r4f←./" $
                RelF [relfile|.x|]     ≟ r4f ⅋ dirname ⊢ [reldir|./|]
            , testCase "r4f←p/" $
                RelF [relfile|p/.x|]   ≟ r4f ⅋ dirname ⊢ [reldir|p/|]
            , testCase "r4f←p/q/" $
                RelF [relfile|p/q/.x|] ≟ r4f ⅋ dirname ⊢ [reldir|p/q/|]
            ]

----------------------------------------

type instance Element Rel = PathComponent

----------------------------------------

instance MonoFunctor Rel where
  omap ∷ (PathComponent → PathComponent) → Rel → Rel
  omap g (RelF f) = RelF (omap g f)
  omap g (RelD d) = RelD (omap g d)

----------------------------------------

instance MonoFoldable Rel where
  otoList ∷ Rel → [PathComponent]
  otoList (RelF f) = otoList f
  otoList (RelD d) = otoList d

  ofoldl' ∷ (α → PathComponent → α) → α → Rel → α
  ofoldl' g x (RelF f) = ofoldl' g x f
  ofoldl' g x (RelD d) = ofoldl' g x d

  ofoldr ∷ (PathComponent → α → α) → α → Rel → α
  ofoldr g x (RelF f) = ofoldr g x f
  ofoldr g x (RelD d) = ofoldr g x d

  ofoldMap ∷ Monoid ν => (PathComponent → ν) → Rel → ν
  ofoldMap g (RelF f) = ofoldMap g f
  ofoldMap g (RelD d) = ofoldMap g d

  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → Rel
            → PathComponent
  ofoldr1Ex g (RelF f) = ofoldr1Ex g f
  ofoldr1Ex g (RelD d) = ofoldr1Ex g d

  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → Rel
             → PathComponent
  ofoldl1Ex' g (RelF f) = ofoldl1Ex' g f
  ofoldl1Ex' g (RelD d) = ofoldl1Ex' g d

----------------------------------------

instance ToSeq Rel where
  toSeq (RelF f) = toSeq f
  toSeq (RelD d) = toSeq d

----------------------------------------

instance Arbitrary Rel where
  arbitrary ∷ Gen Rel
  arbitrary = oneof [RelF ⊳ arbitrary @RelFile, RelD ⊳ arbitrary @RelDir]
  shrink ∷ Rel → [Rel]
  shrink (RelF f) = RelF ⊳ shrink f
  shrink (RelD d) = RelD ⊳ shrink d

----------------------------------------

{-| Things that /may/ be converted from a `Rel` (but will always convert /to/ a
    `Rel`), e.g., @RelFile@, @RelDir@. -}
-- we don't really need the Printable, AsFilePath requirements here; rather,
-- they should be true of all fpathish things, and including them here makes
-- many function type signatures simpler
class (Printable α, AsFilePath α) ⇒ RelAs α where
  _Rel_ ∷ Prism' Rel α
instance RelAs Rel where
  _Rel_ = id
instance RelAs RelFile where
  _Rel_ = _RelFile
instance RelAs RelDir where
  _Rel_ = _RelDir

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

r0d ∷ Rel
r0d = RelD r0
r1d ∷ Rel
r1d = RelD r1
r2d ∷ Rel
r2d = RelD r2
r3d ∷ Rel
r3d = RelD r3

r1f ∷ Rel
r1f = RelF rf1
r2f ∷ Rel
r2f = RelF rf2
r3f ∷ Rel
r3f = RelF rf3
r4f ∷ Rel
r4f = RelF rf4

----------------------------------------

tests ∷ TestTree
tests = testGroup "Rel" [ basenameTests, updateBasenameTests, dirnameTests
                        , filepathTests, parseRelTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
