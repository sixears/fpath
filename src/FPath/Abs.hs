{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.Abs
  ( Abs(..), AsAbs( _Abs ), absT

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
import Data.MoreUnicode.Lens      ( (##) )

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

import FPath.AbsDir            ( AbsDir, AsAbsDir( _AbsDir )
                               , AbsDirAs( _AbsDir_ )
                               , AsNonRootAbsDir( _NonRootAbsDir )
                               , NonRootAbsDir
                               , absdir
                               )
import FPath.AbsFile           ( AbsFile, AsAbsFile( _AbsFile ), absfile )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Basename          ( Basename( basename, updateBasename) )
import FPath.Dirname           ( HasDirname( ancestors', dirname ) )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( PathComponent, toUpper )
import FPath.Rel               ( Rel( RelD, RelF ) )
import FPath.RelDir            ( reldir )
import FPath.RelFile           ( relfile )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.T.FPath.TestData  ( etc, pamd, af1, af2, af3, af4, root, wgm )

--------------------------------------------------------------------------------

data Abs = AbsD AbsDir | AbsF AbsFile
  deriving (Eq, Show)

--------------------

instance Ord Abs where
  a <= b = toText a ≤ toText b

----------------------------------------

{-| Things that may convert to an `Abs` (but an `Abs` will always convert
    to); e.g., @FPath@. -}
-- we don't really need the Printable, AsFilePath requirements here; rather,
-- they should be true of all fpathish things, and including them here makes
-- many function type signatures simpler
class (Printable α, AsFilePath α) ⇒ AsAbs α where
  _Abs ∷ Prism' α Abs

instance AsAbs Abs where
  _Abs = id

----------------------------------------

instance AsAbsDir Abs where
  _AbsDir ∷ Prism' Abs AbsDir
  _AbsDir = prism' AbsD (\ case (AbsD d) → Just d; _ → Nothing)

instance AsNonRootAbsDir Abs where
  _NonRootAbsDir ∷ Prism' Abs NonRootAbsDir
  _NonRootAbsDir = prism' (AbsD ∘ review _AbsDir_)
                          (\ case (AbsD d) → d ⩼ _NonRootAbsDir; _ → Nothing)

instance AsAbsFile Abs where
  _AbsFile ∷ Prism' Abs AbsFile
  _AbsFile = prism' AbsF (\ case (AbsF f) → Just f; _ → Nothing)

----------------------------------------

instance Printable Abs where
  print (AbsD f) = print f
  print (AbsF f) = print f

instance Textual Abs where
  textual = try (AbsD ⊳ textual) ∤ AbsF ⊳ textual

----------------------------------------

instance AsFilePath Abs where
  filepath = prism' toString fromString

filepathTests ∷ TestTree
filepathTests =
  let nothin' = Nothing ∷ Maybe Abs
      fail s  = testCase s $ nothin' @=? s ⩼ filepath
   in testGroup "filepath"
            [ testCase "root"  $ "/"             ≟ AbsD root    ## filepath
            , testCase "etc"   $ "/etc/"         ≟ AbsD etc     ## filepath
            , testCase "pam.d" $ "/etc/pam.d/"   ≟ AbsD pamd    ## filepath
            , testCase "wgm"   $ "/w/g/M/"       ≟ AbsD wgm     ## filepath
            , testCase "/etc/" $ Just (AbsD etc) @=? "/etc/" ⩼ filepath

            , testCase "af1" $ "/r.e"       ≟ AbsF af1 ## filepath
            , testCase "af2" $ "/r/p.x"     ≟ AbsF af2 ## filepath
            , testCase "af3" $ "/p/q/r.mp3" ≟ AbsF af3 ## filepath
            , testCase "af4" $ "/.x"        ≟ AbsF af4 ## filepath

            , testCase "af1" $ Just (AbsF af1) @=? "/r.e"       ⩼ filepath
            , testCase "af2" $ Just (AbsF af2) @=? "/r/p.x"     ⩼ filepath
            , testCase "af3" $ Just (AbsF af3) @=? "/p/q/r.mp3" ⩼ filepath
            , testCase "af4" $ Just (AbsF af4) @=? "/.x"        ⩼ filepath

            , fail "etc"
            , fail "etc/pam.d/"
            , fail "etc//pam.d/"
            , fail "/\0etc"
            , fail "/etc\0"
            , fail "/e\0c"

            , fail "etc/"
            , fail "etc/pam.d"
            , fail "e/c"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"
            ]

------------------------------------------------------------

absT ∷ TypeRep
absT = typeRep (Proxy ∷ Proxy Abs)

instance Parseable Abs where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η Abs
  parse (toText → t) =
    case null t of
      𝕿 → __FPathEmptyE__ absT
      𝕱 → case last t of
            '/' → AbsD ⊳ parse t
            _   → AbsF ⊳ parse t

parseAbsTests ∷ TestTree
parseAbsTests =
  let success d f t = testCase t $ 𝕽 (d ## f) @=? parse @Abs @FPathError t
   in testGroup "parseAbs"
                [ success [absdir|/|]           _AbsDir "/"
                , success [absdir|/etc/|]       _AbsDir "/etc/"
                , success [absfile|/etc/group|] _AbsFile "/etc/group"
                ]

--------------------

instance RelTypeC Abs where
  type RelType Abs = Rel

--------------------

instance DirTypeC Abs where
  type DirType Abs = AbsDir

--------------------

instance Basename Abs where
  basename ∷ Abs → Rel
  basename (AbsD d) = RelD (basename d)
  basename (AbsF f) = RelF (basename f)

  updateBasename ∷ (PathComponent → PathComponent) → Abs → Abs
  updateBasename g (AbsD d) = AbsD (updateBasename g d)
  updateBasename g (AbsF f) = AbsF (updateBasename g f)

----------

basenameTests ∷ TestTree
basenameTests =
  testGroup "basename"
            [
              testCase "a0d" $ RelD [reldir|./|]     ≟ basename a0d
            , testCase "a1d" $ RelD [reldir|etc/|]   ≟ basename a1d
            , testCase "a2d" $ RelD [reldir|pam.d/|] ≟ basename a2d
            , testCase "a3d" $ RelD [reldir|M/|]     ≟ basename a3d
            , testCase "a1f" $ RelF [relfile|r.e|]   ≟ basename a1f
            , testCase "a2f" $ RelF [relfile|p.x|]   ≟ basename a2f
            , testCase "a3f" $ RelF [relfile|r.mp3|] ≟ basename a3f
            , testCase "a4f" $ RelF [relfile|.x|]    ≟ basename a4f
            ]
----------

updateBasenameTests ∷ TestTree
updateBasenameTests =
  let
      test input expect =
        testCase (toString input) $ expect ≟ updateBasename toUpper input
   in testGroup "updateBasename"
            [ test a0d a0d
            , test a1d (AbsD [absdir|/ETC/|])
            , test a2d (AbsD [absdir|/etc/PAM.D/|])
            , test a3d (AbsD [absdir|/w/g/M/|])
            , test a1f (AbsF [absfile|/R.E|])
            , test a2f (AbsF [absfile|/r/P.X|])
            , test a3f (AbsF [absfile|/p/q/R.MP3|])
            , test a4f (AbsF [absfile|/.X|])
            ]

--------------------

instance HasDirname Abs where
  dirname ∷ Lens' Abs AbsDir
  dirname = lens (\ case (AbsD d) → d ⊣ dirname
                         (AbsF f) → f ⊣ dirname)
                 (\ a ad → case (a,ad) of
                             (AbsD d, _) → AbsD $ d ⅋ dirname ⊢ ad
                             (AbsF f, _) → AbsF $ f ⅋ dirname ⊢ ad
                 )

  ancestors' ∷ Abs → [AbsDir]
  ancestors' (AbsD d) = ancestors' d
  ancestors' (AbsF f) = ancestors' f

----------

dirnameTests ∷ TestTree
dirnameTests =
  testGroup "dirname"
            [ testCase "a0d" $ [absdir|/|]     ≟ a0d ⊣ dirname
            , testCase "a1d" $ [absdir|/|]     ≟ a1d ⊣ dirname
            , testCase "a2d" $ [absdir|/etc/|] ≟ a2d ⊣ dirname
            , testCase "a3d" $ [absdir|/w/g/|] ≟ a3d ⊣ dirname

            , testCase "a1f" $ [absdir|/|]     ≟ a1f ⊣ dirname
            , testCase "a2f" $ [absdir|/r/|]   ≟ a2f ⊣ dirname
            , testCase "a3f" $ [absdir|/p/q/|] ≟ a3f ⊣ dirname
            , testCase "a4f" $ [absdir|/|]     ≟ a4f ⊣ dirname

            , testCase "a0d←./" $
                AbsD [absdir|/|]   ≟ a0d ⅋ dirname ⊢ [absdir|/|]
            , testCase "a0d←/p/" $
                AbsD [absdir|/p/|]   ≟ a0d ⅋ dirname ⊢ [absdir|/p/|]
            , testCase "a0d←/p/q/" $
                AbsD [absdir|/p/q/|] ≟ a0d ⅋ dirname ⊢ [absdir|/p/q/|]

            , testCase "a1f←./" $
                AbsF [absfile|/r.e|]     ≟ a1f ⅋ dirname ⊢ [absdir|/|]
            , testCase "a1f←p/" $
                AbsF [absfile|/p/r.e|]   ≟ a1f ⅋ dirname ⊢ [absdir|/p/|]
            , testCase "a1f←p/q/" $
                AbsF [absfile|/p/q/r.e|] ≟ a1f ⅋ dirname ⊢ [absdir|/p/q/|]

            , testCase "a1d←./" $
                AbsD [absdir|/etc/|]     ≟ a1d ⅋ dirname ⊢ [absdir|/|]
            , testCase "a1d←p/" $
                AbsD [absdir|/p/etc/|]   ≟ a1d ⅋ dirname ⊢ [absdir|/p/|]
            , testCase "a1d←p/q/" $
                AbsD [absdir|/p/q/etc/|] ≟ a1d ⅋ dirname ⊢ [absdir|/p/q/|]

            , testCase "a2f←./" $
                AbsF [absfile|/p.x|]     ≟ a2f ⅋ dirname ⊢ [absdir|/|]
            , testCase "a2f←p/" $
                AbsF [absfile|/p/p.x|]   ≟ a2f ⅋ dirname ⊢ [absdir|/p/|]
            , testCase "a2f←p/q/" $
                AbsF [absfile|/p/q/p.x|] ≟ a2f ⅋ dirname ⊢ [absdir|/p/q/|]

            , testCase "a2d←./" $
                AbsD [absdir|/pam.d/|]     ≟ a2d ⅋ dirname ⊢ [absdir|/|]
            , testCase "a2d←p/" $
                AbsD [absdir|/p/pam.d/|]   ≟ a2d ⅋ dirname ⊢ [absdir|/p/|]
            , testCase "a2d←p/q/" $
                AbsD [absdir|/p/q/pam.d/|] ≟ a2d ⅋ dirname ⊢ [absdir|/p/q/|]

            , testCase "a3f←./" $
                AbsF [absfile|/r.mp3|]     ≟ a3f ⅋ dirname ⊢ [absdir|/|]
            , testCase "a3f←p/" $
                AbsF [absfile|/p/r.mp3|]   ≟ a3f ⅋ dirname ⊢ [absdir|/p/|]
            , testCase "a3f←p/q/" $
                AbsF [absfile|/p/q/r.mp3|] ≟ a3f ⅋ dirname ⊢ [absdir|/p/q/|]

            , testCase "a3d←./" $
                AbsD [absdir|/M/|]     ≟ a3d ⅋ dirname ⊢ [absdir|/|]
            , testCase "a3d←p/" $
                AbsD [absdir|/p/M/|]   ≟ a3d ⅋ dirname ⊢ [absdir|/p/|]
            , testCase "a3d←p/q/" $
                AbsD [absdir|/p/q/M/|] ≟ a3d ⅋ dirname ⊢ [absdir|/p/q/|]

            , testCase "a4f←./" $
                AbsF [absfile|/.x|]     ≟ a4f ⅋ dirname ⊢ [absdir|/|]
            , testCase "a4f←p/" $
                AbsF [absfile|/p/.x|]   ≟ a4f ⅋ dirname ⊢ [absdir|/p/|]
            , testCase "a4f←p/q/" $
                AbsF [absfile|/p/q/.x|] ≟ a4f ⅋ dirname ⊢ [absdir|/p/q/|]
            ]

----------------------------------------

type instance Element Abs = PathComponent

----------------------------------------

instance MonoFunctor Abs where
  omap ∷ (PathComponent → PathComponent) → Abs → Abs
  omap g (AbsF f) = AbsF (omap g f)
  omap g (AbsD d) = AbsD (omap g d)

----------------------------------------

instance MonoFoldable Abs where
  otoList ∷ Abs → [PathComponent]
  otoList (AbsF f) = otoList f
  otoList (AbsD d) = otoList d

  ofoldl' ∷ (α → PathComponent → α) → α → Abs → α
  ofoldl' g x (AbsF f) = ofoldl' g x f
  ofoldl' g x (AbsD d) = ofoldl' g x d

  ofoldr ∷ (PathComponent → α → α) → α → Abs → α
  ofoldr g x (AbsF f) = ofoldr g x f
  ofoldr g x (AbsD d) = ofoldr g x d

  ofoldMap ∷ Monoid ν => (PathComponent → ν) → Abs → ν
  ofoldMap g (AbsF f) = ofoldMap g f
  ofoldMap g (AbsD d) = ofoldMap g d

  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → Abs
            → PathComponent
  ofoldr1Ex g (AbsF f) = ofoldr1Ex g f
  ofoldr1Ex g (AbsD d) = ofoldr1Ex g d

  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → Abs
             → PathComponent
  ofoldl1Ex' g (AbsF f) = ofoldl1Ex' g f
  ofoldl1Ex' g (AbsD d) = ofoldl1Ex' g d

----------------------------------------

instance ToSeq Abs where
  toSeq (AbsF f) = toSeq f
  toSeq (AbsD d) = toSeq d

----------------------------------------

instance Arbitrary Abs where
  arbitrary ∷ Gen Abs
  arbitrary = oneof [AbsF ⊳ arbitrary @AbsFile, AbsD ⊳ arbitrary @AbsDir]
  shrink ∷ Abs → [Abs]
  shrink (AbsF f) = AbsF ⊳ shrink f
  shrink (AbsD d) = AbsD ⊳ shrink d

----------------------------------------

{-| Things that /may/ be converted from an `Abs` (but will always convert /to/
    an `Abs`), e.g., @AbsFile@, @AbsDir@. -}
-- we don't really need the Printable, AsFilePath requirements here; rather,
-- they should be true of all fpathish things, and including them here makes
-- many function type signatures simpler
class (Printable α, AsFilePath α) ⇒ AbsAs α where
  _Abs_ ∷ Prism' Abs α
instance AbsAs Abs where
  _Abs_ = id
instance AbsAs AbsFile where
  _Abs_ = _AbsFile
instance AbsAs AbsDir where
  _Abs_ = _AbsDir

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

a0d ∷ Abs
a0d = AbsD root
a1d ∷ Abs
a1d = AbsD etc
a2d ∷ Abs
a2d = AbsD pamd
a3d ∷ Abs
a3d = AbsD wgm

a1f ∷ Abs
a1f = AbsF af1
a2f ∷ Abs
a2f = AbsF af2
a3f ∷ Abs
a3f = AbsF af3
a4f ∷ Abs
a4f = AbsF af4

----------------------------------------

tests ∷ TestTree
tests = testGroup "Abs" [ basenameTests, dirnameTests, updateBasenameTests
                        , filepathTests, parseAbsTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
