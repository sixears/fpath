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

module FPath.FPath
  ( FPath(..), FPathAs(..), tests )
where

import Base1T  hiding  ( head, last )

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

import Data.Text  ( head, last, null )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Abs               ( AsAbs(_Abs ), Abs( AbsD, AbsF ) )
import FPath.AbsDir            ( AbsDir, AsAbsDir( _AbsDir )
                               , AbsDirAs( _AbsDir_ )
                               , AsNonRootAbsDir( _NonRootAbsDir )
                               , NonRootAbsDir
                               , absdir
                               )
import FPath.AbsFile           ( AbsFile, AsAbsFile( _AbsFile )
                               , absfile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Basename          ( Basename( basename, updateBasename) )
import FPath.Dir               ( AsDir( _Dir ), Dir( DirA, DirR ) )
import FPath.Dirname           ( HasDirname( ancestors', dirname ) )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.File              ( AsFile( _File ), File( FileA, FileR ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( PathComponent, toUpper )
import FPath.Rel               ( AsRel(_Rel ), Rel( RelD, RelF ) )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir, reldir )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile, relfile )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.T.FPath.TestData  ( af1, af2, af3, af4, etc, pamd
                               , r0, r1, r2, r3, rf1, rf2, rf3, rf4, root, wgm )

--------------------------------------------------------------------------------

data FPath = FAbsD AbsDir | FAbsF AbsFile | FRelD RelDir | FRelF RelFile
  deriving (Eq, Show)

--------------------

instance Ord FPath where
  a <= b = toText a ≤ toText b

----------------------------------------

instance AsAbsDir FPath where
  _AbsDir ∷ Prism' FPath AbsDir
  _AbsDir = prism' FAbsD (\ case (FAbsD d) → Just d; _ → Nothing)

----------------------------------------

instance AsAbsFile FPath where
  _AbsFile ∷ Prism' FPath AbsFile
  _AbsFile = prism' FAbsF (\ case (FAbsF f) → Just f; _ → Nothing)

----------------------------------------

instance AsNonRootAbsDir FPath where
  _NonRootAbsDir ∷ Prism' FPath NonRootAbsDir
  _NonRootAbsDir = prism' (FAbsD ∘ review _AbsDir_)
                          (\ case (FAbsD d) → d ⩼ _NonRootAbsDir; _ → Nothing)

----------------------------------------

instance AsRelDir FPath where
  _RelDir ∷ Prism' FPath RelDir
  _RelDir = prism' FRelD (\ case (FRelD d) → Just d; _ → Nothing)

----------------------------------------

instance AsRelFile FPath where
  _RelFile ∷ Prism' FPath RelFile
  _RelFile = prism' FRelF (\ case (FRelF f) → Just f; _ → Nothing)

----------------------------------------

instance AsAbs FPath where
  _Abs = prism' (\ p → case p of AbsD d → FAbsD d
                                 AbsF f → FAbsF f
                )
                (\ p → case p of FAbsD d → Just $ AbsD d
                                 FAbsF f → Just $ AbsF f
                                 _       → Nothing
                )


----------------------------------------

instance AsRel FPath where
  _Rel = prism' (\ p → case p of RelD d → FRelD d
                                 RelF f → FRelF f
                )
                (\ p → case p of FRelD d → Just $ RelD d
                                 FRelF f → Just $ RelF f
                                 _       → Nothing
                )

----------------------------------------

instance AsDir FPath where
  _Dir = prism' (\ p → case p of DirA d → FAbsD d
                                 DirR f → FRelD f
                )
                (\ p → case p of FAbsD d → Just $ DirA d
                                 FRelD f → Just $ DirR f
                                 _       → Nothing
                )

----------------------------------------

instance AsFile FPath where
  _File = prism' (\ p → case p of FileA d → FAbsF d
                                  FileR f → FRelF f
                 )
                 (\ p → case p of FAbsF d → Just $ FileA d
                                  FRelF f → Just $ FileR f
                                  _       → Nothing
                 )

----------------------------------------

instance Printable FPath where
  print (FAbsD f) = print f
  print (FAbsF f) = print f
  print (FRelD f) = print f
  print (FRelF f) = print f

instance Textual FPath where
  textual = try (FAbsD ⊳ textual) ∤ try (FAbsF ⊳ textual)
          ∤ try (FRelD ⊳ textual) ∤ FRelF ⊳ textual

----------------------------------------

instance AsFilePath FPath where
  filepath = prism' toString fromString

filepathTests ∷ TestTree
filepathTests =
  let nothin' = Nothing ∷ Maybe FPath
      fail s  = testCase s $ nothin' @=? s ⩼ filepath
   in testGroup "filepath"
            [ testCase "r0" $ "./"     ≟ r0d ⫥ filepath
            , testCase "r1" $ "r/"     ≟ r1d ⫥ filepath
            , testCase "r2" $ "r/p/"   ≟ r2d ⫥ filepath
            , testCase "r3" $ "p/q/r/" ≟ r3d ⫥ filepath

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

            , testCase "root"  $ "/"             ≟ a0d ⫥ filepath
            , testCase "etc"   $ "/etc/"         ≟ a1d ⫥ filepath
            , testCase "pam.d" $ "/etc/pam.d/"   ≟ a2d ⫥ filepath
            , testCase "wgm"   $ "/w/g/M/"       ≟ a3d ⫥ filepath
            , testCase "/etc/" $ Just (FAbsD etc) @=? "/etc/" ⩼ filepath

            , testCase "af1" $ "/r.e"       ≟ a1f ⫥ filepath
            , testCase "af2" $ "/r/p.x"     ≟ a2f ⫥ filepath
            , testCase "af3" $ "/p/q/r.mp3" ≟ a3f ⫥ filepath
            , testCase "af4" $ "/.x"        ≟ a4f ⫥ filepath

            , testCase "af1" $ Just a1f @=? "/r.e"       ⩼ filepath
            , testCase "af2" $ Just a2f @=? "/r/p.x"     ⩼ filepath
            , testCase "af3" $ Just a3f @=? "/p/q/r.mp3" ⩼ filepath
            , testCase "af4" $ Just a4f @=? "/.x"        ⩼ filepath

            , fail "/etc//pam.d/"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"
            ]

------------------------------------------------------------

fpathT ∷ TypeRep
fpathT = typeRep (Proxy ∷ Proxy FPath)

instance Parseable FPath where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η FPath
  parse (toText → t) =
    case null t of
      𝕿 → __FPathEmptyE__ fpathT
      𝕱 → case (head t, last t) of
            ('/','/') → FAbsD ⊳ parse t
            ('/',_  ) → FAbsF ⊳ parse t
            (_  ,'/') → FRelD ⊳ parse t
            (_  ,_  ) → FRelF ⊳ parse t

parseFPathTests ∷ TestTree
parseFPathTests =
  let success d f t =
        testCase t $ 𝕽 (d ⫥ f) @=? parse @FPath @FPathError t
   in testGroup "parseFPath"
                [ success [absdir|/|]      _AbsDir  "/"
                , success [absdir|/etc/|]  _AbsDir  "/etc/"
                , success [absfile|/quux|] _AbsFile "/quux"
                , success [reldir|foo/|]   _RelDir  "foo/"
                , success [relfile|bar|]   _RelFile "bar"
                ]

------------------------------------------------------------

{- | Class of things that are guaranteed convertable to an FPath (but that an
     FPath might or might not be able to convert to). -}
class (Printable α, AsFilePath α) ⇒ FPathAs α where
  _FPath ∷ Prism' FPath α

instance FPathAs FPath where
  _FPath = id

instance FPathAs AbsDir where
  _FPath = _AbsDir

instance FPathAs RelDir where
  _FPath = _RelDir

instance FPathAs AbsFile where
  _FPath = _AbsFile

instance FPathAs RelFile where
  _FPath = _RelFile

instance FPathAs Dir where
  _FPath = _Dir

instance FPathAs File where
  _FPath = _File

instance FPathAs Rel where
  _FPath = _Rel

instance FPathAs Abs where
  _FPath = _Abs

----------------------------------------

instance RelTypeC FPath where
  type RelType FPath = Rel

--------------------

instance DirTypeC FPath where
  type DirType FPath = Dir

--------------------

instance Basename FPath where
  basename ∷ FPath → Rel
  basename (FAbsD d) = RelD $ basename d
  basename (FAbsF f) = RelF $ basename f
  basename (FRelD d) = RelD $ basename d
  basename (FRelF f) = RelF $ basename f

  updateBasename ∷ (PathComponent → PathComponent) → FPath → FPath
  updateBasename g (FAbsD d) = FAbsD (updateBasename g d)
  updateBasename g (FAbsF f) = FAbsF (updateBasename g f)
  updateBasename g (FRelD d) = FRelD (updateBasename g d)
  updateBasename g (FRelF f) = FRelF (updateBasename g f)

----------

basenameTests ∷ TestTree
basenameTests =
  testGroup "basename"
            [ testCase "a0d" $ RelD [reldir|./|]     ≟ basename a0d
            , testCase "a1d" $ RelD [reldir|etc/|]   ≟ basename a1d
            , testCase "a2d" $ RelD [reldir|pam.d/|] ≟ basename a2d
            , testCase "a3d" $ RelD [reldir|M/|]     ≟ basename a3d
            , testCase "a1f" $ RelF [relfile|r.e|]   ≟ basename a1f
            , testCase "a2f" $ RelF [relfile|p.x|]   ≟ basename a2f
            , testCase "a3f" $ RelF [relfile|r.mp3|] ≟ basename a3f
            , testCase "a4f" $ RelF [relfile|.x|]    ≟ basename a4f
            , testCase "r0d" $ RelD [reldir|./|]     ≟ basename r0d
            , testCase "r1d" $ RelD [reldir|r/|]     ≟ basename r1d
            , testCase "r2d" $ RelD [reldir|p/|]     ≟ basename r2d
            , testCase "r3d" $ RelD [reldir|r/|]     ≟ basename r3d
            , testCase "r1f" $ RelF [relfile|r.e|]   ≟ basename r1f
            , testCase "r2f" $ RelF [relfile|p.x|]   ≟ basename r2f
            , testCase "r3f" $ RelF [relfile|r.mp3|] ≟ basename r3f
            , testCase "r4f" $ RelF [relfile|.x|]    ≟ basename r4f
            ]
----------

updateBasenameTests ∷ TestTree
updateBasenameTests =
  let test input expect =
        testCase (toString input) $ expect ≟ updateBasename toUpper input
   in testGroup "updateBasename"
            [ test a0d a0d
            , test a1d (FAbsD [absdir|/ETC/|])
            , test a2d (FAbsD [absdir|/etc/PAM.D/|])
            , test a3d (FAbsD [absdir|/w/g/M/|])
            , test a1f (FAbsF [absfile|/R.E|])
            , test a2f (FAbsF [absfile|/r/P.X|])
            , test a3f (FAbsF [absfile|/p/q/R.MP3|])
            , test a4f (FAbsF [absfile|/.X|])
            , test r0d r0d
            , test r1d (FRelD [reldir|R/|])
            , test r2d (FRelD [reldir|r/P/|])
            , test r3d (FRelD [reldir|p/q/R/|])
            , test r1f (FRelF [relfile|R.E|])
            , test r2f (FRelF [relfile|r/P.X|])
            , test r3f (FRelF [relfile|p/q/R.MP3|])
            , test r4f (FRelF [relfile|.X|])
            ]

--------------------

instance HasDirname FPath where
  dirname ∷ Lens' FPath Dir
  dirname = lens (\ case (FAbsD d) → DirA $ d ⊣ dirname
                         (FAbsF f) → DirA $ f ⊣ dirname
                         (FRelD d) → DirR $ d ⊣ dirname
                         (FRelF f) → DirR $ f ⊣ dirname
                 )
                 (\ a d → case (a,d) of
                             (FAbsD f, DirA n) → FAbsD $ f ⅋ dirname ⊢ n
                             (FAbsD f, DirR n) → FRelD $ n ⫻ (basename f)
                             (FAbsF f, DirA n) → FAbsF $ f ⅋ dirname ⊢ n
                             (FAbsF f, DirR n) → FRelF $ n ⫻ (basename f)
                             (FRelD f, DirA n) → FAbsD $ n ⫻ (basename f)
                             (FRelD f, DirR n) → FRelD $ f ⅋ dirname ⊢ n
                             (FRelF f, DirA n) → FAbsF $ n ⫻ (basename f)
                             (FRelF f, DirR n) → FRelF $ f ⅋ dirname ⊢ n
                 )

  ancestors' ∷ FPath → [Dir]
  ancestors' (FRelD d) = DirR ⊳ ancestors' d
  ancestors' (FRelF f) = DirR ⊳ ancestors' f
  ancestors' (FAbsD d) = DirA ⊳ ancestors' d
  ancestors' (FAbsF f) = DirA ⊳ ancestors' f

----------

dirnameTests ∷ TestTree
dirnameTests =
  testGroup "dirname"
            [ testCase "a0d" $ DirA [absdir|/|]     ≟ a0d ⊣ dirname
            , testCase "a1d" $ DirA [absdir|/|]     ≟ a1d ⊣ dirname
            , testCase "a2d" $ DirA [absdir|/etc/|] ≟ a2d ⊣ dirname
            , testCase "a3d" $ DirA [absdir|/w/g/|] ≟ a3d ⊣ dirname

            , testCase "a1f" $ DirA [absdir|/|]     ≟ a1f ⊣ dirname
            , testCase "a2f" $ DirA [absdir|/r/|]   ≟ a2f ⊣ dirname
            , testCase "a3f" $ DirA [absdir|/p/q/|] ≟ a3f ⊣ dirname
            , testCase "a4f" $ DirA [absdir|/|]     ≟ a4f ⊣ dirname

            , testCase "r0d" $ DirR [reldir|./|]   ≟ r0d ⊣ dirname
            , testCase "r1d" $ DirR [reldir|./|]   ≟ r1d ⊣ dirname
            , testCase "r2d" $ DirR [reldir|r/|]   ≟ r2d ⊣ dirname
            , testCase "r3d" $ DirR [reldir|p/q/|] ≟ r3d ⊣ dirname

            , testCase "r1f" $ DirR [reldir|./|]   ≟ r1f ⊣ dirname
            , testCase "r2f" $ DirR [reldir|r/|]   ≟ r2f ⊣ dirname
            , testCase "r3f" $ DirR [reldir|p/q/|] ≟ r3f ⊣ dirname
            , testCase "r4f" $ DirR [reldir|./|]   ≟ r4f ⊣ dirname

            , testCase "a0d←/" $
                FAbsD [absdir|/|]     ≟ a0d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "a0d←/p/" $
                FAbsD [absdir|/p/|]   ≟ a0d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "a0d←/p/q/" $
                FAbsD [absdir|/p/q/|] ≟ a0d ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "a0d←./" $
                FRelD [reldir|./|]   ≟ a0d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "a0d←/p/" $
                FRelD [reldir|p/|]   ≟ a0d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "a0d←/p/q/" $
                FRelD [reldir|p/q/|] ≟ a0d ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "a1f←./" $
                FAbsF [absfile|/r.e|]     ≟ a1f ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "a1f←p/" $
                FAbsF [absfile|/p/r.e|]   ≟ a1f ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "a1f←p/q/" $
                FAbsF [absfile|/p/q/r.e|] ≟ a1f ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "a1f←./" $
                FRelF [relfile|r.e|]     ≟ a1f ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "a1f←p/" $
                FRelF [relfile|p/r.e|]   ≟ a1f ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "a1f←p/q/" $
                FRelF [relfile|p/q/r.e|] ≟ a1f ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "a1d←/" $
                FAbsD [absdir|/etc/|]     ≟ a1d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "a1d←/p/" $
                FAbsD [absdir|/p/etc/|]   ≟ a1d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "a1d←/p/q/" $
                FAbsD [absdir|/p/q/etc/|] ≟ a1d ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "a1d←./" $
                FRelD [reldir|etc/|]     ≟ a1d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "a1d←p/" $
                FRelD [reldir|p/etc/|]   ≟ a1d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "a1d←p/q/" $
                FRelD [reldir|p/q/etc/|] ≟ a1d ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "a2f←/" $
                FAbsF [absfile|/p.x|]     ≟ a2f ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "a2f←/p/" $
                FAbsF [absfile|/p/p.x|]   ≟ a2f ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "a2f←/p/q/" $
                FAbsF [absfile|/p/q/p.x|] ≟ a2f ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "a2f←./" $
                FRelF [relfile|p.x|]     ≟ a2f ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "a2f←p/" $
                FRelF [relfile|p/p.x|]   ≟ a2f ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "a2f←p/q/" $
                FRelF [relfile|p/q/p.x|] ≟ a2f ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "a2d←/" $
                FAbsD [absdir|/pam.d/|]     ≟ a2d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "a2d←/p/" $
                FAbsD [absdir|/p/pam.d/|]   ≟ a2d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "a2d←/p/q/" $
                FAbsD [absdir|/p/q/pam.d/|] ≟ a2d ⅋ dirname ⊢ DirA [absdir|/p/q/|]
            , testCase "a2d←./" $
                FRelD [reldir|pam.d/|]     ≟ a2d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "a2d←p/" $
                FRelD [reldir|p/pam.d/|]   ≟ a2d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "a2d←p/q/" $
                FRelD [reldir|p/q/pam.d/|] ≟ a2d ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "a3f←/" $
                FAbsF [absfile|/r.mp3|]     ≟ a3f ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "a3f←/p/" $
                FAbsF [absfile|/p/r.mp3|]   ≟ a3f ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "a3f←/p/q/" $
                FAbsF [absfile|/p/q/r.mp3|] ≟ a3f ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "a3f←./" $
                FRelF [relfile|r.mp3|]     ≟ a3f ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "a3f←p/" $
                FRelF [relfile|p/r.mp3|]   ≟ a3f ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "a3f←p/q/" $
                FRelF [relfile|p/q/r.mp3|] ≟ a3f ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "a3d←/" $
                FAbsD [absdir|/M/|]     ≟ a3d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "a3d←/p/" $
                FAbsD [absdir|/p/M/|]   ≟ a3d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "a3d←/p/q/" $
                FAbsD [absdir|/p/q/M/|] ≟ a3d ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "a3d←./" $
                FRelD [reldir|M/|]     ≟ a3d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "a3d←p/" $
                FRelD [reldir|p/M/|]   ≟ a3d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "a3d←p/q/" $
                FRelD [reldir|p/q/M/|] ≟ a3d ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "a4f←/" $
                FAbsF [absfile|/.x|]     ≟ a4f ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "a4f←/p/" $
                FAbsF [absfile|/p/.x|]   ≟ a4f ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "a4f←/p/q/" $
                FAbsF [absfile|/p/q/.x|] ≟ a4f ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "a4f←./" $
                FRelF [relfile|.x|]     ≟ a4f ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "a4f←p/" $
                FRelF [relfile|p/.x|]   ≟ a4f ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "a4f←p/q/" $
                FRelF [relfile|p/q/.x|] ≟ a4f ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "r0d←/" $
                FAbsD [absdir|/|]     ≟ r0d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "r0d←/p/" $
                FAbsD [absdir|/p/|]   ≟ r0d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "r0d←/p/q/" $
                FAbsD [absdir|/p/q/|] ≟ r0d ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "r0d←./" $
                FRelD [reldir|./|]   ≟ r0d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "r0d←/p/" $
                FRelD [reldir|p/|]   ≟ r0d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "r0d←/p/q/" $
                FRelD [reldir|p/q/|] ≟ r0d ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "r1f←./" $
                FAbsF [absfile|/r.e|]     ≟ r1f ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "r1f←p/" $
                FAbsF [absfile|/p/r.e|]   ≟ r1f ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "r1f←p/q/" $
                FAbsF [absfile|/p/q/r.e|] ≟ r1f ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "r1f←./" $
                FRelF [relfile|r.e|]     ≟ r1f ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "r1f←p/" $
                FRelF [relfile|p/r.e|]   ≟ r1f ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "r1f←p/q/" $
                FRelF [relfile|p/q/r.e|] ≟ r1f ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "r1d←/" $
                FAbsD [absdir|/r/|]     ≟ r1d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "r1d←/p/" $
                FAbsD [absdir|/p/r/|]   ≟ r1d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "r1d←/p/q/" $
                FAbsD [absdir|/p/q/r/|] ≟ r1d ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "r1d←./" $
                FRelD [reldir|r/|]     ≟ r1d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "r1d←p/" $
                FRelD [reldir|p/r/|]   ≟ r1d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "r1d←p/q/" $
                FRelD [reldir|p/q/r/|] ≟ r1d ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "r2f←/" $
                FAbsF [absfile|/p.x|]     ≟ r2f ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "r2f←/p/" $
                FAbsF [absfile|/p/p.x|]   ≟ r2f ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "r2f←/p/q/" $
                FAbsF [absfile|/p/q/p.x|] ≟ r2f ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "r2f←./" $
                FRelF [relfile|p.x|]     ≟ r2f ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "r2f←p/" $
                FRelF [relfile|p/p.x|]   ≟ r2f ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "r2f←p/q/" $
                FRelF [relfile|p/q/p.x|] ≟ r2f ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "r2d←/" $
                FAbsD [absdir|/p/|]     ≟ r2d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "r2d←/p/" $
                FAbsD [absdir|/p/p/|]   ≟ r2d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "r2d←/p/q/" $
                FAbsD [absdir|/p/q/p/|] ≟ r2d ⅋ dirname ⊢ DirA [absdir|/p/q/|]
            , testCase "r2d←./" $
                FRelD [reldir|p/|]     ≟ r2d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "r2d←p/" $
                FRelD [reldir|p/p/|]   ≟ r2d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "r2d←p/q/" $
                FRelD [reldir|p/q/p/|] ≟ r2d ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "r3f←/" $
                FAbsF [absfile|/r.mp3|]     ≟ r3f ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "r3f←/p/" $
                FAbsF [absfile|/p/r.mp3|]   ≟ r3f ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "r3f←/p/q/" $
                FAbsF [absfile|/p/q/r.mp3|] ≟ r3f ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "r3f←./" $
                FRelF [relfile|r.mp3|]     ≟ r3f ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "r3f←p/" $
                FRelF [relfile|p/r.mp3|]   ≟ r3f ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "r3f←p/q/" $
                FRelF [relfile|p/q/r.mp3|] ≟ r3f ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "r3d←/" $
                FAbsD [absdir|/r/|]     ≟ r3d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "r3d←/p/" $
                FAbsD [absdir|/p/r/|]   ≟ r3d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "r3d←/p/q/" $
                FAbsD [absdir|/p/q/r/|] ≟ r3d ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "r3d←./" $
                FRelD [reldir|r/|]     ≟ r3d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "r3d←p/" $
                FRelD [reldir|p/r/|]   ≟ r3d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "r3d←p/q/" $
                FRelD [reldir|p/q/r/|] ≟ r3d ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "r4f←/" $
                FAbsF [absfile|/.x|]     ≟ r4f ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "r4f←/p/" $
                FAbsF [absfile|/p/.x|]   ≟ r4f ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "r4f←/p/q/" $
                FAbsF [absfile|/p/q/.x|] ≟ r4f ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "r4f←./" $
                FRelF [relfile|.x|]     ≟ r4f ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "r4f←p/" $
                FRelF [relfile|p/.x|]   ≟ r4f ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "r4f←p/q/" $
                FRelF [relfile|p/q/.x|] ≟ r4f ⅋ dirname ⊢ DirR [reldir|p/q/|]
            ]


----------------------------------------

type instance Element FPath = PathComponent

----------------------------------------

instance MonoFunctor FPath where
  omap ∷ (PathComponent → PathComponent) → FPath → FPath
  omap g (FAbsF f) = FAbsF (omap g f)
  omap g (FAbsD d) = FAbsD (omap g d)
  omap g (FRelF f) = FRelF (omap g f)
  omap g (FRelD d) = FRelD (omap g d)

----------------------------------------

instance MonoFoldable FPath where
  otoList ∷ FPath → [PathComponent]
  otoList (FAbsF f) = otoList f
  otoList (FAbsD d) = otoList d
  otoList (FRelF f) = otoList f
  otoList (FRelD d) = otoList d

  ofoldl' ∷ (α → PathComponent → α) → α → FPath → α
  ofoldl' g x (FAbsF f) = ofoldl' g x f
  ofoldl' g x (FAbsD d) = ofoldl' g x d
  ofoldl' g x (FRelF f) = ofoldl' g x f
  ofoldl' g x (FRelD d) = ofoldl' g x d

  ofoldr ∷ (PathComponent → α → α) → α → FPath → α
  ofoldr g x (FAbsF f) = ofoldr g x f
  ofoldr g x (FAbsD d) = ofoldr g x d
  ofoldr g x (FRelF f) = ofoldr g x f
  ofoldr g x (FRelD d) = ofoldr g x d

  ofoldMap ∷ Monoid ν => (PathComponent → ν) → FPath → ν
  ofoldMap g (FAbsF f) = ofoldMap g f
  ofoldMap g (FAbsD d) = ofoldMap g d
  ofoldMap g (FRelF f) = ofoldMap g f
  ofoldMap g (FRelD d) = ofoldMap g d

  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → FPath
            → PathComponent
  ofoldr1Ex g (FAbsF f) = ofoldr1Ex g f
  ofoldr1Ex g (FAbsD d) = ofoldr1Ex g d
  ofoldr1Ex g (FRelF f) = ofoldr1Ex g f
  ofoldr1Ex g (FRelD d) = ofoldr1Ex g d

  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → FPath
             → PathComponent
  ofoldl1Ex' g (FAbsF f) = ofoldl1Ex' g f
  ofoldl1Ex' g (FAbsD d) = ofoldl1Ex' g d
  ofoldl1Ex' g (FRelF f) = ofoldl1Ex' g f
  ofoldl1Ex' g (FRelD d) = ofoldl1Ex' g d

----------------------------------------

instance ToSeq FPath where
  toSeq (FAbsF f) = toSeq f
  toSeq (FAbsD d) = toSeq d
  toSeq (FRelF f) = toSeq f
  toSeq (FRelD d) = toSeq d

----------------------------------------

instance Arbitrary FPath where
  arbitrary ∷ Gen FPath
  arbitrary = oneof [ FAbsF ⊳ arbitrary @AbsFile, FAbsD ⊳ arbitrary @AbsDir
                    , FRelF ⊳ arbitrary @RelFile, FRelD ⊳ arbitrary @RelDir ]
  shrink ∷ FPath → [FPath]
  shrink (FAbsF f) = FAbsF ⊳ shrink f
  shrink (FAbsD d) = FAbsD ⊳ shrink d
  shrink (FRelF f) = FRelF ⊳ shrink f
  shrink (FRelD d) = FRelD ⊳ shrink d

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

r1f ∷ FPath
r1f = FRelF rf1

r2f ∷ FPath
r2f = FRelF rf2

r3f ∷ FPath
r3f = FRelF rf3

r4f ∷ FPath
r4f = FRelF rf4

r0d ∷ FPath
r0d = FRelD r0

r1d ∷ FPath
r1d = FRelD r1

r2d ∷ FPath
r2d = FRelD r2

r3d ∷ FPath
r3d = FRelD r3

a0d ∷ FPath
a0d = FAbsD root

a1d ∷ FPath
a1d = FAbsD etc

a2d ∷ FPath
a2d = FAbsD pamd

a3d ∷ FPath
a3d = FAbsD wgm

a1f ∷ FPath
a1f = FAbsF af1

a2f ∷ FPath
a2f = FAbsF af2

a3f ∷ FPath
a3f = FAbsF af3

a4f ∷ FPath
a4f = FAbsF af4

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath" [ basenameTests, updateBasenameTests, dirnameTests
                          , filepathTests, parseFPathTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
