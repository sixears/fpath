module FPath.Dir
  ( AsDir( _Dir ), Dir(..), DirAs( _Dir_ )

  , tests
  )
where

import Base1T  hiding  ( head )

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

-- more-unicode-symbols ----------------

import Data.MoreUnicode.Function  ( (⅋) )
import Data.MoreUnicode.Lens      ( (⊩), (##) )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqConversions  ( ToSeq( toSeq ) )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( Gen, oneof )

-- safe --------------------------------

import Safe  ( headDef )

-- tasty-plus --------------------------

import TastyPlus  ( (≟) )

-- text --------------------------------

import Data.Text  ( head, null )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir            ( AbsDir, AsAbsDir( _AbsDir)
                               , AbsDirAs( _AbsDir_ )
                               , AsNonRootAbsDir( _NonRootAbsDir )
                               , NonRootAbsDir

                               , absdir
                               )
import FPath.AppendableFPath   ( AppendableFPath( (⫻) ) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.AsFilePath'       ( AsFilePath'( filepath' ) )
import FPath.Basename          ( Basename( basename, updateBasename) )
import FPath.Dirname           ( HasDirname( ancestors', dirname ) )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parent            ( HasParentMay( parentMay, parents, parents' ) )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( PathComponent, toUpper )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir, reldir )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.T.FPath.TestData  ( etc, pamd, a0, a1, a2, a3, r0, r1, r2, r3, root
                               , wgm )

-------------------------------------------------------------------------------

data Dir = DirA AbsDir | DirR RelDir
  deriving (Eq, Show)

--------------------

instance Ord Dir where
  a <= b = toText a ≤ toText b

--------------------

{-| Things that may convert to a `Dir` (but a `Dir` will always convert
    to); e.g., @FPath@. -}
-- we don't really need the Printable, AsFilePath requirements here; rather,
-- they should be true of all fpathish things, and including them here makes
-- many function type signatures simpler
class (Printable α, AsFilePath α) ⇒ AsDir α where
  _Dir ∷ Prism' α Dir

instance AsDir Dir where
  _Dir = id

--------------------

{-| Things that /may/ be converted from a `Dir` (but will always convert /to/ a
    `Dir`), e.g., @AbsDir@, @RelDir@. -}
-- we don't really need the Printable, AsFilePath requirements here; rather,
-- they should be true of all fpathish things, and including them here makes
-- many function type signatures simpler
class (Printable α, AsFilePath α) ⇒ DirAs α where
  _Dir_ ∷ Prism' Dir α
instance DirAs Dir where
  _Dir_ = id
instance DirAs AbsDir where
  _Dir_ = _AbsDir
instance DirAs RelDir where
  _Dir_ = _RelDir

--------------------

instance RelTypeC Dir where
  type RelType Dir = RelDir

--------------------

instance DirTypeC Dir where
  type DirType Dir = Dir

--------------------

instance AsAbsDir Dir where
  _AbsDir ∷ Prism' Dir AbsDir
  _AbsDir = prism' DirA (\ case (DirA d) → Just d; _ → Nothing)

--------------------

instance AsNonRootAbsDir Dir where
  _NonRootAbsDir ∷ Prism' Dir NonRootAbsDir
  _NonRootAbsDir = prism' (DirA ∘ review _AbsDir_)
                          (\ case (DirA d) → d ⩼ _NonRootAbsDir; _ → Nothing)

--------------------

instance AsRelDir Dir where
  _RelDir ∷ Prism' Dir RelDir
  _RelDir = prism' DirR (\ case (DirR d) → Just d; _ → Nothing)

----------------------------------------

instance Printable Dir where
  print (DirA f) = print f
  print (DirR f) = print f

instance Textual Dir where
  textual = DirA ⊳ textual ∤ DirR ⊳ textual

----------------------------------------

instance AsFilePath Dir where
  filepath = prism' toString fromString

--------------------

filepathTests ∷ TestTree
filepathTests =
  let nothin' = Nothing ∷ Maybe Dir
      fail s  = testCase s $ nothin' @=? s ⩼ filepath
   in testGroup "filepath"
            [ testCase "root"  $ "/"           ≟ DirA root    ## filepath
            , testCase "etc"   $ "/etc/"       ≟ DirA etc     ## filepath
            , testCase "pam.d" $ "/etc/pam.d/" ≟ DirA pamd    ## filepath
            , testCase "wgm"   $ "/w/g/M/"     ≟ DirA wgm     ## filepath

            , testCase "/etc/" $ Just etc      @=? "/etc/" ⩼ filepath

            , testCase "r0" $ "./"     ≟ DirR r0 ## filepath
            , testCase "r1" $ "r/"     ≟ DirR r1 ## filepath
            , testCase "r2" $ "r/p/"   ≟ DirR r2 ## filepath
            , testCase "r3" $ "p/q/r/" ≟ DirR r3 ## filepath

            , testCase "r0" $ Just (DirR r0) @=? "./"     ⩼ filepath
            , testCase "r1" $ Just (DirR r1) @=? "r/"     ⩼ filepath
            , testCase "r2" $ Just (DirR r2) @=? "r/p/"   ⩼ filepath
            , testCase "r3" $ Just (DirR r3) @=? "p/q/r/" ⩼ filepath

            , fail "/etc"
            , fail "foo/etc"
            , fail "etc"
            , fail "/etc/pam.d"
            , fail "etc/pam.d"
            , fail "/etc//pam.d/"
            , fail "e/c"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"
            ]

--------------------

instance AsFilePath' Dir where
  filepath' = prism' (\ case DirA f → f ⫥ filepath'; DirR f → f ⫥ filepath')
                     (\ fp → case headDef '/' fp of
                               '/' → (DirA ⊳ fp ⩼ filepath')
                               _   → (DirR ⊳ fp ⩼ filepath')
                     )

----------

asFilePath'Tests ∷ TestTree
asFilePath'Tests =
  let testGet expect input =
        testCase (toString input) $ expect ≟ input ⫥ filepath'
      testSet expect input =
        testCase (toString input) $ Just expect @=? input ⩼ filepath'
   in testGroup "asFilePath'"
                [ testGroup "get"
                             [ testGet "/"      a0d
                             , testGet "/r"     a1d
                             , testGet "/r/p"   a2d
                             , testGet "/p/q/r" a3d
                             , testGet "."      r0d
                             , testGet "r"      r1d
                             , testGet "r/p"    r2d
                             , testGet "p/q/r"  r3d
                             ]
                , testGroup "set"
                             [ testSet a0d "/"
                             , testSet a1d "/r"
                             -- should still cope with trailing '/'
                             , testSet a2d "/r/p/"
                             , testSet a3d "/p/q/r"
                             , testSet r0d "."
                             , testSet r1d "r"
                             , testSet r2d "r/p"
                             -- should still cope with trailing '/'
                             , testSet r3d "p/q/r/"
                             ]

                ]

--------------------

instance Basename Dir where
  basename (DirA d) = basename d
  basename (DirR d) = basename d
  updateBasename u (DirA d) = DirA $ updateBasename u d
  updateBasename u (DirR d) = DirR $ updateBasename u d

----------

basenameTests ∷ TestTree
basenameTests =
  testGroup "basename"
            [ testCase "a0d" $ r0           ≟ basename a0d
            , testCase "a1d" $ r1           ≟ basename a1d
            , testCase "a2d" $ [reldir|p/|] ≟ basename a2d
            , testCase "a3d" $ r1           ≟ basename a3d
            , testCase "r0d" $ r0           ≟ basename r0d
            , testCase "r1d" $ r1           ≟ basename r1d
            , testCase "r2d" $ [reldir|p/|] ≟ basename r2d
            , testCase "r3d" $ r1           ≟ basename r3d
            ]

----------

updateBasenameTests ∷ TestTree
updateBasenameTests =
  let
      test input expect =
        testCase (toString input) $ expect ≟ updateBasename toUpper input
   in testGroup "updateBasename"
            [ test a0d a0d
            , test a1d (DirA [absdir|/R/|])
            , test a2d (DirA [absdir|/r/P/|])
            , test a3d (DirA [absdir|/p/q/R/|])
            , test r0d (DirR [reldir|./|])
            , test r1d (DirR [reldir|R/|])
            , test r2d (DirR [reldir|r/P/|])
            , test r3d (DirR [reldir|p/q/R/|])
            ]

--------------------

instance HasDirname Dir where
  dirname ∷ Lens' Dir Dir
  dirname = lens ( \ case (DirA fn) → DirA $ fn ⊣ dirname
                          (DirR fn) → DirR $ fn ⊣ dirname; )
                 ( \ f d → case (f,d) of
                             ((DirA fn),(DirA dn)) → DirA $ fn ⅋ dirname ⊢ dn
                             ((DirA _),(DirR dn)) → DirR $ dn ⫻ basename f
                             ((DirR fn),(DirR dn)) → DirR $ fn ⅋ dirname ⊢ dn
                             ((DirR _),(DirA dn)) → DirA $ dn ⫻ basename f
                 )

  ancestors' ∷ Dir → [Dir]
  ancestors' (DirA a) = DirA ⊳ ancestors' a
  ancestors' (DirR r) = DirR ⊳ ancestors' r

----------

dirnameTests ∷ TestTree
dirnameTests =
  testGroup "dirname"
            [ testCase "a0d" $ DirA [absdir|/|]     ≟ a0d ⊣ dirname
            , testCase "a1d" $ DirA [absdir|/|]     ≟ a1d ⊣ dirname
            , testCase "a2d" $ DirA [absdir|/r/|]   ≟ a2d ⊣ dirname
            , testCase "a3d" $ DirA [absdir|/p/q/|] ≟ a3d ⊣ dirname

            , testCase "r0d" $ DirR [reldir|./|]   ≟ r0d ⊣ dirname
            , testCase "r1d" $ DirR [reldir|./|]   ≟ r1d ⊣ dirname
            , testCase "r2d" $ DirR [reldir|r/|]   ≟ r2d ⊣ dirname
            , testCase "r3d" $ DirR [reldir|p/q/|] ≟ r3d ⊣ dirname

            , testCase "a0d←/" $
                DirA [absdir|/|]     ≟ a0d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "a0d←/p/" $
                DirA [absdir|/p/|]   ≟ a0d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "a0d←/p/q/" $
                DirA [absdir|/p/q/|] ≟ a0d ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "a0d←./" $
                DirR [reldir|./|]   ≟ a0d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "a0d←p/" $
                DirR [reldir|p/|]   ≟ a0d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "a0d←p/q/" $
                DirR [reldir|p/q/|] ≟ a0d ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "a1d←/" $
                DirA [absdir|/r/|]     ≟ a1d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "a1d←/p/" $
                DirA [absdir|/p/r/|]   ≟ a1d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "a1d←/p/q/" $
                DirA [absdir|/p/q/r/|] ≟ a1d ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "a1d←./" $
                DirR [reldir|r/|]     ≟ a1d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "a1d←p/" $
                DirR [reldir|p/r/|]   ≟ a1d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "a1d←p/q/" $
                DirR [reldir|p/q/r/|] ≟ a1d ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "r0d←/" $
                DirA [absdir|/|]     ≟ r0d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "r0d←/p/" $
                DirA [absdir|/p/|]   ≟ r0d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "r0d←/p/q/" $
                DirA [absdir|/p/q/|] ≟ r0d ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "r0d←./" $
                DirR [reldir|./|]   ≟ r0d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "r0d←p/" $
                DirR [reldir|p/|]   ≟ r0d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "r0d←p/q/" $
                DirR [reldir|p/q/|] ≟ r0d ⅋ dirname ⊢ DirR [reldir|p/q/|]

            , testCase "r1d←/" $
                DirA [absdir|/r/|]     ≟ r1d ⅋ dirname ⊢ DirA [absdir|/|]
            , testCase "r1d←/p/" $
                DirA [absdir|/p/r/|]   ≟ r1d ⅋ dirname ⊢ DirA [absdir|/p/|]
            , testCase "r1d←/p/q/" $
                DirA [absdir|/p/q/r/|] ≟ r1d ⅋ dirname ⊢ DirA [absdir|/p/q/|]

            , testCase "r1d←./" $
                DirR [reldir|r/|]     ≟ r1d ⅋ dirname ⊢ DirR [reldir|./|]
            , testCase "r1d←p/" $
                DirR [reldir|p/r/|]   ≟ r1d ⅋ dirname ⊢ DirR [reldir|p/|]
            , testCase "r1d←p/q/" $
                DirR [reldir|p/q/r/|] ≟ r1d ⅋ dirname ⊢ DirR [reldir|p/q/|]
            ]

--------------------

instance HasParentMay Dir where
  parentMay = lens get set
              where get ∷ Dir → Maybe Dir
                    get (DirA d) = DirA ⊳ d ⊣ parentMay
                    get (DirR d) = DirR ⊳ d ⊣ parentMay
                    set ∷ Dir → Maybe Dir → Dir
                    set (DirA d) (Just (DirA p)) =
                      DirA $ d & parentMay ⊢ Just p
                    set (DirA d) (Just (DirR p)) =
                      DirR $ p ⫻ basename d
                    set (DirA d) Nothing =
                      DirA $ d & parentMay ⊢ Nothing
                    set (DirR d) (Just (DirR p)) =
                      DirR $ d & parentMay ⊢ Just p
                    set (DirR d) (Just (DirA p)) =
                      DirA $ p ⫻ basename d
                    set (DirR d) Nothing =
                      DirR $ d & parentMay ⊢ Nothing

----------

parentMayGetTests ∷ TestTree
parentMayGetTests =
  let getTest expect input =
        testCase (toString input) $ expect @=? input ⊣ parentMay
   in testGroup "get"
                [ getTest Nothing                       a0d
                , getTest (Just a0d)                    a1d
                , getTest (Just $ DirA [absdir|/r/|])   a2d
                , getTest (Just $ DirA [absdir|/p/q/|]) a3d
                , getTest Nothing                       r0d
                , getTest (Just $ DirR [reldir|./|])    r1d
                , getTest (Just $ DirR [reldir|r/|])    r2d
                , getTest (Just $ DirR [reldir|p/q/|])  r3d
                ]

----------

parentMaySetTests ∷ TestTree
parentMaySetTests =
  let -- (~~) ∷ α → α → α
      -- set d's parent to Just d'
      d ~~ d' = d & parentMay ⊩ d'
      -- set d's parent to d'
      d *~ d' = d & parentMay ⊢ d'
      setTest expect got = testCase (toString expect) $ expect @=? got
   in testGroup "set"
                [ testGroup "abs/abs"
                            [ -- setting root's parent to root → root
                              setTest a0d (a0d ~~ a0d)
                            , setTest a0d (a0d *~ Nothing)

                              -- setting root's parent to /r/ → /r/
                            , setTest a1d (a0d ~~ a1d)

                              -- setting /r/'s parent to root → /r/
                            , setTest a1d (a1d *~ Nothing)
                            , setTest a1d (a1d ~~ a0d)

                              -- setting /r/p/'s parent to root → /p/
                            , setTest (DirA [absdir|/p/|]) (a2d ~~ a0d)

                              -- setting /r/p/'s parent to /q/s/ → /q/s/p/
                            , setTest (DirA [absdir|/q/s/p/|])
                                      (a2d ~~ DirA [absdir|/q/s/|])

                              -- setting /p/q/r/'s parent to /r/ → /r/r/
                            , setTest (DirA [absdir|/r/r/|]) (a3d ~~ a1d)
                              -- setting /p/q/r/'s parent to /p/q/r/ → /p/q/r/r/
                            , setTest (DirA [absdir|/p/q/r/r/|]) (a3d ~~ a3d)
                            ]

                , testGroup "rel/rel"
                            [ setTest r0d (r0d ~~ r0d)
                            , setTest r0d (r0d *~ Nothing)
                            , setTest r1d (r0d ~~ r1d)
                            , setTest r1d (r1d *~ Nothing)
                            , setTest r1d (r1d ~~ r0d)
                            , setTest (DirR [reldir|p/|]) (r2d ~~ r0d)
                            , setTest (DirR [reldir|q/s/p/|])
                                      (r2d ~~ DirR [reldir|q/s/|])
                            , setTest (DirR [reldir|r/r/|]) (r3d ~~ r1d)
                            , setTest (DirR [reldir|p/q/r/r/|]) (r3d ~~ r3d)
                            ]

                  -- cross type dirs (abs-rel)  ------------
                , testGroup "abs/rel"
                            [ setTest r0d (a0d ~~ r0d)
                            , setTest r1d (a0d ~~ r1d)
                            , setTest r1d (a1d ~~ r0d)
                            , setTest (DirR [reldir|p/|]) (a2d ~~ r0d)
                            , setTest (DirR [reldir|q/s/p/|])
                                      (a2d ~~ DirR [reldir|q/s/|])
                            , setTest (DirR [reldir|r/r/|]) (a3d ~~ r1d)
                            , setTest (DirR [reldir|p/q/r/r/|]) (a3d ~~ r3d)
                            ]

                  -- cross type dirs (abs-rel)  ------------
                , testGroup "rel/abs"
                            [ setTest a0d (r0d ~~ a0d)
                            , setTest a1d (r0d ~~ a1d)
                            , setTest a1d (r1d ~~ a0d)
                            , setTest (DirA [absdir|/p/|]) (r2d ~~ a0d)
                            , setTest (DirA [absdir|/q/s/p/|])
                                      (r2d ~~ DirA [absdir|/q/s/|])
                            , setTest (DirA [absdir|/r/r/|]) (r3d ~~ a1d)
                            , setTest (DirA [absdir|/p/q/r/r/|]) (r3d ~~ a3d)
                            ]
                ]

----------

parentMayTests ∷ TestTree
parentMayTests = testGroup "parentMay" [ parentMayGetTests, parentMaySetTests ]

----------

parentsTests ∷ TestTree
parentsTests =
  let parentsTest expect input =
        testCase (toString input) $ expect @=? parents input
   in testGroup "parents"
            [ parentsTest []                                                a0d
            , parentsTest [ a0d ]                                           a1d
            , parentsTest [ a0d, DirA [absdir|/r/|] ]                       a2d
            , parentsTest [ a0d, DirA [absdir|/p/|], DirA [absdir|/p/q/|] ] a3d
            , parentsTest []                                                r0d
            , parentsTest [ r0d ]                                           r1d
            , parentsTest [ r0d, DirR [reldir|r/|] ]                        r2d
            , parentsTest [ r0d, DirR [reldir|p/|], DirR [reldir|p/q/|] ]   r3d
            ]

----------

parents'Tests ∷ TestTree
parents'Tests =
  let parents'Test expect input =
        testCase (toString input) $ expect @=? parents' input
   in testGroup "parents'"
            [ parents'Test [ a0d ]                                           a0d
            , parents'Test [ a0d, a1d ]                                      a1d
            , parents'Test [ a0d, DirA [absdir|/r/|], a2d ]                  a2d
            , parents'Test [a0d,DirA [absdir|/p/|],DirA [absdir|/p/q/|],a3d] a3d
            , parents'Test [ r0d ]                                           r0d
            , parents'Test [ r0d, r1d ]                                      r1d
            , parents'Test [ r0d, DirR [reldir|r/|], r2d ]                   r2d
            , parents'Test [ r0d,DirR [reldir|p/|],DirR [reldir|p/q/|],r3d ] r3d
            ]

----------------------------------------

type instance Element Dir = PathComponent

----------------------------------------

instance MonoFunctor Dir where
  omap ∷ (PathComponent → PathComponent) → Dir → Dir
  omap f (DirA a) = DirA (omap f a)
  omap f (DirR r) = DirR (omap f r)

----------------------------------------

instance MonoFoldable Dir where
  otoList ∷ Dir → [PathComponent]
  otoList (DirA a) = otoList a
  otoList (DirR r) = otoList r

  ofoldl' ∷ (α → PathComponent → α) → α → Dir → α
  ofoldl' f x (DirA a) = ofoldl' f x a
  ofoldl' f x (DirR r) = ofoldl' f x r

  ofoldr ∷ (PathComponent → α → α) → α → Dir → α
  ofoldr f x (DirA a) = ofoldr f x a
  ofoldr f x (DirR r) = ofoldr f x r

  ofoldMap ∷ Monoid ν => (PathComponent → ν) → Dir → ν
  ofoldMap f (DirA a) = ofoldMap f a
  ofoldMap f (DirR r) = ofoldMap f r

  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → Dir
            → PathComponent
  ofoldr1Ex f (DirA a) = ofoldr1Ex f a
  ofoldr1Ex f (DirR r) = ofoldr1Ex f r

  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → Dir
             → PathComponent
  ofoldl1Ex' f (DirA a) = ofoldl1Ex' f a
  ofoldl1Ex' f (DirR r) = ofoldl1Ex' f r

----------------------------------------

instance ToSeq Dir where
  toSeq (DirA a) = toSeq a
  toSeq (DirR r) = toSeq r

----------------------------------------

instance Arbitrary Dir where
  arbitrary ∷ Gen Dir
  arbitrary = oneof [DirA ⊳ arbitrary @AbsDir, DirR ⊳ arbitrary @RelDir]
  shrink ∷ Dir → [Dir]
  shrink (DirA a) = DirA ⊳ shrink a
  shrink (DirR r) = DirR ⊳ shrink r

----------------------------------------

dirT ∷ TypeRep
dirT = typeRep (Proxy ∷ Proxy Dir)

instance Parseable Dir where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η Dir
  parse (toText → t) =
    case null t of
      𝕿 → __FPathEmptyE__ dirT
      𝕱 → case head t of
                '/' → DirA ⊳ parse t
                _   → DirR ⊳ parse t

parseDirTests ∷ TestTree
parseDirTests =
  let success d f t = testCase t $ Right (d ## f) @=? parse @Dir @FPathError t
   in testGroup "parseDir"
                [ success [absdir|/|]     _AbsDir "/"
                , success [absdir|/etc/|] _AbsDir "/etc/"
                , success [reldir|./|]    _RelDir "./"
                , success [reldir|etc/|]  _RelDir "etc/"
                ]

----------------------------------------

instance AppendableFPath Dir RelDir Dir where
  (DirA d) ⫻ f = DirA $ (d ⫻ f)
  (DirR d) ⫻ f = DirR $ (d ⫻ f)

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

a0d ∷ Dir
a0d = DirA a0

a1d ∷ Dir
a1d = DirA a1

a2d ∷ Dir
a2d = DirA a2

a3d ∷ Dir
a3d = DirA a3

r0d ∷ Dir
r0d = DirR r0

r1d ∷ Dir
r1d = DirR r1

r2d ∷ Dir
r2d = DirR r2

r3d ∷ Dir
r3d = DirR r3

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath.Dir" [ filepathTests, asFilePath'Tests, basenameTests
                              , updateBasenameTests, dirnameTests
                              , parentMayTests, parentsTests, parents'Tests
                              , parseDirTests
                              ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
