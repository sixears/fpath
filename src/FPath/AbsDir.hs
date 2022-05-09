module FPath.AbsDir
  ( AbsDir, AbsDirAs( _AbsDir_ ), AsAbsDir( _AbsDir )
  , AsNonRootAbsDir( _NonRootAbsDir ), NonRootAbsDir

  , absdirT
  -- quasi-quoters
  , absdir, absdirN

  , nonRootAbsDir
  , root

  , parseAbsDirP , parseAbsDirP'  , __parseAbsDirP__

  , tests
  )
where

import Base1T  hiding  ( toList )
import Prelude  ( error )

-- base --------------------------------

import qualified  Data.List.NonEmpty  as  NonEmpty

import Data.Foldable        ( concat, foldMap )
import Data.Monoid          ( Monoid )
import Data.Ord             ( Ordering( GT ), comparing )
import Data.Typeable        ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Exts             ( IsList( toList ) )
import GHC.Generics         ( Generic )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode    ( (∅) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed ), Textual( textual )
                     , fromString, parseString )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- lens --------------------------------

import Control.Lens.Cons    ( unsnoc )
import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( iso )

-- monaderror-io -----------------------

import MonadError  ( ѭ )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element
                             , MonoFoldable( oall, oany, ocompareLength, oelem
                                           , ofoldl', ofoldl1Ex', ofoldlM
                                           , ofoldMap, ofoldMap1Ex, ofoldr
                                           , ofoldr1Ex, olength, olength64
                                           , onotElem, onull, otoList
                                           )
                             , MonoFunctor( omap )
                             , maximumByEx, minimumByEx, unsafeHead, unsafeLast
                             )
import Data.NonNull          ( fromNullable )

-- more-unicode ------------------------

import Data.MoreUnicode.Function         ( (⅋) )
import Data.MoreUnicode.Lens             ( (⊩), (⫣) )
import Data.MoreUnicode.MonoTraversable  ( (⪦), (⪧) )

-- non-empty-containers ----------------

import qualified  NonEmptyContainers.SeqConversions  as  SeqConversions
import qualified  NonEmptyContainers.SeqNE           as  SeqNE

import NonEmptyContainers.IsNonEmpty  ( FromMonoNonEmpty( fromNonEmpty )
                                      , IsMonoNonEmpty( nonEmpty )
                                      , ToMonoNonEmpty( toNonEmpty )
                                      , defaultNonEmpty
                                      )
import NonEmptyContainers.SeqConversions
                                    ( FromSeq( fromSeq ), IsSeq( seq )
                                    , ToSeq( toSeq ) )
import NonEmptyContainers.SeqNE     ( SeqNE( (:⫸) ), (⫸), (⪪) )
import NonEmptyContainers.SeqNEConversions
                                    ( FromSeqNonEmpty( fromSeqNE )
                                    , IsSeqNonEmpty( seqNE )
                                    , ToSeqNonEmpty( toSeqNE, toSeq_ ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char )
import Text.Parser.Combinators  ( endBy, endByNonEmpty )

-- quasiquoting ------------------------

import QuasiQuoting  ( QuasiQuoter, mkQQ, exp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- tasty-plus --------------------------

import TastyPlus  ( (≣), (≟), assertListEq, propInvertibleString
                  , propInvertibleText, propInvertibleUtf8 )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import qualified  Data.Text  as  Text
import Data.Text  ( Text, any, length, empty, splitOn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.AsFilePath'       ( AsFilePath'( filepath' )
                               , exterminate, terminate )
import FPath.Basename          ( Basename( basename, updateBasename ) )
import FPath.Dirname           ( Ancestors( ancestors )
                               , HasDirname( ancestors', dirname ) )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathComponentError
                               ( FPathComponentError
                               , fPathComponentEmptyE
                               , fPathComponentIllegalCharE
                               )
import FPath.Error.FPathError  ( AsFPathError, FPathError( FPathComponentE )
                               , __FPathComponentE__
                               , __FPathEmptyE__
                               , __FPathNonAbsE__
                               , __FPathNotADirE__
                               , __FPathRootDirE__
                               , fPathEmptyE, fPathNonAbsE, fPathNotADirE
                               , fPathRootDirE
                               , mapTypeRepE
                               )
import FPath.Parent            ( HasParent( parent )
                               , HasParentMay( parentMay, parents ) )
import FPath.Parseable         ( Parseable( parse, parse' ) )
import FPath.PathComponent     ( PathComponent, parsePathC, pc, stub, toUpper )
import FPath.RelDir            ( RelDir, reldir )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.Util              ( __ERROR'__ )

-------------------------------------------------------------------------------

{- | A non-root absolute directory, e.g., /etc. -}
-- a non-root dir is a path component appended to a (possibly-root) absolute
-- directory
newtype NonRootAbsDir = NonRootAbsDir (SeqNE PathComponent)
  deriving (Eq,Generic,NFData,Lift)

type instance Element NonRootAbsDir = PathComponent

instance Show NonRootAbsDir where
  show r = [fmt|[absdir|%T%s]|] (toText r) "|"

showTests ∷ TestTree
showTests =
  testGroup "show"
            [ testCase "root"  $ "[absdir|/|]"           ≟ show root
            , testCase "etc"   $ "[absdir|/etc/|]"       ≟ show etc
            , testCase "pam.d" $ "[absdir|/etc/pam.d/|]" ≟ show pamd
            ]

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
  deriving (Eq,Generic,NFData,Lift)

absNonRootDir ∷ SeqNE PathComponent → AbsDir
absNonRootDir = AbsNonRootDir ∘ NonRootAbsDir

type instance Element AbsDir = PathComponent

instance Show AbsDir where
  show r = [fmt|[absdir|%T%s]|] (toText r) "|"

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

{-| Things that may convert to an `AbsDir` (but an `AbsDir` will always convert
    to); e.g., @Dir@, @Abs@, @FPath@. -}
class AsAbsDir α where
  _AbsDir ∷ Prism' α AbsDir

instance AsAbsDir AbsDir where
  _AbsDir = id

--------------------

{-| Things that /may/ be converted from an `AbsDir` (but will always convert
    /to/ an `AbsDir`). -}
class AbsDirAs α where
  _AbsDir_ ∷ Prism' AbsDir α

instance AbsDirAs AbsDir where
  _AbsDir_ = id

instance AbsDirAs NonRootAbsDir where
  _AbsDir_ =
    prism' AbsNonRootDir (\ case AbsRootDir → 𝕹; AbsNonRootDir d → 𝕵 d)

------------------------------------------------------------

instance MonoFunctor NonRootAbsDir where
  omap ∷ (PathComponent → PathComponent) → NonRootAbsDir → NonRootAbsDir
  omap f (NonRootAbsDir ps) = NonRootAbsDir (omap f ps)

instance MonoFunctor AbsDir where
  omap ∷ (PathComponent → PathComponent) → AbsDir → AbsDir
  omap _ AbsRootDir = AbsRootDir
  omap f (AbsNonRootDir d) = AbsNonRootDir (omap f d)

----------

monoFunctorTests ∷ TestTree
monoFunctorTests =
  testGroup "MonoFunctor"
            [ testCase "usr" $
                    etc ≟ omap (const [pc|etc|]) etc
            , testCase "wgm.d" $
                  fromSeqNE @AbsDir([pc|w.d|] ⪪ [pc|g.d|] ⪪ pure [pc|M.d|])
                ≟ (◇ [pc|.d|]) ⪧ wgm
            , testCase "WGM" $
                  fromSeqNE @AbsDir([pc|W|] ⪪ [pc|G|] ⪪ pure [pc|M|])
                ≟ wgm ⪦ toUpper
            ]

----------------------------------------

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

--------------------

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

newtype B = B Bool
 deriving Eq

instance Printable B where
  print (B b) = P.string (show b)

monoFoldableTests ∷ TestTree
monoFoldableTests =
  testGroup "MonoFoldable"
            [ testCase "ofoldMap" $
                "w-g-M-" ≟ ofoldMap ((⊕ "-") ∘ toText) wgm
            , testCase "ofoldr" $
                "w-g-M-ф" ≟ ofoldr (\ a b → toText a ⊕ "-" ⊕ b) "ф" wgm
            , testCase "ofoldl'" $
                "ф-w-g-M" ≟ ofoldl' (\ b a → b ⊕ "-" ⊕ toText a) "ф" wgm
            , testCase "otoList" $
                [ [pc|w|], [pc|g|], [pc|M|] ] @=? otoList wgm
            , testCase "oall (F)" $
                𝕱 @=? oall (any (≡ 'r' ) ∘ toText) wgm
            , testCase "oall (T)" $
                𝕿 @=? oall ((< 6) ∘ length ∘ toText) wgm
            , testCase "oany (F)" $
                𝕱 @=? oany (any (≡ 'x' ) ∘ toText) wgm
            , testProperty "onull" (\ x → B (x ≡ root) ≣ B (onull x))
            , testCase "olength" $
                3 ≟ olength wgm
            , testCase "olength64" $
                0 ≟ olength64 root
            , testCase "ocompareLength" $
               GT @=? ocompareLength wgm (2 ∷ ℕ)
            , testCase "ofoldlM" $
                    Just [[pc|M|],[pc|g|],[pc|w|]]
                @=? ofoldlM (\ a e → Just $ e : a) [] wgm
            , testCase "ofoldMap1Ex" $
                [[pc|w|],[pc|g|],[pc|M|]] @=? ofoldMap1Ex pure wgm
            , testCase "ofoldr1Ex" $
                [pc|wgM|] ≟ ofoldr1Ex (◇) wgm
            , testCase "ofoldl1Ex'" $
                [pc|wgM|] ≟ ofoldl1Ex' (◇) wgm
            , testCase "unsafeHead" $
                [pc|w|] ≟ unsafeHead wgm
            , testCase "unsafeLast" $
                [pc|M|] ≟ unsafeLast wgm
            , testCase "maximumByEx" $
                [pc|w|] ≟ maximumByEx (comparing toText) wgm
            , testCase "minimumByEx" $
                [pc|M|] ≟ minimumByEx (comparing toText) wgm
            , testCase "oelem (T)" $
                𝕿 @=? oelem [pc|g|] wgm
            , testCase "oelem (F)" $
                𝕱 @=? oelem [pc|x|] wgm
            , testCase "onotElem (T)" $
                𝕿 @=? onotElem [pc|x|] wgm
            , testCase "onotElem (F)" $
                𝕱 @=? onotElem [pc|g|] wgm
            ]

----------------------------------------

instance FromSeqNonEmpty NonRootAbsDir where
  fromSeqNE ∷ SeqNE PathComponent → NonRootAbsDir
  fromSeqNE = NonRootAbsDir

instance FromSeqNonEmpty AbsDir where
  fromSeqNE = AbsNonRootDir ∘ fromSeqNE

----------------------------------------

instance ToSeqNonEmpty NonRootAbsDir where
  toSeqNE (NonRootAbsDir ps) = ps

----------------------------------------

instance IsSeqNonEmpty NonRootAbsDir where
  seqNE = iso toSeqNE fromSeqNE

----------------------------------------

instance FromSeq AbsDir where
  fromSeq xs = case fromNullable xs of
                 Nothing → AbsRootDir
                 Just ps → fromSeqNE (toSeqNE ps)

----------------------------------------

instance ToSeq AbsDir where
  toSeq AbsRootDir = (∅)
  toSeq (AbsNonRootDir n) = toSeq n

instance ToSeq NonRootAbsDir where
  toSeq = toSeq_

----------------------------------------

instance IsSeq AbsDir where
  seq = iso toSeq fromSeq

isMonoSeqGetterTests ∷ TestTree
isMonoSeqGetterTests =
  testGroup "getter"
            [ testCase "root"  $ Seq.Empty               @=? root ⊣ seq
            , testCase "etc"   $ pure [pc|etc|]          @=? etc  ⊣ seq
            , testCase "pam.d" $ [[pc|etc|],[pc|pam.d|]] @=? toList (pamd ⊣ seq)
            , testCase "wgm" $ [[pc|w|],[pc|g|],[pc|M|]] @=? toList (wgm  ⊣ seq)
            ]

isMonoSeqSetterTests ∷ TestTree
isMonoSeqSetterTests =
  let x ~~ y = x & seq ⊢ Seq.fromList y
   in testGroup "setter"
                [ testCase "etc"   $ etc   ≟ root ~~ [ [pc|etc|] ]
                , testCase "root"  $ root  ≟ etc ~~ []
                , testCase "d.pam" $
                      dpam ≟ dpam ~~ [ [pc|pam.d|], [pc|etc|] ]
                , testCase "wgm"   $
                      wgm   ≟ (⫣ seq) (Seq.fromList [[pc|w|],[pc|g|],[pc|M|]])
                ]

isMonoSeqTests ∷ TestTree
isMonoSeqTests =
  testGroup "IsMonoSeq" [ isMonoSeqGetterTests, isMonoSeqSetterTests ]

----------------------------------------

instance IsList AbsDir where
  type instance Item AbsDir = PathComponent
  fromList = fromSeq ∘ Seq.fromList
  toList   = toList ∘ toSeq

isListTests ∷ TestTree
isListTests =
  testGroup "IsList"
    [ testGroup "fromList"
                [ testCase "root"  $ root ≟ fromList []
                , testCase "etc"   $ etc  ≟ fromList [ [pc|etc|] ]
                , testCase "pam.d" $ pamd ≟ fromList [ [pc|etc|], [pc|pam.d|] ]
                , testCase "wgm"   $ wgm  ≟ fromList [ [pc|w|],[pc|g|],[pc|M|] ]
                ]
    , testGroup "toList"
                [ testCase "root"  $
                      []                            @=? toList root
                , testCase "etc"   $
                      [ [pc|etc|] ]                 @=? toList etc
                , testCase "pam.d" $
                      [ [pc|etc|], [pc|pam.d|] ]    @=? toList pamd
                , testCase "wgm"   $
                      [ [pc|w|], [pc|g|], [pc|M|] ] @=? toList wgm
                ]
    ]

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
pDir ∷ (P.Printer ρ, ToSeq α, Printable (Element α)) ⇒
       (String → String) → α → ρ
pDir f =  P.string ∘ f ∘ concat ∘ fmap ((⊕ "/") ∘ toString) ∘ toSeq

instance Printable AbsDir where
  print = pDir ("/" ⊕)

instance Printable NonRootAbsDir where
  print = pDir ("/" ⊕)

printableTests ∷ TestTree
printableTests =
  testGroup "printable"
            [ testCase "root"  $ "/"           ≟ toText root
            , testCase "etc"   $ "/etc/"       ≟ toText etc
            , testCase "pam.d" $ "/etc/pam.d/" ≟ toText pamd
            , testCase "wgm"   $ "/w/g/M/"     ≟ toText wgm
            ]

----------------------------------------

instance AsFilePath AbsDir where
  filepath = prism' toString fromString

instance AsFilePath NonRootAbsDir where
  filepath = prism' toString fromString

instance AsFilePath' AbsDir where
  filepath' = prism' (exterminate ∘ toString)
                     (fromString ∘ terminate)

instance AsFilePath' NonRootAbsDir where
  filepath' = prism' (exterminate ∘ toString)
                     (fromString ∘ terminate)

filepathTests ∷ TestTree
filepathTests =
  let nothin' = Nothing ∷ Maybe AbsDir
      fail s  = testCase s $ nothin' @=? s ⩼ filepath
   in testGroup "filepath"
            [ testCase "root"  $ "/"           ≟ root    ⫥ filepath
            , testCase "etc"   $ "/etc/"       ≟ etc     ⫥ filepath
            , testCase "pam.d" $ "/etc/pam.d/" ≟ pamd    ⫥ filepath
            , testCase "wgm"   $ "/w/g/M/"     ≟ wgm     ⫥ filepath
            , testCase "/etc/" $ Just etc      @=? "/etc/" ⩼ filepath
            , fail "/etc"
            , fail "/etc/pam.d"
            , fail "etc/"
            , fail "etc/pam.d"
            , fail "/etc//pam.d/"
            , fail "e/c"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"
            ]

----------------------------------------

instance Textual AbsDir where
  textual = SeqConversions.fromList ⊳ (char '/' ⋫ endBy textual (char '/'))

instance Textual NonRootAbsDir where
  textual =
    fromSeqNE ∘ fromNonEmpty ⊳ (char '/' ⋫ endByNonEmpty textual (char '/'))

textualTests ∷ TestTree
textualTests =
  let nothin'     ∷ Maybe AbsDir
      nothin'     = Nothing
      success e s = testCase s $ Parsed e  @=? parseString s
      fail s      = testCase s $ nothin'   @=? fromString s
   in testGroup "Textual"
                [ success root "/"
                , success etc  "/etc/"
                , success pamd "/etc/pam.d/"
                , fail "/etc"
                , fail "/etc/pam.d"
                , fail "etc/"
                , fail "etc/pam.d"
                , fail "/etc//pam.d/"
                , fail "e/c"
                , fail "\0etc"
                , fail "etc\0"
                , fail "e\0c"
                , testProperty "parseString - toString"
                               (propInvertibleString @AbsDir)
                , testProperty "parseText - toText" (propInvertibleText @AbsDir)
                , testProperty "parseUtf8 - toUtf8" (propInvertibleUtf8 @AbsDir)
                ]

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

----------

parentTests ∷ TestTree
parentTests =
  let par d = (view parent) ⊳ (d ⩼ nonRootAbsDir)
   in testGroup "parent"
                [ testCase "root"        $ Nothing   @=? par root
                , testCase "etc"         $ Just root @=? par etc
                , testCase "pamd"        $ Just etc  @=? par pamd
                ]

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

--------------------

instance HasParentMay NonRootAbsDir where
  parentMay = lens nonAbsRootGetParentMay nonAbsRootSetParentMay

--------------------

parentMayTests ∷ TestTree
parentMayTests =
  let -- (~~) ∷ α → α → α
      d ~~ d' = d & parentMay ⊩ d'
   in testGroup "parentMay"
                [ testCase "root"   $ Nothing   @=? root  ⊣ parentMay
                , testCase "etc"    $ Just root @=? etc   ⊣ parentMay
                , testCase "pamd"  $ Just etc  @=? pamd ⊣ parentMay

                , testCase "etc → root" $ etc @=? etc ~~ root
                , testCase "root → etc" $ etc @=? root ~~ etc

                , testCase "pamd → root" $
                    fromSeq @AbsDir (pure [pc|pam.d|]) @=? pamd ~~ root
                , testCase "root → pamd" $ pamd @=? root ~~ pamd

                , testCase "etc → wgm" $
                      fromSeqNE @AbsDir([pc|w|]⪪[pc|g|]⪪[pc|M|]⪪pure[pc|etc|])
                    @=? etc ~~ wgm
                , testCase "wgm → etc" $
                    fromSeqNE @AbsDir ([pc|etc|]⪪pure [pc|M|]) @=? wgm ~~ etc

                , testCase "root → wgm" $ wgm @=? root ~~ wgm
                , testCase "wgm → root" $
                    fromSeq @AbsDir (pure [pc|M|]) @=? wgm ~~ root

                , testCase "pamd → etc" $ pamd @=? pamd ~~ etc
                , testCase "etc → pamd" $
                      fromSeqNE @AbsDir ([pc|etc|]⪪[pc|pam.d|]⪪pure [pc|etc|])
                    @=? etc ~~ pamd

                , testCase "pamd → Nothing" $
                    fromSeq (pure [pc|pam.d|]) @=? (pamd & parentMay ⊢ Nothing)
                ]

parentsTests ∷ TestTree
parentsTests =
  let check t d ps = assertListEq t ps (parents d)
   in testGroup "parents" $ [ check "/"          root []
                            , check "/etc/"      etc  [root]
                            , check "/etc/pam.d" pamd [root,etc]
                            , check "/w/g/m/"    wgm  [root,w,wg]
                            , check "/etc/"      etcN  [root]
                            , check "/etc/pam.d" pamdN [root,etc]
                            , check "/w/g/m/"    wgmN  [root,w,wg]
                            ]

----------------------------------------

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
      illegalCE   = let fpcice = fPathComponentIllegalCharE '\0' "pam\0"
                     in FPathComponentE fpcice absdirT pamNUL
      emptyCompCE = FPathComponentE fPathComponentEmptyE absdirT "/etc//pam.d/"
      _parseAbsDir ∷ MonadError FPathError η ⇒ Text → η AbsDir
      _parseAbsDir = parse'
   in testGroup "parseAbsDir"
                [ testCase "root"  $ Right root   @=? _parseAbsDir "/"
                , testCase "etc"   $ Right etc    @=? _parseAbsDir "/etc/"
                , testCase "pam.d" $ Right pamd   @=? _parseAbsDir "/etc/pam.d/"
                , testCase "wgm"   $ Right wgm    @=? _parseAbsDir "/w/g/M/"
                , testCase "no trailing /" $
                      Left (fPathNotADirE absdirT pamF) @=? _parseAbsDir pamF
                , testCase "empty" $
                      Left (fPathEmptyE absdirT)  @=? _parseAbsDir ""
                , testCase "no leading /" $
                      Left (fPathNonAbsE absdirT "etc/") @=? _parseAbsDir "etc/"
                , testCase "bad component" $
                      Left illegalCE @=? _parseAbsDir pamNUL
                , testCase "empty component" $
                      Left emptyCompCE @=? _parseAbsDir "/etc//pam.d/"
                ]

----------------------------------------

{- | Like `parseAbsDir`, but non-empty input that doesn't end in a '/' character
     has a '/' appended rather than failing as a non-file (thus, "permissive
     `parseAbsDir`" -}
parseAbsDirP ∷ ∀ ε τ η . (AsFPathError ε, MonadError ε η, Printable τ) ⇒
               τ → η AbsDir
parseAbsDirP (toText → t) =
  let safeLast "" = Nothing
      safeLast s  = Just $ Text.last s
   in case safeLast t of
        Nothing  → parse empty
        Just '/' → parse t
        _        → parse (t ⊕ "/")

{-# DEPRECATED parseAbsDirP' "use parseAbsDirP @FPathError" #-}
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
      illegalCE   = let fpcice = fPathComponentIllegalCharE '\0' "pam\0"
                     in FPathComponentE fpcice absdirT pamNUL
      emptyCompCE = FPathComponentE fPathComponentEmptyE absdirT "/etc//pam.d/"
      _parseAbsDirP ∷ MonadError FPathError η ⇒ Text → η AbsDir
      _parseAbsDirP = parseAbsDirP'
   in testGroup "parseAbsDirP"
                [ testCase "root"  $ Right root   @=? _parseAbsDirP "/"
                , testCase "etc"   $ Right etc    @=? _parseAbsDirP "/etc/"
                , testCase "etc"   $ Right etc    @=? _parseAbsDirP "/etc"
                , testCase "pam.d" $ Right pamd   @=? _parseAbsDirP "/etc/pam.d"
                , testCase "pam.d" $
                      Right pamd   @=? _parseAbsDirP "/etc/pam.d/"
                , testCase "wgm"   $ Right wgm    @=? _parseAbsDirP "/w/g/M/"
                , testCase "wgm"   $ Right wgm    @=? _parseAbsDirP "/w/g/M"
                , testCase "empty" $
                      Left (fPathEmptyE absdirT)  @=? _parseAbsDirP ""
                , testCase "no leading /" $
                          Left (fPathNonAbsE absdirT "etc/")
                      @=? _parseAbsDirP "etc/"
                , testCase "bad component" $
                      Left illegalCE @=? _parseAbsDirP pamNUL
                , testCase "empty component" $
                      Left emptyCompCE @=? _parseAbsDirP "/etc//pam.d/"
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
      illegalCE   = let fpcice = fPathComponentIllegalCharE '\0' "pam\0"
                     in FPathComponentE fpcice nrabsdirT pamNUL
      emptyCompCE = FPathComponentE fPathComponentEmptyE nrabsdirT "/etc//pam/"
      parseAbsDirN_ ∷ MonadError FPathError η ⇒ Text → η NonRootAbsDir
      parseAbsDirN_ = parse'
   in testGroup "parseAbsDirN"
                [ testCase "etc"   $ Right etcN    @=? parseAbsDirN_ "/etc/"
                , testCase "pam.d" $
                      Right pamdN   @=? parseAbsDirN_ "/etc/pam.d/"
                , testCase "wgm"   $ Right wgmN    @=? parseAbsDirN_ "/w/g/M/"
                , testCase "root"  $
                          Left (fPathRootDirE nrabsdirT)
                      @=? parseAbsDirN_ "/"
                , testCase "no trailing /" $
                          Left (fPathNotADirE nrabsdirT pamF)
                      @=? parseAbsDirN_ pamF
                , testCase "empty" $
                          Left (fPathEmptyE nrabsdirT)
                      @=? parseAbsDirN_ ""
                , testCase "no leading /" $
                         Left (fPathNonAbsE nrabsdirT "etc/")
                      @=? parseAbsDirN_ "etc/"
                , testCase "bad component" $
                      Left illegalCE @=? parseAbsDirN_ pamNUL
                , testCase "empty component" $
                      Left emptyCompCE @=? parseAbsDirN_ "/etc//pam/"
                ]

----------------------------------------

{- | Like `parseAbsDirN`, but non-empty input that doesn't end in a '/'
      character has a '/' appended rather than failing as a non-file (thus,
      "permissive `parseAbsDirN`" -}
parseAbsDirNP ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒
               τ → η NonRootAbsDir
parseAbsDirNP (toText → t) = do
  let safeLast "" = Nothing
      safeLast s  = Just $ Text.last s
   in case safeLast t of
        Nothing  → parse empty
        Just '/' → parse t
        _        → parse (t ⊕ "/")

--------------------

parseAbsDirNP' ∷ (Printable τ, MonadError FPathError η) ⇒ τ → η NonRootAbsDir
parseAbsDirNP' = parseAbsDirNP

--------------------

__parseAbsDirNP__ ∷ HasCallStack ⇒ String → NonRootAbsDir
__parseAbsDirNP__ s = case parse' s of
                       Left e → __ERROR'__ e
                       Right AbsRootDir → __ERROR'__ $ fPathRootDirE nrabsdirT
                       Right (AbsNonRootDir nr) → nr

__parseAbsDirNP'__ ∷ String → NonRootAbsDir
__parseAbsDirNP'__ = __parseAbsDirNP__

--------------------

parseAbsDirNPTests ∷ TestTree
parseAbsDirNPTests =
  let pamNUL      = "/etc/pam\0/"
      illegalCE   = let fpcice = fPathComponentIllegalCharE '\0' "pam\0"
                     in FPathComponentE fpcice nrabsdirT pamNUL
      emptyCompCE = FPathComponentE fPathComponentEmptyE nrabsdirT "/etc//pam/"
      parseAbsDirNP_ ∷ MonadError FPathError η ⇒ Text → η NonRootAbsDir
      parseAbsDirNP_ = parseAbsDirNP'
   in testGroup "parseAbsDirNP"
                [ testCase "etc"   $ Right etcN  @=? parseAbsDirNP_ "/etc"
                , testCase "pam.d" $ Right pamdN @=? parseAbsDirNP_ "/etc/pam.d"
                , testCase "wgm"   $ Right wgmN  @=? parseAbsDirNP_ "/w/g/M"
                , testCase "root"  $
                      Left (fPathRootDirE nrabsdirT) @=? parseAbsDirNP_ "/"
                , testCase "empty" $
                      Left (fPathEmptyE nrabsdirT)  @=? parseAbsDirNP_ ""
                , testCase "no leading /" $
                        Left (fPathNonAbsE nrabsdirT "etc/")
                      @=? parseAbsDirNP_ "etc/"
                , testCase "bad component" $
                      Left illegalCE @=? parseAbsDirNP_ pamNUL
                , testCase "empty component" $
                      Left emptyCompCE @=? parseAbsDirNP_ "/etc//pam/"
                ]

------------------------------------------------------------
--                     Quasi-Quoting                      --
------------------------------------------------------------

absdirQQ ∷ String → Maybe ExpQ
absdirQQ = (\ d → ⟦d⟧) ⩺ (ѭ ∘ parse @AbsDir @FPathError)

absdirNQQ ∷ String → Maybe ExpQ
absdirNQQ = (\ d → ⟦d⟧) ⩺ (ѭ ∘ parse @NonRootAbsDir @FPathError)

{- | quasi-quoter for AbsDir -}
absdir ∷ QuasiQuoter
absdir = mkQQ "AbsDir" $ def & exp ⊩ absdirQQ

{- | quasi-quoter for NonRootAbsDir -}

absdirN ∷ QuasiQuoter
absdirN = mkQQ "NonRootAbsDir" $ def & exp ⊩ absdirNQQ

----------------------------------------

instance HasDirname AbsDir where
  dirname ∷ Lens' AbsDir AbsDir
  dirname = lens (\ case AbsRootDir → AbsRootDir; AbsNonRootDir d → d ⊣ dirname)
                 (\ o d → case o of
                            AbsRootDir → d
                            AbsNonRootDir n → AbsNonRootDir $ n & parent ⊢ d
                 )

  ancestors' ∷ AbsDir → [AbsDir]
  ancestors' fp | fp ≡ root  = []
                | otherwise  = (fp ⊣ dirname) : ancestors' (fp ⊣ dirname)

absDirAncestors'Tests ∷ TestTree
absDirAncestors'Tests =
  testGroup "AbsDir"
            [ testCase "root" $ []          @=? ancestors' root
            , testCase "etc"  $ [root]      @=? ancestors' etc
            , testCase "pamd" $ [etc,root]  @=? ancestors' pamd
            , testCase "wgm"  $ [wg,w,root] @=? ancestors' wgm
            ]

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

instance Ancestors NonRootAbsDir where
  ancestors ∷ NonRootAbsDir → NonEmpty AbsDir
  ancestors fp = (fp ⊣ dirname) :| ancestors' (fp ⊣ dirname)

nonRootAbsDirAncestorsTests ∷ TestTree
nonRootAbsDirAncestorsTests =
  testGroup "NonRootAbsDir"
            [ testCase "etc"  $ pure root      @=? ancestors etcN
            , testCase "pamd" $ etc :| [root]  @=? ancestors pamdN
            , testCase "wgm"  $ wg :| [w,root] @=? ancestors wgmN
            ]

instance HasDirname NonRootAbsDir where
  dirname ∷ Lens' NonRootAbsDir AbsDir
  dirname = parent

  ancestors' ∷ NonRootAbsDir → [AbsDir]
  ancestors' fp = (fp ⊣ dirname) : ancestors' (fp ⊣ dirname)

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

nonRootAbsDirAncestors'Tests ∷ TestTree
nonRootAbsDirAncestors'Tests =
  testGroup "NonRootAbsDir"
            [ testCase "etc"  $ [root]      @=? ancestors' etcN
            , testCase "pamd" $ [etc,root]  @=? ancestors' pamdN
            , testCase "wgm"  $ [wg,w,root] @=? ancestors' wgmN
            ]

dirnameTests ∷ TestTree
dirnameTests = testGroup "dirname" [ absDirDirnameTests
                                   , nonRootAbsDirDirnameTests ]

ancestors'Tests ∷ TestTree
ancestors'Tests = testGroup "ancestors'" [ absDirAncestors'Tests
                                         , nonRootAbsDirAncestors'Tests ]

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

  updateBasename ∷ (PathComponent → PathComponent) → NonRootAbsDir
                 → NonRootAbsDir
  updateBasename f (NonRootAbsDir (ps :⫸ p)) = NonRootAbsDir (ps :⫸ f p)

nonRootBasenameTests ∷ TestTree
nonRootBasenameTests =
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
basenameTests =
  testGroup "basename" [ absDirBasenameTests, nonRootBasenameTests ]

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

dpam ∷ AbsDir
dpam = fromSeq $ [pc|pam.d|] ⪪ pure [pc|etc|]

wgm ∷ AbsDir
wgm = fromSeq $ [pc|w|] ⪪ [pc|g|] ⪪ pure [pc|M|]

wg ∷ AbsDir
wg = fromSeq $ [pc|w|] ⪪ pure [pc|g|]

w ∷ AbsDir
w = fromSeq $ pure [pc|w|]

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
tests = testGroup "FPath.AbsDir" [ constructionTests, showTests
                                 , isListTests, filepathTests
                                 , dirnameTests, nonRootAbsDirAncestorsTests
                                 , ancestors'Tests, basenameTests
                                 , printableTests, textualTests
                                 , monoFoldableTests, isMonoSeqTests
                                 , monoFunctorTests
                                 , parentTests, parentMayTests, parentsTests
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
