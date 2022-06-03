{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

module FPath.RelDir
  ( AsRelDir( _RelDir ), RelDir --, AsNonRootRelDir( _NonRootRelDir )
  , NonRootRelDir

  , reldir, reldirN, reldirT
  , parseRelDirP

  , tests
  )
where

import Base1T  hiding ( toList )
import Prelude  ( error )

-- base --------------------------------

import Data.Foldable  ( concat, foldMap )
import Data.Monoid    ( Monoid( mempty ) )
import Data.Typeable  ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Exts       ( IsList( toList ) )
import GHC.Generics   ( Generic )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

import Data.Sequence  ( (<|) )

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

import Data.MonoTraversable  ( Element, MonoFoldable( ofoldl', ofoldl1Ex'
                                                    , ofoldMap, ofoldr
                                                    , ofoldr1Ex, otoList )
                             , MonoFunctor( omap )
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Function  ( (⅋) )
import Data.MoreUnicode.Lens      ( (⊩) )

-- non-empty-containers ----------------

import qualified  NonEmptyContainers.SeqNE  as  SeqNE

import NonEmptyContainers.IsNonEmpty        ( fromNonEmpty )
import NonEmptyContainers.SeqConversions    ( FromSeq( fromSeq )
                                            , IsSeq( seq )
                                            , ToSeq( toSeq ) )
import NonEmptyContainers.SeqNE             ( SeqNE( (:⫸) ) , pattern(:⪭)
                                            , pattern(:⪬), (⋖) )
import NonEmptyContainers.SeqNEConversions  ( FromSeqNonEmpty( fromSeqNE )
                                            , ToSeqNonEmpty( toSeqNE ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char, string )
import Text.Parser.Combinators  ( endBy, endByNonEmpty )

-- quasiquoting ------------------------

import QuasiQuoting  ( QuasiQuoter, mkQQ, exp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertListEq, propInvertibleString, propInvertibleText
                  , propInvertibleUtf8 )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ )
import Language.Haskell.TH.Syntax  ( Exp( AppE, ConE )
                                   , Lift( lift, liftTyped ), TExp( TExp ) )

-- text --------------------------------

import qualified  Data.Text  as  Text
import Data.Text  ( Text, empty, splitOn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

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
                               , __FPathRootDirE__
                               , _FPathComponentE
                               , fPathAbsE, fPathEmptyE
                               , fPathNotADirE, mapTypeRepE
                               )
import FPath.Parent            ( HasParent( parent )
                               , HasParentMay( parentMay, parents ) )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( PathComponent, parsePathC, pc, toUpper )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.Util              ( __ERROR'__ )

-------------------------------------------------------------------------------

{- | A non-root absolute directory, e.g., /etc. -}
-- a non-root dir is a path component appended to a (possibly-root) absolute
-- directory
newtype NonRootRelDir = NonRootRelDir (SeqNE PathComponent)
  deriving (Eq,Generic,Lift)
  deriving anyclass NFData

nrreldirT ∷ TypeRep
nrreldirT = typeRep (Proxy ∷ Proxy NonRootRelDir)

type instance Element NonRootRelDir = PathComponent

instance Show NonRootRelDir where
  show r = [fmt|[reldir|%T%s]|] (toText r) "|"

----------------------------------------

instance Ord NonRootRelDir where
  a <= b = toText a ≤ toText b

--------------------

instance Printable NonRootRelDir where
  print (NonRootRelDir ps) = pDir id ps

--------------------

instance Textual NonRootRelDir where
  textual = fromSeqNE ∘ fromNonEmpty ⊳ (endByNonEmpty textual (char '/'))

--------------------

instance AsFilePath NonRootRelDir where
  filepath = prism' toString fromString

--------------------

instance AsFilePath' NonRootRelDir where
  filepath' = prism' (exterminate ∘ toString)
                     (fromString ∘ terminate)

--------------------

instance Semigroup NonRootRelDir where
  (NonRootRelDir ps) <> (NonRootRelDir ps') = NonRootRelDir $ ps ◇ ps'

--------------------

instance RelTypeC NonRootRelDir where
  type RelType NonRootRelDir = NonRootRelDir

--------------------

instance DirTypeC NonRootRelDir where
  type DirType NonRootRelDir = RelDir

----------------------------------------

instance FromSeqNonEmpty NonRootRelDir where
  fromSeqNE = NonRootRelDir ∘ fromSeqNE

----------------------------------------

instance ToSeqNonEmpty NonRootRelDir where
  toSeqNE (NonRootRelDir d) = d

----------------------------------------

instance ToSeq NonRootRelDir where
  toSeq (NonRootRelDir ps) = toSeq ps

----------------------------------------

instance Basename NonRootRelDir where
  basename (NonRootRelDir (_ :⫸ p)) = fromSeqNE (pure p)
  updateBasename f (NonRootRelDir (ps :⫸ p)) = NonRootRelDir (ps :⪭ f p)

----------------------------------------

nrrdGetParent ∷ NonRootRelDir → RelDir
nrrdGetParent (NonRootRelDir (ps :⫸ _)) = fromSeq ps

nrrdSetParent ∷ NonRootRelDir → RelDir → NonRootRelDir
nrrdSetParent (NonRootRelDir (_ :⫸ d)) p = NonRootRelDir (toSeq p :⫸ d)

instance HasParent NonRootRelDir where
  parent = lens nrrdGetParent nrrdSetParent
----------------------------------------

instance HasDirname NonRootRelDir where
  dirname ∷ Lens' NonRootRelDir RelDir
  dirname = lens nrrdGetParent nrrdSetParent

  ancestors' ∷ NonRootRelDir → [RelDir]
  ancestors' d = (d ⊣ dirname) : ancestors' (d ⊣ dirname)

----------------------------------------

nonRootGetParentMay ∷ NonRootRelDir → Maybe RelDir
nonRootGetParentMay = 𝕵 ∘ view parent

nonRootSetParentMay ∷ NonRootRelDir → Maybe RelDir → NonRootRelDir
nonRootSetParentMay (NonRootRelDir ps) d =
  fromSeqNE $ (maybe ф toSeq d) :⫸ SeqNE.last ps

instance HasParentMay NonRootRelDir where
  parentMay = lens nonRootGetParentMay nonRootSetParentMay

----------------------------------------

instance MonoFunctor NonRootRelDir where
  omap ∷ (PathComponent → PathComponent) → NonRootRelDir → NonRootRelDir
  omap f (NonRootRelDir ps) = NonRootRelDir (omap f ps)

----------------------------------------

instance MonoFoldable NonRootRelDir where
  otoList (NonRootRelDir ps) = toList $ toSeq ps
  ofoldl' ∷ (α → PathComponent → α) → α → NonRootRelDir → α
  ofoldl' f x r = foldl' f x (otoList r)
  ofoldr ∷ (PathComponent → α → α) → α → NonRootRelDir → α
  ofoldr f x r = foldr f x (otoList r)
  ofoldMap ∷ Monoid ν ⇒ (PathComponent → ν) → NonRootRelDir → ν
  ofoldMap f r = foldMap f (otoList r)
  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → NonRootRelDir
            → PathComponent
  ofoldr1Ex f r = foldr1 f (otoList r)
  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → NonRootRelDir
             → PathComponent
  ofoldl1Ex' f r = foldl1 f (otoList r)

----------------------------------------

instance Parseable NonRootRelDir where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η NonRootRelDir
  parse t = do mapTypeRepE (const nrreldirT) $ parse t ≫ \ case
                 RelRootDir → __FPathRootDirE__ nrreldirT
                 RelNonRootDir d' → return d'

------------------------------------------------------------
--                     Quasi-Quoting                      --
------------------------------------------------------------

reldirNQQ ∷ String → Maybe ExpQ
reldirNQQ = (\ d → ⟦d⟧) ⩺ (ѭ ∘ parse @NonRootRelDir @FPathError)

reldirN ∷ QuasiQuoter
reldirN = mkQQ "NonRootRelDir" $ def & exp ⊩ reldirNQQ

------------------------------------------------------------

{- | a relative directory -}
data RelDir = RelRootDir | RelNonRootDir NonRootRelDir
  deriving Eq

--------------------

type instance Element RelDir = PathComponent

--------------------

instance Show RelDir where
  show r = [fmt|[reldir|%T%s]|] (toText r) "|"

showTests ∷ TestTree
showTests =
  testGroup "show"
            [ testCase "root"  $ "[reldir|./|]"           ≟ show r0
            , testCase "etc"   $ "[reldir|r/|]"       ≟ show r1
            , testCase "pam.d" $ "[reldir|r/p/|]" ≟ show r2
            , testCase "pam.d" $ "[reldir|p/q/r/|]" ≟ show r3
            , testCase "pam.d" $ "[reldir|p/|]" ≟ show r3p
            , testCase "pam.d" $ "[reldir|p/q/|]" ≟ show r3pq
            ]

--------------------

instance Ord RelDir where
  a <= b = toText a ≤ toText b

--------------------

instance Lift RelDir where
  liftTyped RelRootDir = return ∘ TExp $ ConE 'RelRootDir
  liftTyped (RelNonRootDir d) = do
    x ← lift d
    return ∘ TExp $ AppE (ConE 'RelNonRootDir) x

--------------------

instance Semigroup RelDir where
  RelRootDir <> RelRootDir = RelRootDir
  RelRootDir <> r@(RelNonRootDir _) = r
  r@(RelNonRootDir _) <> RelRootDir = r
  (RelNonRootDir r) <> (RelNonRootDir r') = RelNonRootDir $ r ◇ r'

--------------------

instance Monoid RelDir where
  mempty = RelRootDir

--------------------

{-| Things that may convert to an `RelDir` (but  a `RelDir` will always convert
    to); e.g., @Dir@, @Rel@, @FPath@. -}
class AsRelDir α where
  _RelDir ∷ Prism' α RelDir

instance AsRelDir RelDir where
  _RelDir = id

--------------------

{-| Things that /may/ be converted from an `RelDir` (but will always convert
    /to/ an `RelDir`). -}
class RelDirAs α where
  _RelDir_ ∷ Prism' RelDir α

instance RelDirAs RelDir where
  _RelDir_ = id

instance RelDirAs NonRootRelDir where
  _RelDir_ =
    prism' RelNonRootDir (\ case RelRootDir → 𝕹; RelNonRootDir d → 𝕵 d)

--------------------

instance DirTypeC RelDir where
  type DirType RelDir = RelDir

--------------------

instance RelTypeC RelDir where
  type RelType RelDir = RelDir

------------------------------------------------------------

instance MonoFunctor RelDir where
  omap ∷ (PathComponent → PathComponent) → RelDir → RelDir
  omap _ RelRootDir = RelRootDir
  omap f (RelNonRootDir ps) = RelNonRootDir (omap f ps)

----------------------------------------

instance MonoFoldable RelDir where
  otoList ∷ RelDir → [PathComponent]
  otoList RelRootDir = ф
  otoList (RelNonRootDir d) = otoList d
  ofoldl' ∷ (α → PathComponent → α) → α → RelDir → α
  ofoldl' f x r = foldl' f x (toList r)

  ofoldr ∷ (PathComponent → α → α) → α → RelDir → α
  ofoldr f x r = foldr f x (toList r)
  ofoldMap ∷ Monoid ν ⇒ (PathComponent → ν) → RelDir → ν
  ofoldMap f r = foldMap f (toList r)
  ofoldr1Ex ∷ (PathComponent → PathComponent → PathComponent) → RelDir
            → PathComponent
  ofoldr1Ex f r = foldr1 f (toList r)
  ofoldl1Ex' ∷ (PathComponent → PathComponent → PathComponent) → RelDir
             → PathComponent
  ofoldl1Ex' f r = foldl1 f (toList r)

----------------------------------------

instance FromSeqNonEmpty RelDir where
  fromSeqNE = RelNonRootDir ∘ fromSeqNE

----------------------------------------

instance FromSeq RelDir where
  fromSeq Seq.Empty = RelRootDir
  fromSeq (x :⪬ xs) = RelNonRootDir $ fromSeqNE (x :⪬ xs)
  fromSeq _         = error "patterns should not be exhausted!"

----------------------------------------

instance ToSeq RelDir where
  toSeq RelRootDir        = ф
  toSeq (RelNonRootDir d) = toSeq d

----------------------------------------

instance IsSeq RelDir where
  seq = iso toSeq fromSeq

----------------------------------------

instance IsList RelDir where
  type instance Item RelDir = PathComponent
  fromList []     = RelRootDir
  fromList (x:xs) = RelNonRootDir $ fromSeqNE (x ⋖ xs)
  toList   = toList ∘ toSeq

----------------------------------------

{- | Convert a sequence of printable elements to strings by intercalating '/'
     characters; with a post-fact string transformation for extra bells (e.g.,
     a prefix '/' character -}
pDir ∷ (P.Printer ρ, ToSeq α, Printable (Element α)) ⇒ (String → String) → α → ρ
pDir f =  P.string ∘ f ∘ concat ∘ fmap ((⊕ "/") ∘ toString) ∘ toSeq

instance Printable RelDir where
  print RelRootDir        = "./"
  print (RelNonRootDir d) = print d

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

----------

textualTests ∷ TestTree
textualTests =
  let success e s = testCase s $ Parsed e                @=? parseString s
      fail s      = testCase s $ (𝕹 ∷ 𝕄 RelDir)        @=? fromString s
      failN s     = testCase s $ (𝕹 ∷ 𝕄 NonRootRelDir) @=? fromString s
   in testGroup "Textual"
                [ success r0 "./"
                , success r1 "r/"
                , success r2 "r/p/"
                , success r3 "p/q/r/"
                , success r3p "p/"
                , success r3pq "p/q/"
                , success r3pN "p/"
                , success r3pqN "p/q/"
                , failN "./"
                , fail "/etc"
                , fail "/etc/pam.d"
                , fail "/etc/"
                , fail "etc/pam.d"
                , fail "/etc//pam.d/"
                , fail "e/c"
                , fail "\0etc"
                , fail "etc\0"
                , fail "e\0c"
                , testProperty "parseString - toString"
                               (propInvertibleString @RelDir)
                , testProperty "parseText - toText" (propInvertibleText @RelDir)
                , testProperty "parseUtf8 - toUtf8" (propInvertibleUtf8 @RelDir)
                ]

----------------------------------------

instance Arbitrary RelDir where
  arbitrary = fromSeq ⊳ arbitrary
  shrink = fromSeq ⩺ shrink ∘ toSeq

----------------------------------------

instance HasParentMay RelDir where
  parentMay = lens getParentMay setParentMay
              where getParentMay ∷ RelDir → Maybe RelDir
                    getParentMay RelRootDir = 𝕹
                    getParentMay (RelNonRootDir d) = d ⊣ parentMay

                    setParentMay ∷ RelDir → Maybe RelDir → RelDir
                    setParentMay RelRootDir (𝕵 r) = r
                    setParentMay RelRootDir 𝕹    = RelRootDir
                    setParentMay (RelNonRootDir d) r =
                      RelNonRootDir $ d & parentMay ⊢ r

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
  basename (RelNonRootDir d) = RelNonRootDir $ basename d
  basename RelRootDir = RelRootDir

  updateBasename ∷ (PathComponent → PathComponent) → RelDir → RelDir
  updateBasename f (RelNonRootDir d) = RelNonRootDir $ updateBasename f d
  updateBasename _ RelRootDir = RelRootDir

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
  dirname = lens (\ case RelRootDir      → RelRootDir
                         RelNonRootDir d → d ⊣ dirname)
                 (\ r d → case r of
                            RelRootDir       → d
                            RelNonRootDir r' → RelNonRootDir $ r' & dirname ⊢ d
                 )

  ancestors' ∷ RelDir → [RelDir]
  ancestors' RelRootDir = []
  ancestors' fp         = (fp ⊣ dirname) : ancestors' (fp ⊣ dirname)

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
      𝕹           → error "cannot happen: splitOn always returns something"
      𝕵 (("":_), _)  → __FPathAbsE__ reldirT t
      𝕵 ([],"")      → __FPathEmptyE__ reldirT
      𝕵 (["."],"")   → return RelRootDir
      𝕵 ((x:xs), "") → do
        let mkCompE ∷ (AsFPathError ε', MonadError ε' η') ⇒
                      FPathComponentError → η' α
            mkCompE ce = __FPathComponentE__ ce reldirT t
            eCompE ∷ (AsFPathError ε'', MonadError ε'' η'') ⇒
                     Either FPathComponentError α → η'' α
            eCompE = either mkCompE return

        p  ← eCompE $ parsePathC x
        ps ← eCompE $ mapM parsePathC xs
        return $ fromSeq (p <| Seq.fromList ps)
      _                 → __FPathNotADirE__ reldirT t

--------------------

parseRelDirTests ∷ TestTree
parseRelDirTests =
  let pamF        = "etc/pam"
      illegalCE s t = let fpcice = fPathComponentIllegalCharE '\0' t
                       in FPathComponentE fpcice reldirT s
      badChar s p = testCase ("bad component " ⊕ toString s) $
                        𝕷 (illegalCE s p) @=? parseRelDir_ s
      emptyCompCE t = FPathComponentE fPathComponentEmptyE reldirT t
      parseRelDir_ ∷ MonadError FPathError η ⇒ Text → η RelDir
      parseRelDir_ = parse
   in testGroup "parseRelDir"
                [ testCase "r0 (./)" $ 𝕽 r0 @=? parseRelDir_ "./"
                , testCase "r1" $ 𝕽 r1 @=? parseRelDir_ "r/"
                , testCase "r2" $ 𝕽 r2 @=? parseRelDir_ "r/p/"
                , testCase "r3" $ 𝕽 r3 @=? parseRelDir_ "p/q/r/"
                , testCase "no trailing /" $
                      𝕷 (fPathNotADirE reldirT pamF) @=? parseRelDir_ pamF
                , testCase "leading /" $
                      𝕷 (fPathAbsE reldirT "/r/") @=? parseRelDir_ "/r/"
                , badChar "x/\0/y/" "\0"
                , badChar "r/p\0/" "p\0"
                , badChar "\0r/p/" "\0r"
                , testCase "empty component" $
                      𝕷 (emptyCompCE "r//p/") @=? parseRelDir_ "r//p/"
                ]

----------------------------------------

{- | Like `parseRelDir`, but non-empty input that doesn't end in a '/' character
     has a '/' appended rather than failing as a non-file (thus, "permissive
     `parseRelDir`" -}
parseRelDirP ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η RelDir
parseRelDirP (toText → t) =
  let safeLast "" = 𝕹
      safeLast s  = 𝕵 $ Text.last s
   in case safeLast t of
        𝕹  → parse empty
        𝕵 '/' → parse t
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
                [ testCase "r0" $ 𝕽 r0 @=? _parseRelDirP "."
                , testCase "r1" $ 𝕽 r1 @=? _parseRelDirP "r/"
                , testCase "r1" $ 𝕽 r1 @=? _parseRelDirP "r"
                , testCase "r2" $ 𝕽 r2 @=? _parseRelDirP "r/p/"
                , testCase "r2" $ 𝕽 r2 @=? _parseRelDirP "r/p"
                , testCase "r3" $ 𝕽 r3 @=? _parseRelDirP "p/q/r/"
                , testCase "r3" $ 𝕽 r3 @=? _parseRelDirP "p/q/r"
                , testCase "empty" $
                      𝕷 (fPathEmptyE reldirT)  @=? _parseRelDirP ""
                , testCase "no leading /" $
                      𝕷 (fPathAbsE reldirT "/etc/") @=? _parseRelDirP "/etc/"
                , testCase "bad component" $
                      𝕷 illegalCE @=? _parseRelDirP pamNUL
                , testCase "empty component" $
                      𝕷 emptyCompCE @=? _parseRelDirP "etc//pam.d/"
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

r3pN ∷ NonRootRelDir
r3pN = fromSeqNE $ pure [pc|p|]

r3pqN ∷ NonRootRelDir
r3pqN = fromSeqNE $ [pc|p|] ⋖ [[pc|q|]]

----------------------------------------

constructionTests ∷ TestTree
constructionTests = testGroup "construction" [ parseRelDirTests
                                             , parseRelDirPTests
                                             ]

tests ∷ TestTree
tests = testGroup "FPath.RelDir"
                  [ constructionTests, basenameTests, dirnameTests
                  , parentsTests, ancestors'Tests, showTests, textualTests
                  ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
