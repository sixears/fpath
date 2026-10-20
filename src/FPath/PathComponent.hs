{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | A permissable file name (excluding any directory part), thus a sequence of
     characters excluding the null character (`\0`) or the slash character (`/`)
     and also explicitly excluding the special components `.` and `..` .
 -}
module FPath.PathComponent
  ( HasPathComponent(..), IsPathComponent(..), PathComponent
  , (⊙)
  , addExt, ext, parsePathC, pathcomponent, pc, reverse
  , splitExt, stub, toLower, toUpper, updateExt

  , tests
  )
where

import Base1T

-- base --------------------------------

import Data.Char      ( Char, chr )
import Data.Data      ( Data )
import Data.List      ( any, elem, filter, find, nub, subsequences )
import Data.Monoid    ( mconcat )
import Data.Typeable  ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Generics   ( Generic )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ), fromString )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ), isValid )

-- genvalidity-bytestring --------------

import Data.GenValidity.ByteString  ( )

-- genvalidity-text --------------------

import Data.GenValidity.Text  ( )

-- lens --------------------------------

import Control.Lens.Iso  ( Iso' )

-- monaderror-io -----------------------

import MonadError  ( ѭ, mapMError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊩) )

-- parsers -----------------------------

import Text.Parser.Char  ( char, noneOf, string )

-- quasiquoting ------------------------

import QuasiQuoting  ( QuasiQuoter, mkQQ, exp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( suchThat )

-- tasty-plus --------------------------

import TastyPlus  ( (≟) )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ, appE, conE )
import Language.Haskell.TH.Syntax  ( Lift( liftTyped ), TExp( TExp ), liftCode )

-- text --------------------------------

import qualified  Data.Text  as  T
import Data.Text  ( break, pack, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- th-lift-instances -------------------

import Instances.TH.Lift ()

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AsFilePath         ( AsFilePath( filepath ) )
import FPath.AsFilePath'        ( AsFilePath'( filepath' ) )
import FPath.DirType            ( DirTypeC( DirType ) )
import FPath.Error.FPathError   ( AsFPathError( _FPathError )
                                , FPathError ( FPathComponentE ) )
import FPath.Error.FPathComponentError
                                ( AsFPathComponentError, FPathComponentError
                                , __FPathCEmptyE__, __FPathCIllegalE__
                                , __FPathCIllegalCharE__
                                , fPathComponentIllegalCharE, fPathIllegalE
                                )
import FPath.Parseable          ( Parseable( parse, parseFPE ) )
import FPath.Util               ( mkVisS )

--------------------------------------------------------------------------------

pathComponentT ∷ TypeRep
pathComponentT = typeRep (Proxy ∷ Proxy PathComponent)

{-| file name components may contain any character 'cept NUL and SLASH -}
badChars :: [Char]
badChars = (chr ∘ fromIntegral @Word8) ⊳ [0,47] -- '\0', '/'

{- | A single component in a Path, that is, a directory or file ---
     notably no slashes are allowed (or nul chars); must not be empty
 -}
newtype PathComponent = PathComponent { unPathComponent ∷ 𝕋 }
  deriving newtype (NFData,Semigroup,Show)
  deriving stock   (Eq,Data,Generic)

----------

instance Printable PathComponent where
  print (PathComponent t) = P.text t

----------

instance Lift PathComponent where
  liftTyped (PathComponent t) =
    liftCode $ appE (conE 'PathComponent) [| t |] ≫ return ∘ TExp

----------

instance Validity PathComponent where
  validate p =
    let badCs = declare "has neither NUL nor /" $
                  Nothing ≡ find (`elem` badChars) (toString p)
        nonEmpty = declare "is not empty" $ "" ≢ toString p
        nonDotties = declare "is not \".\" or \"..\"" $
                       "." ≢ toString p ∧ ".." ≢ toString p

     in mconcat [ badCs, nonEmpty, nonDotties ]

----------

instance GenValid PathComponent where
  genValid = (PathComponent ⊳ genValid) `suchThat` isValid
  shrinkValid (PathComponent t) = PathComponent ⊳ shrinkValid t

----------

instance Textual PathComponent where
  textual =
    let parseChars = noneOf badChars
        parseNoDotsNorBads = noneOf (badChars ⊕ ['.'])
        jjoin a b ys = a : b : ys
        matches =   ((:) ⊳ parseNoDotsNorBads ⊵ many parseChars)
                  ∤ ((⊕) ⊳ string ".." ⊵ some parseChars)
                  ∤ (jjoin ⊳ char '.' ⊵ parseNoDotsNorBads ⊵ many parseChars)

     in PathComponent ∘ pack ⊳ matches

instance DirTypeC PathComponent where
  -- needed for `FileLike`
  type DirType PathComponent = ()

----------

instance AsFilePath PathComponent where
  filepath = prism' toString fromString

--------------------

filepathTests ∷ TestTree
filepathTests =
  let fail s  = testCase s $ Nothing @PathComponent @=? s ⩼ filepath
      foo     = PathComponent "foo"
      foo_bar = PathComponent "foo.bar"
   in testGroup "filepath"
            [ testCase "foo"     $ "foo"     ≟ foo ⫥ filepath
            , testCase "foo.bar" $ "foo.bar" ≟ foo_bar ⫥ filepath
            , testCase "..." $ "..." ≟ (PathComponent "...") ⫥ filepath
            , fail ""
            , fail "/foo"
            , fail "f/oo"
            , fail "foo/"
            , fail "/foo"
            , fail "f/oo"
            , fail "."
            , fail ".."
            ]

--------------------

instance AsFilePath' PathComponent where
  filepath' = filepath

filepath'Tests ∷ TestTree
filepath'Tests =
  let fail s  = testCase s $ Nothing @PathComponent @=? s ⩼ filepath'
      foo     = PathComponent "foo"
      foo_bar = PathComponent "foo.bar"
   in testGroup "filepath'"
            [ testCase "foo"     $ "foo"     ≟ foo ⫥ filepath'
            , testCase "foo.bar" $ "foo.bar" ≟ foo_bar ⫥ filepath'
            , fail ""
            , fail "/foo"
            , fail "f/oo"
            , fail "foo/"
            ]

--------------------

instance Arbitrary PathComponent where
  arbitrary = genValid

  -- shrink by -) trying proper substrings
  --           -) replacing non-alphanumeric characters with alphanums
  -- per the QuickCheck doc, try the more aggressive efforts first
  shrink (PathComponent p) =
    let subs ∷ [String]
        subs = subsequences (unpack p)
        all_subs ∷ [String]
        all_subs = nub $ (mkVisS ⊳ subs) ⊕ subs
        filt_subs ∷ [String] → [String]
        filt_subs = filter $ \ t → t ≢ unpack p ∧ t ≢ ""
     in fmap (PathComponent ∘ pack) $ filt_subs all_subs

----------------------------------------

parsePathC ∷ ∀ ε τ η . (Printable τ, AsFPathComponentError ε, MonadError ε η) ⇒
             τ → η PathComponent
parsePathC (toString → "")                 = __FPathCEmptyE__
parsePathC (toString → ".")                = __FPathCIllegalE__ "."
parsePathC (toString → "..")               = __FPathCIllegalE__ ".."
parsePathC (toString → t) | any (≡ '\0') t = __FPathCIllegalCharE__ '\0' t
                          | any (≡ '/')  t = __FPathCIllegalCharE__ '/' t
                          | otherwise      = return $ PathComponent (pack t)

----------------------------------------

instance Parseable PathComponent where
  parse (toText → t) =
    mapMError (\ e → _FPathError # FPathComponentE e pathComponentT t) $
      parsePathC t

----------

parseTests ∷ TestTree
parseTests =
  let parsePathComponent_ ∷ MonadError FPathError η ⇒ 𝕋 → η PathComponent
      parsePathComponent_ = parseFPE

      illegalCE ∷ 𝕋 → ℂ → TestTree
      illegalCE t c = let fpcice = fPathComponentIllegalCharE c (unpack t)
                          fpcice' = FPathComponentE fpcice pathComponentT t
                       in testCase ("illegal character: " ⊕ [c] ⊕ "'") $
                            Left fpcice' @=? parsePathComponent_ t

      illegalPC t = let s = toString t
                        fpie e = fPathIllegalE e
                        fpie' e f = FPathComponentE (fpie e) pathComponentT f
                     in testCase ("illegal path component '" ⊕ s ⊕ "'") $
                          Left (fpie' s t) @=? parsePathComponent_ t

      pc1 = PathComponent "r.e"
      pc2 = PathComponent ".e"
      pc3 = PathComponent "r."
      pc4 = PathComponent "r"
      pc5 = PathComponent "..."
      pc6 = PathComponent "…"
   in testGroup "parsePathComponent"
                [ testCase "pc1" $ Right pc1 @=? parseFPE @_ @𝕋 "r.e"
                , testCase "pc2" $ Right pc2 @=? parseFPE @_ @𝕋 ".e"
                , testCase "pc3" $ Right pc3 @=? parseFPE @_ @𝕋 "r."
                , testCase "pc4" $ Right pc4 @=? parseFPE @_ @𝕋 "r"
                , testCase "pc5" $ Right pc5 @=? parseFPE @_ @𝕋 "..."
                , testCase "pc6" $ Right pc6 @=? parseFPE @_ @𝕋 "…"
                , illegalCE "bob/" '/'
                , illegalPC "."
                , illegalPC ".."
                ]

----------------------------------------

class    IsPathComponent α             where pathComponent'∷Iso' α PathComponent
instance IsPathComponent PathComponent where pathComponent'= id

----------------------------------------

class HasPathComponent α where pathComponent ∷ Lens' α PathComponent
instance HasPathComponent PathComponent where pathComponent =lens id (const id)

----------------------------------------

parsePathC' ∷ (Printable τ, MonadError FPathComponentError η) ⇒
              τ → η PathComponent
parsePathC' = parsePathC

{-
__parsePathC__ ∷ Printable τ ⇒ τ → PathComponent
__parsePathC__ = either __ERROR'__ id ∘ parsePathC'

__parsePathC'__ ∷ String → PathComponent
__parsePathC'__ = __parsePathC__
-}

pathComponentQQ ∷ String → Maybe ExpQ
pathComponentQQ = (\ p → ⟦p⟧) ⩺ (ѭ ∘ parsePathC')

{-| quasi-quoter for PathComponent -}
pathcomponent ∷ QuasiQuoter
pathcomponent = mkQQ "PathComponent" $ def & exp ⊩ pathComponentQQ

{- | abbreviation for `pathComponent_` -}
pc ∷ QuasiQuoter
pc = pathcomponent

----------------------------------------

-- don't do this!  In general, omaps over monofunctor aren't safe, as
-- they may map the "inner" "string" to something invalid (containing /, or \0,
-- or empty).  And if we declare this instance here, then it will get exported -
-- no way to stop that :-(.
{-
type instance Element PathComponent = PathCInner

instance MonoFunctor PathComponent where
  omap = _
-}

-- don't export this, it wouldn't be safe... it must only be used with functions
-- that map a Valid PathComponent to a Valid PathComponent
pcmap ∷ HasPathComponent γ => (𝕋 → 𝕋) → γ → γ
pcmap f x =
  x & pathComponent ⊢ PathComponent (f ∘ unPathComponent $ x ⊣ pathComponent)

toUpper ∷ HasPathComponent γ => γ → γ
toUpper = pcmap T.toUpper

toLower ∷ HasPathComponent γ => γ → γ
toLower = pcmap T.toLower

reverse ∷ HasPathComponent γ => γ → γ
reverse = pcmap T.reverse

----------------------------------------

{- | Add an "extension", that is, join two `PathComponent`s with a '.'
     character
 -}
addExt ∷ PathComponent → PathComponent → PathComponent
addExt (PathComponent pfx) (PathComponent sfx) = PathComponent $ pfx ⊕ "." ⊕ sfx

{- | operator alias for `addExt` -}
infixr 6 ⊙ -- same as for ⊕
(⊙) ∷ PathComponent → PathComponent → PathComponent
(⊙) = addExt

splitExt ∷ PathComponent → (PathComponent, Maybe PathComponent)
splitExt p@(PathComponent xs) =
  case break (≡ '.') (T.reverse xs) of
    (_,"")    → (p, Nothing)
    ("",_)    → (p, Nothing)
    (sfx,pfx) → (PathComponent $ T.init (T.reverse pfx),
                 Just $ PathComponent (T.reverse sfx))

stub ∷ PathComponent → PathComponent
stub = fst ∘ splitExt

stubTests ∷ TestTree
stubTests =
  let foo    = PathComponent "foo"
      fooBar = PathComponent "foo.bar"
      fooDot = PathComponent "foo."
   in testGroup "stub" [ testCase "foo.bar" $ foo    ≟ stub fooBar
                       , testCase "foo"     $ foo    ≟ stub foo
                       , testCase "foo."    $ fooDot ≟ stub fooDot
                       ]

ext ∷ PathComponent → Maybe PathComponent
ext = snd ∘ splitExt

updateExt ∷ (PathComponent → PathComponent) → PathComponent → PathComponent
updateExt f p@(splitExt → (s, x)) = case x of
                                       Just e  → s ⊙ f e
                                       Nothing → p

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "PathComponent" [ filepathTests, filepath'Tests, parseTests
                            , stubTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
