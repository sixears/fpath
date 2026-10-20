{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FPath.FilenameExtension
  ( FilenameExtension
  , HasFilenameExtension( filenameExtension, filenameExtensionPC )
  , HasFilenameExtensionMay( filenameExtensionMay, filenameExtensionPCMay )
  , fe, filenameExtension_

  , tests
  )
where

import Base1T

-- base --------------------------------

import Data.Data      ( Data )
import Data.List      ( any )
import Data.Typeable  ( Proxy( Proxy ), TypeRep, typeRep )
import GHC.Generics   ( Generic )

-- base-plus ---------------------------

import BasePlus.New  ( New( new ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( iso )

-- monaderror-io -----------------------

import MonadError  ( ѭ, mapMError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊩) )

-- quasiquoting ------------------------

import QuasiQuoting  ( QuasiQuoter, mkQQ, exp )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertIsNothing, assertJust )

-- template-haskell --------------------

import Language.Haskell.TH  ( ExpQ, appE, conE )
import Language.Haskell.TH.Syntax  ( Lift( liftTyped ), TExp( TExp ), liftCode )

-- text --------------------------------

import qualified  Data.Text  as  T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Error.FPathError
                            ( AsFPathError( _FPathError )
                            , FPathError ( FPathComponentE ) )
import FPath.Error.FPathComponentError
                            ( AsFPathComponentError, FPathComponentError
                            , __FPathCEmptyE__, __FPathCIllegalE__
                            , __FPathCIllegalCharE__
                            , fPathComponentIllegalCharE, fPathIllegalE
                            )
import FPath.Parseable      ( Parseable( parse, parseFPE )
                            , __parse__, __parseS__ )
import FPath.PathComponent  ( HasPathComponent( pathComponent )
                            , IsPathComponent( pathComponent' )
                            , PathComponent
                            , pc, splitExt
                            )

--------------------------------------------------------------------------------

filenameExtensionT ∷ TypeRep
filenameExtensionT = typeRep (Proxy ∷ Proxy FilenameExtension)

------------------------------------------------------------

{-| filename extension, e.g., to be appended after a `.` character -}
newtype FilenameExtension =
  FilenameExtension { unFilenameExtension ∷ PathComponent }
  deriving stock   (Eq,Data,Generic)
  deriving newtype (NFData,Printable,Semigroup,Show)

----------

instance New FilenameExtension PathComponent  where  new = FilenameExtension

----------

instance Lift FilenameExtension where
  liftTyped (FilenameExtension t) =
    liftCode $ appE (conE 'FilenameExtension) [| t |] ≫ return ∘ TExp

----------

instance IsPathComponent FilenameExtension where
  pathComponent' = iso unFilenameExtension FilenameExtension

----------

instance HasPathComponent FilenameExtension where
  pathComponent = lens unFilenameExtension (\ _ p → FilenameExtension p)

------------------------------------------------------------

{-| things that have a `FilenameExtension` -}
class HasFilenameExtension α where
  {-| access to `FilenameExtension` of an α -}
  filenameExtension   ∷ Lens' α FilenameExtension
  {-| direct access to `PathComponent` of `FilenameExtension` of an `α` -}
  filenameExtensionPC ∷ Lens' α PathComponent
  filenameExtensionPC =
    lens (unFilenameExtension ∘ view filenameExtension)
         (\ a p → a & filenameExtension ⊢ FilenameExtension p)

------------------------------------------------------------

{-| parse a `FilenameExtension` from a `Printable`-}
parseFE ∷ ∀ ε τ η . (Printable τ,AsFPathComponentError ε,MonadError ε η) =>
          τ → η FilenameExtension
parseFE (toString → "")                 = __FPathCEmptyE__
parseFE (toString → s) | head s ≡ 𝓙 '.' = __FPathCIllegalE__ s
parseFE (toString → s) | any (≡ '\0') s = __FPathCIllegalCharE__ '\0' s
                       | any (≡ '/')  s = __FPathCIllegalCharE__ '/' s
                       | otherwise      = return $
                                            new @_ @PathComponent (__parse__ s)

----------------------------------------

instance Parseable FilenameExtension where
  parse (toText → t) =
    mapMError (\ e → _FPathError # FPathComponentE e filenameExtensionT t) $
      parseFE t

----------

parseTests ∷ TestTree
parseTests =
  let parseFE_ ∷ MonadError FPathError η ⇒ 𝕋 → η FilenameExtension
      parseFE_ = parseFPE

      feFromString = FilenameExtension ∘ __parseS__

      illegalCE ∷ 𝕋 → ℂ → TestTree
      illegalCE t c = let fpcice = fPathComponentIllegalCharE c (T.unpack t)
                          fpcice' = FPathComponentE fpcice filenameExtensionT t
                       in testCase ("illegal character: " ⊕ [c] ⊕ "'") $
                            Left fpcice' @=? parseFE_ t

      illegalPC t = let s = toString t
                        fpie e = fPathIllegalE e
                        fpie' e f = FPathComponentE (fpie e) filenameExtensionT f
                     in testCase ("illegal path component '" ⊕ s ⊕ "'") $
                          Left (fpie' s t) @=? parseFE_ t

      pc1 = feFromString "r.e"
      pc3 = feFromString "r."
      pc4 = feFromString "r"
      pc6 = feFromString "…"
   in testGroup "parseFE"
                [ testCase "pc1" $ Right pc1 @=? parseFPE @_ @𝕋 "r.e"
                , testCase "pc3" $ Right pc3 @=? parseFPE @_ @𝕋 "r."
                , testCase "pc4" $ Right pc4 @=? parseFPE @_ @𝕋 "r"
                , testCase "pc6" $ Right pc6 @=? parseFPE @_ @𝕋 "…"
                , illegalCE "bob/" '/'
                , illegalPC "."
                , illegalPC ".e"
                , illegalPC ".."
                , illegalPC "..."
                ]

----------------------------------------

{-| template-haskellized `parseFE` -}
filenameExtensionQQ ∷ String → Maybe ExpQ
filenameExtensionQQ =
  (\ p → ⟦p⟧) ⩺ (ѭ ∘ parseFE @FPathComponentError)

{-| quasi-quoter for `FilenameExtension` -}
filenameExtension_ ∷ QuasiQuoter
filenameExtension_ = mkQQ "FilenameExtension" $ def & exp ⊩ filenameExtensionQQ

{- | abbreviation for `filenameExtension_` -}
fe ∷ QuasiQuoter
fe = filenameExtension_

----------

instance HasFilenameExtension FilenameExtension where
  filenameExtension = lens id (const id)

------------------------------------------------------------

{-| things that may have a `FilenameExtension` -}
class HasFilenameExtensionMay α where
  {-| access to `FilenameExtension` of an α -}
  filenameExtensionMay   ∷ Lens' α (𝕄 FilenameExtension)
  {-| direct access to `PathComponent` of `FilenameExtension` of an `α` (if it
      has one) -}
  filenameExtensionPCMay ∷ Lens' α (𝕄 PathComponent)
  filenameExtensionPCMay =
    lens (unFilenameExtension ⩺ view filenameExtensionMay)
         (\ a p → a & filenameExtensionMay ⊢ (FilenameExtension ⊳ p))

----------

instance HasFilenameExtensionMay PathComponent where
  filenameExtensionMay =
    lens
      (\ pc' → FilenameExtension ⊳ snd (splitExt pc'))
      (\ pc' mfe →
         let (f,_) = splitExt pc'
         in  maybe f (__parse__ ∘ (toString f ◇ "." ◇) ∘ toString) mfe)


--------------------

hasFilenameExtensionMayTests ∷ TestTree
hasFilenameExtensionMayTests =
  let foo    = [pc|foo|]
      fooDot = [pc|foo.|]
      fooBar = [pc|foo.bar|]
      fooBaz = [pc|foo.baz|]
      bar    = FilenameExtension [pc|bar|]
      baz    = FilenameExtension [pc|baz|]
      testC p a = testCase (toString p) $ a p
   in testGroup "filenameExtensionMay"
        [ testGroup "getting"
          [ testC foo    $ assertIsNothing ∘ (⊣ filenameExtensionMay)
          , testC fooDot $ assertIsNothing ∘ (⊣ filenameExtensionMay)
          , testC fooBar $ assertJust (bar ≟) ∘ (⊣ filenameExtensionMay)
          ]
        , testGroup "setting"
          [ testCase "foo     ← 𝓝" $
              foo    ≟ (foo & filenameExtensionMay ⊢ 𝓝)
          , testCase "foo.    ← 𝓝" $
              fooDot ≟ (fooDot & filenameExtensionMay ⊢ 𝓝)
          , testCase "foo.bar ← 𝓝" $
              foo    ≟ (fooBar & filenameExtensionMay ⊢ 𝓝)
          , testCase "foo  ← bar" $
              fooBar ≟ (foo & filenameExtensionMay ⊢ 𝓙 bar)
          , testCase "foo. ← bar" $
              [pc|foo..bar|] ≟ (fooDot & filenameExtensionMay ⊢ 𝓙 bar)
          , testCase "foo.bar ← baz" $
              fooBaz ≟ (fooBar & filenameExtensionMay ⊢ 𝓙 baz)
          ]
        ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

{-| unit tests for this module -}
tests ∷ TestTree
tests =
  testGroup "FilenameExtension" [ hasFilenameExtensionMayTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
