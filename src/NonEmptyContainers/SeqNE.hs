{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

module NonEmptyContainers.SeqNE
  ( FromNEList( fromNEList )
    {-| A non-empty finite sequence of homogenous things -}
  , SeqNE( SeqNE, (:|>), (:<|), (:<||), (:||>), (:⫸), (:⫷), unSeqNE )
  , Seqish( (<*|), (|*>) ), (<||), (||>), pattern (:⪬), pattern (:⪭), (⪬)
  , (⪭), (⪪), (⪫), (⫷), (⫸), (⋖), (⋗)
  , cons, snoc, toSeq, uncons, unsnoc
  , onEmpty, onEmpty', onEmpty_, onEmpty'_
  )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Applicative  ( Applicative( (<*>), pure ) )
import Data.Bool            ( Bool )
import Data.Eq              ( Eq )
import Data.Foldable        ( Foldable( foldr )  )
import Data.Function        ( ($), id )
import Data.Functor         ( Functor( fmap ) )
import Data.List            ( filter )
import Data.List.NonEmpty   ( NonEmpty )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Ord             ( Ordering, (>) )
import Data.Traversable     ( Traversable, traverse )
import Data.Word            ( Word64 )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

import Data.Sequence  ( Seq, ViewR( EmptyR ), ViewL( EmptyL ), viewr )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, GrowingAppend, MonoFunctor, MonoFoldable
                             , MonoTraversable )
import Data.NonNull          ( NonNull, fromNonEmpty
                             , impureNonNull, ncons, nuncons, toNullable )
import Data.Sequences        ( Index, SemiSequence( cons, find, intersperse
                                                  , reverse, snoc, sortBy ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( suchThat )

--------------------------------------------------------------------------------

class FromNEList α where
  fromNEList ∷ NonEmpty (Element α) → α

------------------------------------------------------------

{- | Non-Empty Sequence, newtyped from NonNull (Seq α) to allow for additional
     interfaces -}

newtype SeqNE α = SeqNE { unSeqNE ∷ NonNull (Seq α) }
  deriving Eq

type instance Element (SeqNE α) = α

--------------------

instance Functor SeqNE where
  fmap f (SeqNE ss) = SeqNE ∘ impureNonNull $ f ⊳ (toNullable ss)

--------------------

instance Applicative SeqNE where
  pure ∷ α → SeqNE α
  pure a = a <|| Seq.Empty

  (<*>) ∷ SeqNE (α → β) → SeqNE α → SeqNE β
  f <*> a = __SeqNE $ toSeq f <*> toSeq a

--------------------

instance Foldable SeqNE where
  foldr ∷ (α → β → β) → β → SeqNE α → β
  foldr f i = foldr f i ∘ toSeq
  
--------------------

instance Traversable SeqNE where
  traverse ∷ Applicative φ ⇒ (α → φ β) → SeqNE α → φ (SeqNE β)
  traverse f ss = __SeqNE ⊳ (traverse f $ toSeq ss)

--------------------

instance GrowingAppend (SeqNE α) where

--------------------

instance MonoFunctor (SeqNE α) where

--------------------

instance MonoFoldable (SeqNE α) where

--------------------

instance MonoTraversable (SeqNE α) where

--------------------

instance Show α ⇒ Show (SeqNE α) where
  show (x :⫷ xs) = show x ⊕ " ⋖ " ⊕ show xs
  show _          = error "failed to uncons SeqNE"

--------------------

instance SemiSequence (SeqNE α) where

  type instance Index (SeqNE α) = Word64

  cons ∷ α → SeqNE α → SeqNE α
  cons a s = SeqNE $ ncons a (toSeq s)
  
  snoc ∷ SeqNE α → α → SeqNE α
  snoc s a = SeqNE ∘ impureNonNull $ (toSeq s) Seq.|> a

  intersperse ∷ α → SeqNE α → SeqNE α
  intersperse a = __UnsafeSmap (intersperse a)

  reverse ∷ SeqNE α → SeqNE α
  reverse = __UnsafeSmap reverse

  find ∷ (α → Bool) → SeqNE α → Maybe α
  find p = find p ∘ toSeq

  sortBy ∷ (α → α → Ordering) → SeqNE α → SeqNE α  
  sortBy f = __UnsafeSmap (sortBy f)

----------------------------------------

instance FromNEList (SeqNE α) where
  fromNEList xs = SeqNE (fromNonEmpty xs)

----------------------------------------

instance Arbitrary α ⇒ Arbitrary (SeqNE α) where
  arbitrary = __SeqNE ⊳ suchThat arbitrary ((> 0) ∘ Seq.length)
  shrink s = __SeqNE ⊳ filter ((> 0) ∘ Seq.length) (shrink (toSeq s))

------------------------------------------------------------

{- | containers that may be converted to a `Seq` -}

class ToSeq κ where
  {- | convert to a `Seq` -}
  toSeq ∷ κ α → Seq α

instance ToSeq Seq where
  toSeq = id
instance ToSeq SeqNE where
  toSeq = toNullable ∘ unSeqNE
instance ToSeq [] where
  toSeq = Seq.fromList

------------------------------------------------------------

-- would like to say "SemiSequence κ ⇒ Seqish κ" here, but I can't find the
-- correct syntax
{- | things that may be treated, particularly (de)composed, as a `Seq` -}
class ToSeq κ ⇒ Seqish κ where
  -- !!! DO NOT EXPORT THIS.  IT IS UNSAFE. !!!
  -- It could map a sequence to empty, which would be bad for SeqNE
  __UnsafeSmap ∷ (Seq α → Seq α) → κ α → κ α

  infixr 5 <*|
  {- | compose a `SeqNE` from the left -}
  (<*|) ∷ α → κ α → SeqNE α
  infixl 5 |*>
  {- | compose a `SeqNE` from the right -}
  (|*>) ∷ κ α → α → SeqNE α

  infixr 5 <||
  {- | compose a `Seqish` from the left, from any `ToSeq` -}
  (<||) ∷ ToSeq ψ ⇒ α → ψ α → κ α
  infixl 5 ||>
  {- | compose a `Seqish` from the right, from any `ToSeq` -}
  (||>) ∷ ToSeq ψ ⇒ ψ α → α → κ α

  infixr 5 <|
  (<|) ∷ α → κ α → κ α
  {- | compose a `Seqish` κ from the left, from another κ -}
  a <| s = __UnsafeSmap (a Seq.<|) s

  infixl 5 |>
  (|>)  ∷ κ α → α → κ α
  {- | compose a `Seqish` κ from the right, from another κ -}
  s |> a = __UnsafeSmap (Seq.|> a) s

  {- | decompose to the left -}
  maybeSeqL ∷ κ α → Maybe (α, Seq α)
  maybeSeqL = maybeSeqL ∘ toSeq
  {- | decompose to the right -}
  maybeSeqR ∷ κ α → Maybe (Seq α, α)
  maybeSeqR = maybeSeqR ∘ toSeq

----------------------------------------
--          unicode synonyms          --
----------------------------------------

infixr 5 ⫷
{- | synonym for `(<*|)` -}
(⫷) ∷ Seqish κ ⇒ α → κ α → SeqNE α
(⫷) = (<*|)

--------------------

infixl 5 ⫸
{- | synonym for `(|*>)` -}
(⫸) ∷ Seqish κ ⇒ κ α → α → SeqNE α
(⫸) = (|*>)

----------------------------------------

infixr 5 ⪪
{- | synonym for `(<|)` -}
(⪪) ∷ Seqish κ ⇒ α → κ α → κ α
(⪪) = (<|)

--------------------

infixl 5 ⪫
{- | synonym for `(|>)` -}
(⪫) ∷ Seqish κ ⇒ κ α → α → κ α
(⪫) = (|>)

----------------------------------------

infixr 5 ⪬
{-| synonym for `(<||)` -}
(⪬) ∷ Seqish κ ⇒ α → Seq α → κ α
(⪬) = (<||)

--------------------

infixl 5 ⪭
{-| synonym for `(||>)` -}
(⪭) ∷ Seqish κ ⇒ Seq α → α → κ α
(⪭) = (||>)

----------------------------------------

instance Seqish Seq where
  __UnsafeSmap ∷ (Seq α → Seq α) → Seq α → Seq α
  __UnsafeSmap = id

  (<*|) ∷ α → Seq α → SeqNE α
  a <*| s = SeqNE $ ncons a s

  (|*>) ∷ Seq α → α → SeqNE α
  s |*> a = SeqNE ∘ impureNonNull $ s Seq.|> a

  (|>)  ∷ Seq α → α → Seq α
  (|>)  = (Seq.|>)

  (||>) ∷ ToSeq ψ ⇒ ψ α → α → Seq α
  s ||> a = toSeq s Seq.|> a

  (<|)  ∷ α → Seq α → Seq α
  (<|)  = (Seq.<|)

  (<||) ∷ ToSeq ψ ⇒ α → ψ α → Seq α
  a <|| s = a Seq.<| toSeq s
  
  maybeSeqR ss = case Seq.viewr ss of
                   EmptyR  → Nothing
                   s Seq.:> a → Just (s,a)

  maybeSeqL ss = case Seq.viewl ss of
                   EmptyL  → Nothing
                   a Seq.:< s → Just (a,s)

----------------------------------------

instance Seqish SeqNE where
  __UnsafeSmap ∷ (Seq α → Seq α) → SeqNE α → SeqNE α
  __UnsafeSmap f  = SeqNE ∘ impureNonNull ∘ f ∘ toSeq

  (<*|) ∷ α → SeqNE α → SeqNE α
  a <*| s = SeqNE $ ncons a (toSeq s)

  (|*>) ∷ SeqNE α → α → SeqNE α
  s |*> a = SeqNE ∘ impureNonNull $ (toSeq s) Seq.|> a

  (||>) ∷ ToSeq ψ ⇒ ψ α → α → SeqNE α
  s ||> a = SeqNE $ impureNonNull $ toSeq s Seq.|> a

  (<||) ∷ ToSeq ψ ⇒ α → ψ α → SeqNE α
  a <|| s = SeqNE $ impureNonNull $ a Seq.<| toSeq s

----------------------------------------

infixl 5 :|>
{- | pattern rightwards decomposition of a `Seqish` κ -}
pattern (:|>) :: Seqish κ ⇒ Seq α -> α -> κ α
pattern xs :|> x <- (maybeSeqR -> Just (xs,x))

infixl 5 :⪭
{- | pattern rightwards decomposition of a `Seqish` κ -}
pattern (:⪭) :: Seqish κ ⇒ Seq α -> α -> κ α
pattern xs :⪭ x <- (maybeSeqR -> Just (xs,x))

infixr 5 :<|
{- | pattern leftwards decomposition of a `Seqish` κ -}
pattern (:<|) :: Seqish κ ⇒ α -> Seq α -> κ α
pattern x :<| xs <- (maybeSeqL -> Just (x,xs))

infixr 5 :⪬
{- | pattern leftwards decomposition of a `Seqish` κ -}
pattern (:⪬) :: Seqish κ ⇒ α -> Seq α -> κ α
pattern x :⪬ xs <- (maybeSeqL -> Just (x,xs))

{- | decompose a `SeqNE` leftwards -}
uncons ∷ SeqNE α → (α, Seq α)
uncons ss = case nuncons $ unSeqNE ss of
              (a, Nothing) → (a, Seq.Empty)
              (a, Just s)  → (a, toNullable s)

{- | decompose a `SeqNE` rightwards -}
unsnoc ∷ SeqNE α → (Seq α, α)
unsnoc ss = case viewr ∘ toNullable $ unSeqNE ss of
              EmptyR     → -- should never happen
                           error "CONSTRAINT VIOLATION: Empty SeqNE"
              s Seq.:> a → (s,a)

infixr 5 ⋖
{- | compose a `SeqNE α` from a `ToSeq α` and an `α` (leftwards) -}
(⋖) ∷ ToSeq ψ ⇒ α → ψ α → SeqNE α
(⋖) = (<||)

infixl 5 ⋗
{- | compose a `SeqNE α` from a `ToSeq α` and an `α` (rightwards) -}
(⋗) ∷ ToSeq ψ ⇒ ψ α → α → SeqNE α
(⋗) = (||>)


infixl 5 :<||
{- | compose a `SeqNE α` from a `Seq α` and an `α` (leftwards) -}
pattern (:<||) :: α -> Seq α -> SeqNE α
pattern x :<|| xs <- (uncons -> (x,xs))
  where x :<|| xs = x <|| xs

infixl 5 :⫷
{- | compose a `SeqNE α` from a `Seq α` and an `α` (leftwards) -}
pattern (:⫷) :: α -> Seq α -> SeqNE α
pattern x :⫷ xs <- (uncons -> (x,xs))
  where x :⫷ xs = x <|| xs

infixl 5 :||>
{- | compose a `SeqNE α` from a `Seq α` and an `α` (rightwards) -}
pattern (:||>) :: Seq α -> α -> SeqNE α
pattern xs :||> x <- (unsnoc -> (xs,x))
  where xs :||> x = xs ||> x

infixl 5 :⫸
{- | compose a `SeqNE α` from a `Seq α` and an `α` (rightwards) -}
pattern (:⫸) :: Seq α -> α -> SeqNE α
pattern xs :⫸ x <- (unsnoc -> (xs,x))
  where xs :⫸ x = xs ||> x

----------------------------------------

-- UNSAFE only call when you know that the sequence is non-empty
-- !!! DO NOT EXPORT !!!
__SeqNE ∷ Seq α → SeqNE α
__SeqNE = SeqNE ∘ impureNonNull
  
{- | apply a fn to a `Seq`; but if the `Seq` is empty, use a given sentinel
     value -}
onEmpty ∷ β → (SeqNE α → β) → Seq α → β
onEmpty b f ps = onEmpty' b (f ∘ __SeqNE) ps

{- | apply a fn to a `Seq`; but if the `Seq` is empty, use a given sentinel
     value -}
onEmpty' ∷ β → (Seq α → β) → Seq α → β
onEmpty' b _ (Seq.Empty) = b
onEmpty' _ f ps          = f ps

{- | type-convert a `Seq` to a `SeqNE`, using a sentinel value for an empty
     `Seq` -}
onEmpty_ ∷ SeqNE α → Seq α → SeqNE α
onEmpty_ b = onEmpty b id

{- | if a `Seq` is empty, replace it with a given sentinel value -}
onEmpty'_ ∷ Seq α → Seq α → Seq α
onEmpty'_ b = onEmpty' b id

-- that's all, folks! ----------------------------------------------------------