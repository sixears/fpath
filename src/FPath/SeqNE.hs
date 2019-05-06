{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

module FPath.SeqNE
  ( {-| A non-empty finite sequence of homogenous things -}
    SeqNE( (:|>), (:<|), (:⫸), (:⫷), unSeqNE )
  , Seqish( (<*|), (|*>) ), pattern (:⪬), pattern (:⪭), (⪬)
  , (⪭), (⪪), (⪫), (⫷), (⫸), (⋖), (⋗)
  , cons, snoc, toSeq, uncons, unsnoc
  , onEmpty, onEmpty', onEmpty_, onEmpty'_
  )
where

import Prelude  ( error, undefined )

-- base --------------------------------

import Control.Applicative  ( Applicative( (<*>), pure ) )
import Data.Bool            ( Bool )
import Data.Eq              ( Eq )
import Data.Foldable        ( Foldable( foldr)  )
import Data.Function        ( ($), id )
import Data.Functor         ( Functor( fmap ) )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Ord             ( Ordering )
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

import Data.MonoTraversable  ( Element, GrowingAppend, MonoFoldable )
import Data.NonNull          ( NonNull, impureNonNull, ncons, nuncons
                             , toNullable )
import Data.Sequences        ( Index, SemiSequence( cons, find, intersperse
                                                  , reverse, snoc, sortBy ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )

--------------------------------------------------------------------------------

{- | Non-Empty Sequence, newtyped from NonNull (Seq α) to allow for additional
     interfaces -}

newtype SeqNE α = SeqNE { unSeqNE ∷ NonNull (Seq α) }
  deriving Eq

type instance Element (SeqNE α) = α

instance Functor SeqNE where
  fmap f (SeqNE ss) = SeqNE ∘ impureNonNull $ f ⊳ (toNullable ss)

instance Applicative SeqNE where
  pure ∷ α → SeqNE α
  pure a = a <|| Seq.Empty

  (<*>) ∷ SeqNE (α → β) → SeqNE α → SeqNE β
  f <*> a = __SeqNE $ toSeq f <*> toSeq a

instance Foldable SeqNE where
  foldr ∷ (α → β → β) → β → SeqNE α → β
  foldr f i = foldr f i ∘ toSeq
  
instance Traversable SeqNE where
  traverse ∷ Applicative φ ⇒ (α → φ β) → SeqNE α → φ (SeqNE β)
  traverse f ss = __SeqNE ⊳ (traverse f $ toSeq ss)

instance GrowingAppend (SeqNE α) where
instance MonoFoldable (SeqNE α) where

instance Show α ⇒ Show (SeqNE α) where
  show (x :⫷ xs) = show x ⊕ " ⋖ " ⊕ show xs

-- UNSAFE only call when you know that the sequence is non-empty
__SeqNE ∷ Seq α → SeqNE α
__SeqNE = SeqNE ∘ impureNonNull
  
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

class ToSeq κ where
  toSeq ∷ κ α → Seq α

instance ToSeq Seq where
  toSeq = id
instance ToSeq SeqNE where
  toSeq = toNullable ∘ unSeqNE
instance ToSeq [] where
  toSeq = Seq.fromList

-- would like to say "SemiSequence κ ⇒ Seqish κ" here, but I can't find the
-- correct syntax
class ToSeq κ ⇒ Seqish κ where

  __UnsafeSmap ∷ (Seq α → Seq α) → κ α → κ α

  infixr 5 <*|
  (<*|) ∷ α → κ α → SeqNE α
  infixl 5 |*>
  (|*>) ∷ κ α → α → SeqNE α

  infixr 5 <||
  (<||) ∷ ToSeq ψ ⇒ α → ψ α → κ α
  infixl 5 ||>
  (||>) ∷ ToSeq ψ ⇒ ψ α → α → κ α

  infixr 5 <|
  (<|) ∷ α → κ α → κ α
  a <| s = __UnsafeSmap (a Seq.<|) s

  infixl 5 |>
  (|>)  ∷ κ α → α → κ α
  s |> a = __UnsafeSmap (Seq.|> a) s

  maybeSeqL ∷ κ α → Maybe (α, Seq α)
  maybeSeqR = maybeSeqR ∘ toSeq
  maybeSeqR ∷ κ α → Maybe (Seq α, α)
  maybeSeqL = maybeSeqL ∘ toSeq

-- unicode synonyms

infixr 5 ⫷
(⫷) ∷ Seqish κ ⇒ α → κ α → SeqNE α
(⫷) = (<*|)
infixl 5 ⫸
(⫸) ∷ Seqish κ ⇒ κ α → α → SeqNE α
(⫸) = (|*>)

infixr 5 ⪪
(⪪) ∷ Seqish κ ⇒ α → κ α → κ α
(⪪) = (<|)
infixl 5 ⪫
(⪫) ∷ Seqish κ ⇒ κ α → α → κ α
(⪫) = (|>)

infixr 5 ⪬
(⪬) ∷ Seqish κ ⇒ α → Seq α → κ α
(⪬) = (<||)
infixl 5 ⪭
(⪭) ∷ Seqish κ ⇒ Seq α → α → κ α
(⪭) = (||>)

instance Seqish Seq where
  -- DO NOT EXPORT THIS.  IT IS UNSAFE.
  -- It could map a sequence to empty, which would be bad for SeqNE
  __UnsafeSmap ∷ (Seq α → Seq α) → Seq α → Seq α
  __UnsafeSmap  = id

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

infixl 5 :|>
pattern (:|>) :: Seqish κ ⇒ Seq α -> α -> κ α
pattern xs :|> x <- (maybeSeqR -> Just (xs,x))

infixl 5 :⪭
pattern (:⪭) :: Seqish κ ⇒ Seq α -> α -> κ α
pattern xs :⪭ x <- (maybeSeqR -> Just (xs,x))

infixr 5 :<|
pattern (:<|) :: Seqish κ ⇒ α -> Seq α -> κ α
pattern x :<| xs <- (maybeSeqL -> Just (x,xs))

infixr 5 :⪬
pattern (:⪬) :: Seqish κ ⇒ α -> Seq α -> κ α
pattern x :⪬ xs <- (maybeSeqL -> Just (x,xs))

uncons ∷ SeqNE α → (α, Seq α)
uncons ss = case nuncons $ unSeqNE ss of
             (a, Nothing) → (a, Seq.Empty)
             (a, Just s)  → (a, toNullable s)

unsnoc ∷ SeqNE α → (Seq α, α)
unsnoc ss = case viewr ∘ toNullable $ unSeqNE ss of
              EmptyR     → error "CONSTRAINT VIOLATION: Empty SeqNE"
              s Seq.:> a → (s,a)

infixl 5 ⋗
(⋗) ∷ ToSeq ψ ⇒ ψ α → α → SeqNE α
(⋗) = (||>)

infixr 5 ⋖
(⋖) ∷ ToSeq ψ ⇒ α → ψ α → SeqNE α
(⋖) = (<||)

infixl 5 :||>
pattern (:||>) :: Seq α -> α -> SeqNE α
pattern xs :||> x <- (unsnoc -> (xs,x))
  where xs :||> x = xs ||> x

infixl 5 :⫸
pattern (:⫸) :: Seq α -> α -> SeqNE α
pattern xs :⫸ x <- (unsnoc -> (xs,x))
  where xs :⫸ x = xs ||> x

infixl 5 :<||
pattern (:<||) :: α -> Seq α -> SeqNE α
pattern x :<|| xs <- (uncons -> (x,xs))
  where x :<|| xs = x <|| xs

infixl 5 :⫷
pattern (:⫷) :: α -> Seq α -> SeqNE α
pattern x :⫷ xs <- (uncons -> (x,xs))
  where x :⫷ xs = x <|| xs

onEmpty ∷ β → (SeqNE α → β) → Seq α → β
onEmpty b f ps = onEmpty' b (f ∘ __SeqNE) ps

onEmpty' ∷ β → (Seq α → β) → Seq α → β
onEmpty' b _ (Seq.Empty) = b
onEmpty' _ f ps          = f ps

onEmpty_ ∷ SeqNE α → Seq α → SeqNE α
onEmpty_ b = onEmpty b id

onEmpty'_ ∷ Seq α → Seq α → Seq α
onEmpty'_ b = onEmpty' b id

-- that's all, folks! ----------------------------------------------------------
