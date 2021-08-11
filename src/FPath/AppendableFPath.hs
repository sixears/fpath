module FPath.AppendableFPath
  ( AppendableFPath( (⫻) ), (</>) )
where

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- lens --------------------------------

import Control.Lens.Review  ( re )

-- more-unicode-symbols ----------------

import Data.MoreUnicode.Lens       ( (⊣) )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqConversions   ( IsSeq( seq ) )
import NonEmptyContainers.SeqNE            ( (⪡) )
import NonEmptyContainers.SeqNEConversions ( IsSeqNonEmpty( seqNE ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir   ( AbsDir )
import FPath.AbsFile  ( AbsFile )
import FPath.RelDir   ( RelDir )
import FPath.RelFile  ( RelFile )

--------------------------------------------------------------------------------

infixl 7 ⫻

class AppendableFPath α β γ | α β → γ where
  (⫻) ∷ α → β → γ

instance AppendableFPath AbsDir RelDir AbsDir where
  d ⫻ f = (d ⊣ seq ⊕ f ⊣ seq) ⊣ re seq

instance AppendableFPath AbsDir RelFile AbsFile where
  d ⫻ f = (d ⊣ seq ⪡ f ⊣ seqNE) ⊣ re seqNE

instance AppendableFPath RelDir RelDir RelDir where
  d ⫻ f = (d ⊣ seq ⊕ f ⊣ seq) ⊣ re seq

instance AppendableFPath RelDir RelFile RelFile where
  d ⫻ f = (d ⊣ seq ⪡ f ⊣ seqNE) ⊣ re seqNE

infixl 7 </>

-- | non-unicode synonym of `(⫻)`
(</>) ∷ AppendableFPath α β γ ⇒ α → β → γ
(</>) = (⫻)

-- that's all, folks! ----------------------------------------------------------
