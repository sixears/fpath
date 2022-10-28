module FPath.AppendableFPath
  ( AppendableFPath( AppendableFPathD, AppendableFPathF, (⫻) ), (</>) )
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

class AppendableFPath γ where
  type AppendableFPathD γ
  type AppendableFPathF γ
  (⫻) ∷ AppendableFPathD γ → AppendableFPathF γ → γ

instance AppendableFPath AbsDir where
  type AppendableFPathD AbsDir = AbsDir
  type AppendableFPathF AbsDir = RelDir
  d ⫻ f = (d ⊣ seq ⊕ f ⊣ seq) ⊣ re seq

instance AppendableFPath AbsFile where
  type AppendableFPathD AbsFile = AbsDir
  type AppendableFPathF AbsFile = RelFile
  d ⫻ f = (d ⊣ seq ⪡ f ⊣ seqNE) ⊣ re seqNE

instance AppendableFPath RelDir where
  type AppendableFPathD RelDir = RelDir
  type AppendableFPathF RelDir = RelDir
  d ⫻ f = (d ⊣ seq ⊕ f ⊣ seq) ⊣ re seq

instance AppendableFPath RelFile where
  type AppendableFPathD RelFile = RelDir
  type AppendableFPathF RelFile = RelFile
  d ⫻ f = (d ⊣ seq ⪡ f ⊣ seqNE) ⊣ re seqNE

infixl 7 </>

-- | non-unicode synonym of `(⫻)`
(</>) ∷ AppendableFPath γ ⇒ AppendableFPathD γ → AppendableFPathF γ → γ
(</>) = (⫻)

-- that's all, folks! ----------------------------------------------------------
