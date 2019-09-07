{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

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

import NonEmptyContainers.SeqConversions   ( IsMonoSeq( seq ) )
import NonEmptyContainers.SeqNE            ( (⪡) )
import NonEmptyContainers.SeqNEConversions ( IsMonoSeqNonEmpty( seqNE ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir   ( AbsDir )
import FPath.AbsFile  ( AbsFile )
import FPath.DirType  ( DirTypeC( DirType ) )
import FPath.RelDir   ( RelDir )
import FPath.RelFile  ( RelFile )
import FPath.RelType  ( RelTypeC( RelType ) )

--------------------------------------------------------------------------------

infixl 7 ⫻

class (DirTypeC γ, RelTypeC γ) ⇒ AppendableFPath γ where
  (⫻) ∷ DirType γ → RelType γ → γ

instance AppendableFPath AbsDir where
  d ⫻ f = (d ⊣ seq ⊕ f ⊣ seq) ⊣ re seq

instance AppendableFPath AbsFile where
  d ⫻ f = (d ⊣ seq ⪡ f ⊣ seqNE) ⊣ re seqNE

instance AppendableFPath RelDir where
  d ⫻ f = (d ⊣ seq ⊕ f ⊣ seq) ⊣ re seq

instance AppendableFPath RelFile where
  d ⫻ f = (d ⊣ seq ⪡ f ⊣ seqNE) ⊣ re seqNE

infixl 7 </>

-- | non-unicode synonym of `(⫻)`
(</>) ∷ AppendableFPath γ ⇒ DirType γ → RelType γ → γ
(</>) = (⫻)

-- that's all, folks! ----------------------------------------------------------
