{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.T.FPath.TestData where

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Function        ( ($) )

-- fluffy ------------------------------

import NonEmptyContainers.SeqNE  ( (⋖), (⪪) )

-- more-unicode ------------------------

import Data.MoreUnicode.Monoid  ( ф )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqConversions    ( fromSeq )
import NonEmptyContainers.SeqNEConversions  ( fromSeqNE )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath                ( AbsDir, NonRootAbsDir, RelDir )
import FPath.PathComponent  ( pc )

--------------------------------------------------------------------------------

root ∷ AbsDir
root = fromSeq ф

etc ∷ AbsDir
etc = fromSeq $ pure [pc|etc|]

pamd ∷ AbsDir
pamd = fromSeq $ [pc|etc|] ⪪ pure [pc|pam.d|]

wgm ∷ AbsDir
wgm = fromSeq $ [pc|w|] ⪪ [pc|g|] ⪪ pure [pc|M|]

etcN ∷ NonRootAbsDir
etcN = fromSeqNE $ pure [pc|etc|]

pamdN ∷ NonRootAbsDir
pamdN = fromSeqNE $ [pc|etc|] ⪪ pure [pc|pam.d|]

wgmN ∷ NonRootAbsDir
wgmN = fromSeqNE $ [pc|w|] ⪪ [pc|g|] ⪪ pure [pc|M|]

r0 ∷ RelDir
r0 = fromSeq ф

r1 ∷ RelDir
r1 = fromSeqNE $ pure [pc|r|]

r2 ∷ RelDir
r2 = fromSeqNE $ [pc|r|] ⋖ [[pc|p|]]

r3 ∷ RelDir
r3 = fromSeqNE $ [pc|p|] ⋖ [[pc|q|], [pc|r|]]

-- that's all, folks! ----------------------------------------------------------
