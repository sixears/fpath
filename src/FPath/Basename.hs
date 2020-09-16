{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | Classes for methods common to most all FPath types , such as `dirname` -}

module FPath.Basename
  ( Basename( basename, updateBasename ) )
where

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.PathComponent  ( PathComponent )
import FPath.RelType        ( RelTypeC( RelType ) )

--------------------------------------------------------------------------------

class RelTypeC α ⇒ Basename α where
  {- | Well-typed version of `basename` utility; note that `basename` of "/" is
       "/", and `basename` of "./" is "./".
   -}
  -- this would be a bit odd as a lens, as then
  -- [absdir|/|] & basename .~ [pc|foo|] ⇒ [absfile|/foo|]
  basename ∷ α → RelType α
  {- | When applied to "/" or "./", `setBasename` will be a no-op -}
  updateBasename ∷ (PathComponent → PathComponent) → α → α

-- that's all, folks! ----------------------------------------------------------
