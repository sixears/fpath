{-# LANGUAGE UnicodeSyntax #-}

-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty           ( defaultIngredients, testGroup )
import Test.Tasty.Runners   ( defaultMainWithIngredients )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified NonEmptyContainers.T.SeqConversions
import qualified NonEmptyContainers.T.SeqNE

--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMainWithIngredients defaultIngredients $
         testGroup "non-empty-containers"
                   [ NonEmptyContainers.T.SeqConversions.tests
                   , NonEmptyContainers.T.SeqNE.tests
                   ]

-- that's all, folks! ----------------------------------------------------------
