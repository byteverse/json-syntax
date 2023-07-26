-- Compile this with -02 to test that the rewrite rule for
-- objectFromList works correctly.
module Buildjson (tripleMember) where

import qualified Json

tripleMember :: Json.Member -> Json.Value
{-# noinline tripleMember #-}
tripleMember x = Json.objectFromList [x,x,x]
