module Test.Main where

import Moment
import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Prelude.Unicode ((∘))

main :: Effect Unit
main = do
  now' <- now
  log ∘ show $ now' :~+ (duration 1 Days ~* 10)
