module Moment
  ( Moment
  , Duration
  , Increment(..)
  , now
  , fromUTC
  , fromDate
  , add
  , subtract
  , diff
  , duration
  , asMilliseconds
  , fromMilliseconds
  , isBefore
  , isAfter
  , isSame
  , seconds
  , addDurationToMoment
  , (:~+)
  , addDurations
  , (~+)
  , multDuration
  , (~*)
  ) where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldl)
import Data.Function.Uncurried (Fn2, Fn3, Fn1, runFn1, runFn2, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (JSDate)
import Effect (Effect)
import Foreign (readNumber)
import Foreign.Generic.Class (class Decode, class Encode, encode)
import Prelude.Unicode ((∘))

foreign import data Moment :: Type

foreign import data Duration :: Type

instance encodeDuration :: Encode Duration where
  encode = encode ∘ asMilliseconds

instance decodeDuration :: Decode Duration where
  decode = map fromMilliseconds ∘ readNumber

data Increment
  = Months
  | Days
  | Years
  | Hours
  | Minutes
  | Seconds

derive instance genericIncrement :: Generic Increment _

instance showIncrement :: Show Increment where
  show = genericShow

instance showMoment :: Show Moment where
  show = format

-- Wrapped methods from moment js.
foreign import _add :: Fn3 Int String Moment Moment

foreign import _addDurations :: Fn2 Duration Duration Duration

foreign import _addDurationToMoment :: Fn2 Moment Duration Moment

foreign import _subtract :: Fn3 Int String Moment Moment

foreign import _duration :: Fn2 Int String Duration

foreign import _fromUTC :: Fn1 String Moment

foreign import _fromDate :: Fn1 JSDate Moment

foreign import _isAfter :: Fn2 Moment Moment Boolean

foreign import _isBefore :: Fn2 Moment Moment Boolean

foreign import _isSame :: Fn2 Moment Moment Boolean

foreign import _diff :: Fn2 Moment Moment Duration

foreign import _seconds :: Fn1 Duration Int

foreign import _now :: Effect Moment

foreign import _format :: Fn1 Moment String

foreign import asMilliseconds :: Duration -> Number
foreign import fromMilliseconds :: Number -> Duration

infixl 6 addDurationToMoment as :~+

infixl 6 multDuration as ~*

infixl 6 addDurations as ~+

format :: Moment -> String
format = runFn1 _format

add :: Int -> Increment -> Moment -> Moment
add mult inc m = (runFn3 _add) mult (show inc) m

subtract :: Int -> Increment -> Moment -> Moment
subtract mult inc m = (runFn3 _subtract) mult (show inc) m

duration :: Int -> Increment -> Duration
duration mult inc = (runFn2 _duration) mult (show inc)

addDurationToMoment :: Moment -> Duration -> Moment
addDurationToMoment = runFn2 _addDurationToMoment

addDurations :: Duration -> Duration -> Duration
addDurations = runFn2 _addDurations

fromUTC :: String -> Moment
fromUTC = runFn1 _fromUTC

fromDate :: JSDate -> Moment
fromDate = runFn1 _fromDate

isAfter :: Moment -> Moment -> Boolean
isAfter = runFn2 _isAfter

isBefore :: Moment -> Moment -> Boolean
isBefore = runFn2 _isBefore

isSame :: Moment -> Moment -> Boolean
isSame = runFn2 _isSame

now :: Effect Moment
now = _now

diff :: Moment -> Moment -> Duration
diff = runFn2 _diff

seconds :: Duration -> Int
seconds = runFn1 _seconds

multDuration :: Duration -> Int -> Duration
multDuration d n = crunch (n - 1) (addDurations d) d

crunch :: forall a. Int -> (a -> a) -> a -> a
crunch 0 f init = init
crunch n f init = foldl (\m _ -> f m) init (0 .. (n - 1))
