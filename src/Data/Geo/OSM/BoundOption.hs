-- | A bound-option is either a @Bound@, @Bounds@ or empty.
module Data.Geo.OSM.BoundOption
(
  BoundOption
, foldBoundOption
, optionBound
, optionBounds
, optionEmptyBound
) where

import Data.Geo.OSM.Bound
import Data.Geo.OSM.Bounds

data BoundOption =
  OptionBound Bound
  | OptionBounds Bounds
  | Empty
  deriving Eq

foldBoundOption ::
  (Bound -> x)
  -> (Bounds -> x)
  -> x
  -> BoundOption
  -> x
foldBoundOption f _ _ (OptionBound b) =
  f b
foldBoundOption _ f _ (OptionBounds b) =
  f b
foldBoundOption _ _ f Empty =
  f

optionBound ::
  Bound
  -> BoundOption
optionBound =
  OptionBound

optionBounds ::
  Bounds
  -> BoundOption
optionBounds =
  OptionBounds

optionEmptyBound ::
  BoundOption
optionEmptyBound =
  Empty
