-- | A bound-option is either a @Bound@, @Bounds@ or empty.
module Data.Geo.OSM.BoundOption
(
  BoundOption, _BoundOptions
, foldBoundOption
, optionBound
, optionBounds
, optionEmptyBound
) where

import Data.Geo.OSM.Bound
import Data.Geo.OSM.Bounds
import Control.Lens.Iso


data BoundOption =
  OptionBound Bound
  | OptionBounds Bounds
  | Empty
  deriving Eq

_BoundOptions :: Iso' (Maybe (Either Bound Bounds)) BoundOption
_BoundOptions = iso toBoundOptions fromBoundOptions
  where
    toBoundOptions Nothing = optionEmptyBound
    toBoundOptions (Just (Left b)) = optionBound b
    toBoundOptions (Just (Right b)) = optionBounds b
    fromBoundOptions Empty = Nothing
    fromBoundOptions (OptionBound b) = Just (Left b)
    fromBoundOptions (OptionBounds b) = Just (Right b)


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
