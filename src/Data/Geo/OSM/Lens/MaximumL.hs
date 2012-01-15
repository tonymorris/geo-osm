-- | Values with a @maximum@ string accessor.
module Data.Geo.OSM.Lens.MaximumL where

import Data.Lens.Common

class MaximumL a where
  maximumL :: 
    Lens a String

