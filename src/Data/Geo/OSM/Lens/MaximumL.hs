-- | Values with a @maximum@ string accessor.
module Data.Geo.OSM.Lens.MaximumL where

import Control.Lens.Lens

class MaximumL a where
  maximumL :: 
    Lens' a String

