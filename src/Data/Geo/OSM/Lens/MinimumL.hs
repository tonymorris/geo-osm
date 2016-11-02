-- | Values with a @minimum@ string accessor.
module Data.Geo.OSM.Lens.MinimumL where

import Control.Lens.Lens

class MinimumL a where
  minimumL :: 
    Lens' a String
