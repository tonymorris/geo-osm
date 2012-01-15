-- | Values with a @minimum@ string accessor.
module Data.Geo.OSM.Lens.MinimumL where

import Data.Lens.Common

class MinimumL a where
  minimumL :: 
    Lens a String
