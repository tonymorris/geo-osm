-- | Values with a @maxlat@ string accessor.
module Data.Geo.OSM.Lens.MaxlatL where

import Data.Lens.Common

class MaxlatL a where
  maxlatL :: 
    Lens a String

