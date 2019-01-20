-- | Values with a @generator@ optional string accessor.
module Data.Geo.OSM.Lens.AttributionL where

import Control.Lens.Lens

class AttributionL a where
  attributionL ::
    Lens' a (Maybe String)

