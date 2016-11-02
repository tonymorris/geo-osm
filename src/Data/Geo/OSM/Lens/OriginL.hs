-- | Values with a @origin@ optional string accessor.
module Data.Geo.OSM.Lens.OriginL where

import Control.Lens.Lens

class OriginL a where
  originL :: 
    Lens' a (Maybe String)

