-- | Values with a @origin@ optional string accessor.
module Data.Geo.OSM.Lens.OriginL where

import Data.Lens.Common

class OriginL a where
  originL :: 
    Lens a (Maybe String)

