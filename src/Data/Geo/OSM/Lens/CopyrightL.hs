-- | Values with a @generator@ optional string accessor.
module Data.Geo.OSM.Lens.CopyrightL where

import Control.Lens.Lens

class CopyrightL a where
  copyrightL ::
    Lens' a (Maybe String)

