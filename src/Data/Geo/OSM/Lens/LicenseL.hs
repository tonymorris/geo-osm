-- | Values with a @generator@ optional string accessor.
module Data.Geo.OSM.Lens.LicenseL where

import Control.Lens.Lens

class LicenseL a where
  licenseL ::
    Lens' a (Maybe String)

