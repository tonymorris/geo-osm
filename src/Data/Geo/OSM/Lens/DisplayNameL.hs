-- | Values with a @display_name@ string accessor.
module Data.Geo.OSM.Lens.DisplayNameL where

import Control.Lens.Lens

class DisplayNameL a where
  displayNameL ::
    Lens' a String

