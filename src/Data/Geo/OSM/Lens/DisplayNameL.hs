-- | Values with a @display_name@ string accessor.
module Data.Geo.OSM.Lens.DisplayNameL where

import Data.Lens.Common

class DisplayNameL a where
  displayNameL ::
    Lens a String

