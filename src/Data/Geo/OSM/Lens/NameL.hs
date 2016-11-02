-- -- | Values with a @name@ string accessor.
module Data.Geo.OSM.Lens.NameL where

import Control.Lens.Lens

class NameL a where
  nameL ::
    Lens' a String

