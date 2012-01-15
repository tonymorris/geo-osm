-- -- | Values with a @name@ string accessor.
module Data.Geo.OSM.Lens.NameL where

import Data.Lens.Common

class NameL a where
  nameL ::
    Lens a String

