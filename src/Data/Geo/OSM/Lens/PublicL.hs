-- -- | Values with a @public@ boolean accessor.
module Data.Geo.OSM.Lens.PublicL where

import Control.Lens.Lens

class PublicL a where
  publicL ::
    Lens' a Bool

