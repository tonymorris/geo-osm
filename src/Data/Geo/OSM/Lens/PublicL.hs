-- -- | Values with a @public@ boolean accessor.
module Data.Geo.OSM.Lens.PublicL where

import Data.Lens.Common

class PublicL a where
  publicL ::
    Lens a Bool

