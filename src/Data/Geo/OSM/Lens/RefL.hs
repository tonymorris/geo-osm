-- | Values with a @ref@ string accessor.
module Data.Geo.OSM.Lens.RefL where

import Data.Lens.Common

class RefL a where
  refL ::
    Lens a String

