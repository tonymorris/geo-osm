-- | Values with node, way or relation children accessor.
module Data.Geo.OSM.Lens.ChildrenL where

import Data.Geo.OSM.Children
import Control.Lens.Lens

class ChildrenL a where
  childrenL ::
    Lens' a Children

