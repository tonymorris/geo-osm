-- | Values with a @generator@ optional string accessor.
module Data.Geo.OSM.Lens.GeneratorL where

import Control.Lens.Lens

class GeneratorL a where
  generatorL ::
    Lens' a (Maybe String)

