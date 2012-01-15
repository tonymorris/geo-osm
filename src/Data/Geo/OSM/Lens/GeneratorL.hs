-- | Values with a @generator@ optional string accessor.
module Data.Geo.OSM.Lens.GeneratorL where

import Data.Lens.Common

class GeneratorL a where
  generatorL ::
    Lens a (Maybe String)

