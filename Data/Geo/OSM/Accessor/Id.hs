-- | Values with a @id@ string accessor.
module Data.Geo.OSM.Accessor.Id where

import Data.Geo.OSM.Accessor.Accessor
import Prelude hiding (id)

class Id a where
  id :: a -> String
  setId :: String -> a -> a

  updateId :: a -> (String -> String) -> a
  updateId = id `using` setId
