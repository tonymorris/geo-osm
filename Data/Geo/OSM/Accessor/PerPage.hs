-- | Values with a @per_page@ string accessor.
module Data.Geo.OSM.Accessor.PerPage where

import Data.Geo.OSM.Accessor.Accessor

class PerPage a where
  perPage :: a -> String
  setPerPage :: String -> a -> a

  usingPerPage :: a -> (String -> String) -> a
  usingPerPage = perPage `using` setPerPage
