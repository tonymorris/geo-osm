-- | Values with a @box@ string accessor.
module Data.Geo.OSM.Accessor.Box where

import Data.Geo.OSM.Accessor.Accessor

class Box a where
  box :: a -> String
  setBox :: String -> a -> a

  usingBox :: (String -> String) -> a -> a
  usingBox = box `using` setBox
