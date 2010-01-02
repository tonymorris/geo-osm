-- | Values with a @lat@ string accessor.
module Data.Geo.OSM.Accessor.Lat where

class Lat a where
  lat :: a -> String
  setLat :: String -> a -> a
