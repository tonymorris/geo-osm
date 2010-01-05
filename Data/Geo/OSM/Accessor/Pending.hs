-- -- | Values with a @pending@ boolean accessor.
module Data.Geo.OSM.Accessor.Pending where

import Data.Geo.OSM.Accessor.Accessor

class Pending a where
  pending :: a -> Bool
  setPending :: Bool -> a -> a

  usingPending :: (Bool -> Bool) -> a -> a
  usingPending = pending `using` setPending
