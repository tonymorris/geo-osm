-- -- | Values with a @pending@ boolean accessor.
module Data.Geo.OSM.Accessor.Pending where

import Data.Geo.OSM.Accessor.Accessor

class Pending a where
  pending :: a -> Bool
  setPending :: Bool -> a -> a

  usingPending :: a -> (Bool -> Bool) -> a
  usingPending = pending `using` setPending
