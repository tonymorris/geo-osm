-- | Values with a @visible@ boolean accessor.
module Data.Geo.OSM.Accessor.Visible where

import Data.Geo.OSM.Accessor.Accessor

class Visible a where
  visible :: a -> Bool
  setVisible :: Bool -> a -> a

  makeVisible :: a -> a
  makeVisible = setVisible True

  makeInvisible :: a -> a
  makeInvisible = setVisible False

  usingVisible :: (Bool -> Bool) -> a -> a
  usingVisible = visible `using` setVisible
