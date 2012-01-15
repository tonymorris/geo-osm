{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | Values with a @timestamp@ accessor.
module Data.Geo.OSM.Lens.TimestampL where

import Data.Lens.Common

class TimestampL a b | a -> b where
  timestampL ::
    Lens a b

