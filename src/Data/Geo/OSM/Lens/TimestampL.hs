{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | Values with a @timestamp@ accessor.
module Data.Geo.OSM.Lens.TimestampL where

import Control.Lens.Lens

class TimestampL a b | a -> b where
  timestampL ::
    Lens' a b

