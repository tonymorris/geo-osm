{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | Values with a @version@ accessor.
module Data.Geo.OSM.Lens.VersionL where

import Control.Lens.Lens

class VersionL a b | a -> b where
  versionL ::
    Lens' a b

