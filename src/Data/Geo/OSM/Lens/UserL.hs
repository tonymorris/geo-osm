{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | Values with a @user@ accessor.
module Data.Geo.OSM.Lens.UserL where

import Control.Lens.Lens

class UserL a b | a -> b where
  userL ::
    Lens' a b

