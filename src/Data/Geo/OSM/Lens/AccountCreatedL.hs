-- | Values with a @account_created@ string accessor.
module Data.Geo.OSM.Lens.AccountCreatedL where

import Control.Lens.Lens

class AccountCreatedL a where
  accountCreatedL ::
    Lens' a String
