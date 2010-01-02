-- | Values with a @account_created@ string accessor.
module Data.Geo.OSM.Accessor.AccountCreated where

import Data.Geo.OSM.Accessor.Accessor

class AccountCreated a where
  accountCreated :: a -> String
  setAccountCreated :: String -> a -> a

  usingAccountCreated :: a -> (String -> String) -> a
  usingAccountCreated = accountCreated `using` setAccountCreated
