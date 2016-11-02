{-# LANGUAGE TemplateHaskell #-}
-- | The @user@ element of a OSM file.
module Data.Geo.OSM.User
(
  User
, user
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Home
import Data.Geo.OSM.Lens.HomeL
import Data.Geo.OSM.Lens.DisplayNameL
import Data.Geo.OSM.Lens.AccountCreatedL


import Control.Lens.TH

-- | The @user@ element of a OSM file.
data User = User {
  _userHome :: Maybe Home,
  _userDisplayName :: String,
  _userAccountCreated :: String
  } deriving Eq

makeLenses ''User

-- | Constructs a @user@ with an optional home, display_name and account_created.
user ::
  Maybe Home -- ^ The @home@ element.
  -> String -- ^ The @display_name@ attribute.
  -> String -- ^ The @account_created@ attribute.
  -> User
user =
  User

instance XmlPickler User where
  xpickle =
    xpElem "user" (xpWrap (\(home', dn', ac') -> user home' dn' ac', \(User home' dn' ac') -> (home', dn', ac')) (xpTriple (xpOption (xpElem "home" xpickle)) (xpAttr "display_name" xpText) (xpAttr "account_created" xpText)))

instance Show User where
  show =
    showPickled []

instance HomeL User where
  homeL = userHome

instance DisplayNameL User where
  displayNameL = userDisplayName

instance AccountCreatedL User where
  accountCreatedL = userAccountCreated
