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
import Data.Lens.Common
import Control.Comonad.Trans.Store

-- | The @user@ element of a OSM file.
data User =
  User (Maybe Home) String String
  deriving Eq

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
  homeL =
    Lens $ \(User home displayName accountCreated) -> store (\home -> User home displayName accountCreated) home

instance DisplayNameL User where
  displayNameL =
    Lens $ \(User home displayName accountCreated) -> store (\displayName -> User home displayName accountCreated) displayName

instance AccountCreatedL User where
  accountCreatedL =
    Lens $ \(User home displayName accountCreated) -> store (\accountCreated -> User home displayName accountCreated) accountCreated

