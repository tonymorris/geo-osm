-- | The @user@ element of a OSM file.
module Data.Geo.OSM.User
(
  User
, user
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Home
import Data.Geo.OSM.Accessor.Hm
import Data.Geo.OSM.Accessor.DisplayName
import Data.Geo.OSM.Accessor.AccountCreated

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

instance Hm User where
  hm (User x _ _) =
    x
  setHm a (User _ b c) =
    user a b c

instance DisplayName User where
  displayName (User _ x _) =
    x
  setDisplayName b (User a _ c) =
    user a b c

instance AccountCreated User where
  accountCreated (User _ _ x) =
    x
  setAccountCreated c (User a b _) =
    user a b c
