-- | The @user@ element of a OSM file.
module Data.Geo.OSM.UserE
(
  UserE
, userE
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Home
import Data.Geo.OSM.Accessor.Hm
import Data.Geo.OSM.Accessor.DisplayName
import Data.Geo.OSM.Accessor.AccountCreated

-- | The @user@ element of a OSM file.
data UserE =
  UserE (Maybe Home) String String
  deriving Eq

-- | Constructs a @user@ with an optional home, display_name and account_created.
userE ::
  Maybe Home -- ^ The @home@ element.
  -> String -- ^ The @display_name@ attribute.
  -> String -- ^ The @account_created@ attribute.
  -> UserE
userE =
  UserE

instance XmlPickler UserE where
  xpickle =
    xpElem "user" (xpWrap (\(home', dn', ac') -> userE home' dn' ac', \(UserE home' dn' ac') -> (home', dn', ac')) (xpTriple (xpOption (xpElem "home" xpickle)) (xpAttr "display_name" xpText) (xpAttr "account_created" xpText)))

instance Show UserE where
  show =
    showPickled []

instance Hm UserE where
  hm (UserE x _ _) =
    x
  setHm a (UserE _ b c) =
    userE a b c

instance DisplayName UserE where
  displayName (UserE _ x _) =
    x
  setDisplayName b (UserE a _ c) =
    userE a b c

instance AccountCreated UserE where
  accountCreated (UserE _ _ x) =
    x
  setAccountCreated c (UserE a b _) =
    userE a b c
