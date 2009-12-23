-- | The @way@ element of a OSM file.
module Geo.OSM.Way(
                    Way,
                    way
                  ) where

import Text.XML.HXT.Arrow
import Geo.OSM.Nd
import Geo.OSM.NWRCommon
import Geo.OSM.Util
import Geo.OSM.Tag
import Geo.OSM.Accessor.Id
import Geo.OSM.Accessor.Tags
import Geo.OSM.Accessor.Changeset
import Geo.OSM.Accessor.Visible
import Geo.OSM.Accessor.User
import Geo.OSM.Accessor.Uid
import Geo.OSM.Accessor.Timestamp
import Geo.OSM.Accessor.Nds
import Prelude hiding (id)

-- | The @way@ element of a OSM file.
data Way = Way [Nd] NWRCommon
  deriving Eq

instance XmlPickler Way where
  xpickle = xpElem "way" (xpWrap (uncurry Way, \(Way n r) -> (n, r))
                           (xpPair (xpList xpickle) xpickle))

instance Show Way where
  show = showPickled []

instance Nds Way where
  nds (Way x _) = x

instance Id Way where
  id (Way _ x) = id x

instance Tags Way where
  tags (Way _ x) = tags x

instance Changeset Way where
  changeset (Way _ x) = changeset x

instance Visible Way where
  visible (Way _ x) = visible x

instance User Way where
  user (Way _ x) = user x

instance Uid Way where
  uid (Way _ x) = uid x

instance Timestamp Way where
  timestamp (Way _ x) = timestamp x

-- | Constructs a way with a list of nds, id, list of tags, changeset, visible, user&uid and timestamp.
way :: [Nd] -- ^ The list of nds (@nd@ elements).
       -> String -- ^ The @id@ attribute.
       -> [Tag] -- ^ The list of tags (@tag@ elements).
       -> Maybe String -- ^ The @changeset@ attribute.
       -> Bool -- ^ The @visible@ attribute.
       -> (Maybe String, Maybe String) -- ^ The @user@ and @uid@ attributes.
       -> Maybe String -- ^ The @timestamp@ attribute.
       -> Way
way = (. nwrCommon) . (.) . (.) . (.) . (.) . (.) . Way
