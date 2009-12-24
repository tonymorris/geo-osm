-- | The @relation@ element of a OSM file.
module Geo.OSM.Relation(
                         Relation,
                         relation
                       ) where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Geo.OSM.Member
import Geo.OSM.NWRCommon
import Geo.OSM.Tag
import Geo.OSM.Accessor.Id
import Geo.OSM.Accessor.Tags
import Geo.OSM.Accessor.Changeset
import Geo.OSM.Accessor.Visible
import Geo.OSM.Accessor.User
import Geo.OSM.Accessor.Uid
import Geo.OSM.Accessor.Timestamp
import Geo.OSM.Accessor.Members
import Prelude hiding (id)

-- | The @relation@ element of a OSM file.
data Relation = Relation [Member] NWRCommon
  deriving Eq

instance XmlPickler Relation where
  xpickle = xpElem "relation" (xpWrap (uncurry Relation, \(Relation m r) -> (m, r))
                                (xpPair (xpList xpickle) xpickle))

instance Show Relation where
  show = showPickled []

instance Members Relation where
  members (Relation x _) = x

instance Id Relation where
  id (Relation _ x) = id x

instance Tags Relation where
  tags (Relation _ x) = tags x

instance Changeset Relation where
  changeset (Relation _ x) = changeset x

instance Visible Relation where
  visible (Relation _ x) = visible x

instance User Relation where
  user (Relation _ x) = user x

instance Uid Relation where
  uid (Relation _ x) = uid x

instance Timestamp Relation where
  timestamp (Relation _ x) = timestamp x

-- | Constructs a relation with a list of members, id, list of tags, changeset, visible, user&uid and timestamp.
relation :: [Member] -- ^ The list of members (@member@ elements).
            -> String -- ^ The @id@ attribute.
            -> [Tag] -- ^ The list of tags (@tag@ elements).
            -> Maybe String -- ^ The @changeset@ attribute.
            -> Bool -- ^ The @visible@ attribute.
            -> (Maybe String, Maybe String) -- ^ The @user@ and @uid@ attributes.
            -> Maybe String -- ^ The @timestamp@ attribute.
            -> Relation
relation = (. nwrCommon) . (.) . (.) . (.) . (.) . (.) . Relation
