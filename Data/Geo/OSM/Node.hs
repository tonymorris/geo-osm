-- | The @node@ element of a OSM file.
module Data.Geo.OSM.Node(
                     Node,
                     node
                   ) where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.NWRCommon
import Data.Geo.OSM.Tag
import Data.Geo.OSM.Accessor.Id
import Data.Geo.OSM.Accessor.Tags
import Data.Geo.OSM.Accessor.Changeset
import Data.Geo.OSM.Accessor.Visible
import Data.Geo.OSM.Accessor.User
import Data.Geo.OSM.Accessor.Uid
import Data.Geo.OSM.Accessor.Timestamp
import Data.Geo.OSM.Accessor.Lat
import Data.Geo.OSM.Accessor.Lon
import Prelude hiding (id)

-- | The @node@ element of a OSM file.
data Node = Node String String NWRCommon
  deriving Eq

instance XmlPickler Node where
  xpickle = xpElem "node" (xpWrap (\(lat', lon', nwr') -> Node lat' lon' nwr', \(Node lat' lon' nwr') -> (lat', lon', nwr'))
                             (xpTriple (xpAttr "lat" xpText) (xpAttr "lon" xpText) xpickle))

instance Show Node where
  show = showPickled []

instance Lat Node where
  lat (Node x _ _) = x

instance Lon Node where
  lon (Node _ x _) = x

instance Id Node where
  id (Node _ _ x) = id x

instance Tags Node where
  tags (Node _ _ x) = tags x

instance Changeset Node where
  changeset (Node _ _ x) = changeset x

instance Visible Node where
  visible (Node _ _ x) = visible x

instance User Node where
  user (Node _ _ x) = user x

instance Uid Node where
  uid (Node _ _ x) = uid x

instance Timestamp Node where
  timestamp (Node _ _ x) = timestamp x

-- | Constructs a node with a lat, lon, id, list of tags, changeset, visible, user&uid and timestamp.
node :: String -- ^ The @lat@ attribute.
        -> String -- ^ The @lon@ attribute.
        -> String -- ^ The @id@ attribute.
        -> [Tag] -- ^ The list of tags (@tag@ elements).
        -> Maybe String -- ^ The @changeset@ attribute.
        -> Bool -- ^ The @visible@ attribute.
        -> (Maybe String, Maybe String) -- ^ The @user@ and @uid@ attributes.
        -> Maybe String -- ^ The @timestamp@ attribute.
        -> Node
node = flip flip nwrCommon . (((.) . (.) . (.) . (.) . (.) . (.)) .) . Node
