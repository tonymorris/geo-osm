{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TemplateHaskell #-}

-- | The @node@ element of a OSM file.
module Data.Geo.OSM.Node
(
  Node
, node
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.NWRCommon
import Data.Geo.OSM.Tag
import Data.Geo.OSM.Lens.LatL
import Data.Geo.OSM.Lens.LonL
import Data.Geo.OSM.Lens.IdL
import Data.Geo.OSM.Lens.TagsL
import Data.Geo.OSM.Lens.ChangesetL
import Data.Geo.OSM.Lens.VisibleL
import Data.Geo.OSM.Lens.UserL
import Data.Geo.OSM.Lens.UidL
import Data.Geo.OSM.Lens.TimestampL
import Control.Lens.TH

-- | The @node@ element of a OSM file.
data Node = Node {
  _nodeLat :: String,
  _nodeLon :: String,
  _nodeCommon :: NWRCommon
  } deriving Eq

makeLenses ''Node

instance HasNWRCommon Node where
  nWRCommon = nodeCommon

instance XmlPickler Node where
  xpickle =
    xpElem "node" (xpWrap (\(lat', lon', nwr') -> Node lat' lon' nwr', \(Node lat' lon' nwr') -> (lat', lon', nwr'))
                  (xpTriple (xpAttr "lat" xpText) (xpAttr "lon" xpText) xpickle))

instance Show Node where
  show =
    showPickled []

instance LatL Node where
  latL = nodeLat

instance LonL Node where
  lonL = nodeLon

instance IdL Node where
  idL = commonId

instance TagsL Node where
  tagsL = commonTags

instance ChangesetL Node where
  changesetL = commonChangeset

instance VisibleL Node where
  visibleL = commonVisible

instance UserL Node (Maybe String) where
  userL = nodeCommon . userL

instance UidL Node where
  uidL = nodeCommon . uidL

instance TimestampL Node (Maybe String) where
  timestampL = nodeCommon . timestampL

-- | Constructs a node with a lat, lon, id, list of tags, changeset, visible, user&uid and timestamp.
node ::
  String -- ^ The @lat@ attribute.
  -> String -- ^ The @lon@ attribute.
  -> String -- ^ The @id@ attribute.
  -> [Tag] -- ^ The list of tags (@tag@ elements).
  -> Maybe String -- ^ The @changeset@ attribute.
  -> Bool -- ^ The @visible@ attribute.
  -> (Maybe String, Maybe String) -- ^ The @user@ and @uid@ attributes.
  -> Maybe String -- ^ The @timestamp@ attribute.
  -> Maybe String -- ^ The @version@ attribute.
  -> Node
node =
  flip flip nwrCommon . (((.) . (.) . (.) . (.) . (.) . (.) . (.)) .) . Node
