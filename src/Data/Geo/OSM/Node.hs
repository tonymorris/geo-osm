{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

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
import Data.Lens.Common
import Control.Comonad.Trans.Store
import Control.Category
import Prelude hiding ((.))

-- | The @node@ element of a OSM file.
data Node =
  Node String String NWRCommon
  deriving Eq

instance XmlPickler Node where
  xpickle =
    xpElem "node" (xpWrap (\(lat', lon', nwr') -> Node lat' lon' nwr', \(Node lat' lon' nwr') -> (lat', lon', nwr'))
                  (xpTriple (xpAttr "lat" xpText) (xpAttr "lon" xpText) xpickle))

instance Show Node where
  show =
    showPickled []

instance LatL Node where
  latL =
    Lens $ \(Node lat lon common) -> store (\lat -> Node lat lon common) lat

instance LonL Node where
  lonL =
    Lens $ \(Node lat lon common) -> store (\lon -> Node lat lon common) lon

-- not exported
commonL ::
  Lens Node NWRCommon
commonL =
  Lens (\(Node lat lon common) -> store (\common -> Node lat lon common) common)

instance IdL Node where
  idL =
    idL . commonL

instance TagsL Node where
  tagsL =
    tagsL . commonL

instance ChangesetL Node where
  changesetL =
    changesetL . commonL

instance VisibleL Node where
  visibleL = 
    visibleL . commonL

instance UserL Node (Maybe String) where
  userL =
    userL . commonL

instance UidL Node where
  uidL =
    uidL . commonL

instance TimestampL Node (Maybe String) where
  timestampL =
    timestampL . commonL

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
