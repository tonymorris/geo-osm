{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | The @way@ element of a OSM file.
module Data.Geo.OSM.Way
(
  Way
, way
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Nd
import Data.Geo.OSM.NWRCommon
import Data.Geo.OSM.Tag
import Data.Geo.OSM.Accessor.Id
import Data.Geo.OSM.Accessor.Tags
import Data.Geo.OSM.Accessor.Changeset
import Data.Geo.OSM.Accessor.Visible
import Data.Geo.OSM.Accessor.User
import Data.Geo.OSM.Accessor.Uid
import Data.Geo.OSM.Accessor.Timestamp
import Data.Geo.OSM.Accessor.Nds
import Prelude hiding (id)

-- | The @way@ element of a OSM file.
data Way =
  Way [Nd] NWRCommon
  deriving Eq

instance XmlPickler Way where
  xpickle =
    xpElem "way" (xpWrap (uncurry Way, \(Way n r) -> (n, r))
                 (xpPair (xpList xpickle) xpickle))

instance Show Way where
  show =
    showPickled []

instance Nds Way where
  nds (Way x _) =
    x
  setNds a (Way _ c) =
    Way a c

instance Id Way where
  id' (Way _ x) =
    id' x
  setId c (Way a cc) =
    Way a (nwrCommon c (tags cc) (changeset cc) (visible cc) (user cc, uid cc) (timestamp cc))

instance Tags Way where
  tags (Way _ x) =
    tags x
  setTags c (Way a cc) =
    Way a (nwrCommon (id' cc) c (changeset cc) (visible cc) (user cc, uid cc) (timestamp cc))

instance Changeset Way where
  changeset (Way _ x) =
    changeset x
  setChangeset c (Way a cc) =
    Way a (nwrCommon (id' cc) (tags cc) c (visible cc) (user cc, uid cc) (timestamp cc))

instance Visible Way where
  visible (Way _ x) =
    visible x
  setVisible c (Way a cc) =
    Way a (nwrCommon (id' cc) (tags cc) (changeset cc) c (user cc, uid cc) (timestamp cc))

instance User Way (Maybe String) where
  user (Way _ x) =
    user x
  setUser c (Way a cc) =
    Way a (nwrCommon (id' cc) (tags cc) (changeset cc) (visible cc) (c, uid cc) (timestamp cc))

instance Uid Way where
  uid (Way _ x) =
    uid x
  setUid c (Way a cc) =
    Way a (nwrCommon (id' cc) (tags cc) (changeset cc) (visible cc) (user cc, c) (timestamp cc))

instance Timestamp Way (Maybe String) where
  timestamp (Way _ x) =
    timestamp x
  setTimestamp c (Way a cc) =
    Way a (nwrCommon (id' cc) (tags cc) (changeset cc) (visible cc) (user cc, uid cc) c)

-- | Constructs a way with a list of nds, id, list of tags, changeset, visible, user&uid and timestamp.
way ::
  [Nd] -- ^ The list of nds (@nd@ elements).
  -> String -- ^ The @id@ attribute.
  -> [Tag] -- ^ The list of tags (@tag@ elements).
  -> Maybe String -- ^ The @changeset@ attribute.
  -> Bool -- ^ The @visible@ attribute.
  -> (Maybe String, Maybe String) -- ^ The @user@ and @uid@ attributes.
  -> Maybe String -- ^ The @timestamp@ attribute.
  -> Way
way =
  (. nwrCommon) . (.) . (.) . (.) . (.) . (.) . Way
