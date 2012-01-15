{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | The @relation@ element of a OSM file.
module Data.Geo.OSM.Relation
(
  Relation
, relation
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Member
import Data.Geo.OSM.NWRCommon
import Data.Geo.OSM.Tag
import Data.Geo.OSM.Accessor.Id
import Data.Geo.OSM.Accessor.Tags
import Data.Geo.OSM.Accessor.Changeset
import Data.Geo.OSM.Accessor.Visible
import Data.Geo.OSM.Accessor.User
import Data.Geo.OSM.Accessor.Uid
import Data.Geo.OSM.Accessor.Timestamp
import Data.Geo.OSM.Accessor.Members
import Prelude hiding (id)

-- | The @relation@ element of a OSM file.
data Relation =
  Relation [Member] NWRCommon
  deriving Eq

instance XmlPickler Relation where
  xpickle =
    xpElem "relation" (xpWrap (uncurry Relation, \(Relation m r) -> (m, r))
                      (xpPair (xpList xpickle) xpickle))

instance Show Relation where
  show =
    showPickled []

instance Members Relation where
  members (Relation x _) =
    x
  setMembers a (Relation _ c) =
    Relation a c

instance Id Relation where
  id' (Relation _ x) =
    id' x
  setId c (Relation a cc) =
    Relation a (nwrCommon c (tags cc) (changeset cc) (visible cc) (user cc, uid cc) (timestamp cc))

instance Tags Relation where
  tags (Relation _ x) =
    tags x
  setTags c (Relation a cc) =
    Relation a (nwrCommon (id' cc) c (changeset cc) (visible cc) (user cc, uid cc) (timestamp cc))

instance Changeset Relation where
  changeset (Relation _ x) =
    changeset x
  setChangeset c (Relation a cc) =
    Relation a (nwrCommon (id' cc) (tags cc) c (visible cc) (user cc, uid cc) (timestamp cc))

instance Visible Relation where
  visible (Relation _ x) =
    visible x
  setVisible c (Relation a cc) =
    Relation a (nwrCommon (id' cc) (tags cc) (changeset cc) c (user cc, uid cc) (timestamp cc))

instance User Relation (Maybe String) where
  user (Relation _ x) =
    user x
  setUser c (Relation a cc) =
    Relation a (nwrCommon (id' cc) (tags cc) (changeset cc) (visible cc) (c, uid cc) (timestamp cc))

instance Uid Relation where
  uid (Relation _ x) =
    uid x
  setUid c (Relation a cc) =
    Relation a (nwrCommon (id' cc) (tags cc) (changeset cc) (visible cc) (user cc, c) (timestamp cc))

instance Timestamp Relation (Maybe String) where
  timestamp (Relation _ x) =
    timestamp x
  setTimestamp c (Relation a cc) =
    Relation a (nwrCommon (id' cc) (tags cc) (changeset cc) (visible cc) (user cc, uid cc) c)

-- | Constructs a relation with a list of members, id, list of tags, changeset, visible, user&uid and timestamp.
relation ::
  [Member] -- ^ The list of members (@member@ elements).
  -> String -- ^ The @id@ attribute.
  -> [Tag] -- ^ The list of tags (@tag@ elements).
  -> Maybe String -- ^ The @changeset@ attribute.
  -> Bool -- ^ The @visible@ attribute.
  -> (Maybe String, Maybe String) -- ^ The @user@ and @uid@ attributes.
  -> Maybe String -- ^ The @timestamp@ attribute.
  -> Relation
relation =
  (. nwrCommon) . (.) . (.) . (.) . (.) . (.) . Relation
