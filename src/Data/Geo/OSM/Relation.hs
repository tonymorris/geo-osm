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
import Data.Geo.OSM.Lens.IdL
import Data.Geo.OSM.Lens.TagsL
import Data.Geo.OSM.Lens.ChangesetL
import Data.Geo.OSM.Lens.VisibleL
import Data.Geo.OSM.Lens.UserL
import Data.Geo.OSM.Lens.UidL
import Data.Geo.OSM.Lens.TimestampL
import Data.Geo.OSM.Lens.MemberL
import Data.Lens.Common
import Control.Comonad.Trans.Store
import Control.Category
import Prelude hiding ((.))

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

instance MemberL Relation where
  memberL =
    Lens $ \(Relation members common) -> store (\members -> Relation members common) members

-- not exported
commonL ::
  Lens Relation NWRCommon
commonL =
  Lens (\(Relation members common) -> store (\common -> Relation members common) common)

instance IdL Relation where
  idL =
    idL . commonL

instance TagsL Relation where
  tagsL =
    tagsL . commonL

instance ChangesetL Relation where
  changesetL =
    changesetL . commonL

instance VisibleL Relation where
  visibleL = 
    visibleL . commonL

instance UserL Relation (Maybe String) where
  userL =
    userL . commonL

instance UidL Relation where
  uidL =
    uidL . commonL

instance TimestampL Relation (Maybe String) where
  timestampL =
    timestampL . commonL

-- | Constructs a relation with a list of members, id, list of tags, changeset, visible, user&uid and timestamp.
relation ::
  [Member] -- ^ The list of members (@member@ elements).
  -> String -- ^ The @id@ attribute.
  -> [Tag] -- ^ The list of tags (@tag@ elements).
  -> Maybe String -- ^ The @changeset@ attribute.
  -> Bool -- ^ The @visible@ attribute.
  -> (Maybe String, Maybe String) -- ^ The @user@ and @uid@ attributes.
  -> Maybe String -- ^ The @timestamp@ attribute.
  -> Maybe String -- ^ The @version@ attribute.
  -> Relation
relation =
  (. nwrCommon) . (.) . (.) . (.) . (.) . (.) . (.). Relation
