{-# LANGUAGE TemplateHaskell #-}
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


import Control.Lens.TH

-- | The @relation@ element of a OSM file.
data Relation = Relation {
  _relationMembers :: [Member],
  _relationCommon :: NWRCommon
  }
  deriving Eq
makeLenses ''Relation

instance HasNWRCommon Relation where
  nWRCommon = relationCommon

instance XmlPickler Relation where
  xpickle =
    xpElem "relation" (xpWrap (uncurry Relation, \(Relation m r) -> (m, r))
                      (xpPair (xpList xpickle) xpickle))

instance Show Relation where
  show =
    showPickled []

instance MemberL Relation where
  memberL = relationMembers

instance IdL Relation where
  idL = commonId

instance TagsL Relation where
  tagsL = commonTags

instance ChangesetL Relation where
  changesetL = commonChangeset

instance VisibleL Relation where
  visibleL = commonVisible

instance UserL Relation (Maybe String) where
  userL = relationCommon . userL

instance UidL Relation where
  uidL = relationCommon . uidL

instance TimestampL Relation (Maybe String) where
  timestampL = relationCommon . timestampL



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
