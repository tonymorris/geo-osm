{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TemplateHaskell #-}

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
import Data.Geo.OSM.Lens.NdL
import Data.Geo.OSM.Lens.IdL
import Data.Geo.OSM.Lens.TagsL
import Data.Geo.OSM.Lens.ChangesetL
import Data.Geo.OSM.Lens.VisibleL
import Data.Geo.OSM.Lens.UserL
import Data.Geo.OSM.Lens.UidL
import Data.Geo.OSM.Lens.TimestampL
import Control.Category
import Prelude hiding ((.))
import Control.Lens.TH

-- | The @way@ element of a OSM file.
data Way = Way {
  _wayNd :: [Nd],
  _wayCommon :: NWRCommon
  } deriving Eq

makeLenses ''Way

instance HasNWRCommon Way where
  nWRCommon = wayCommon

instance XmlPickler Way where
  xpickle =
    xpElem "way" (xpWrap (uncurry Way, \(Way n r) -> (n, r))
                 (xpPair (xpList xpickle) xpickle))

instance Show Way where
  show =
    showPickled []

instance NdL Way where
  ndL = wayNd

instance IdL Way where
  idL = commonId

instance TagsL Way where
  tagsL = commonTags

instance ChangesetL Way where
  changesetL = commonChangeset

instance VisibleL Way where
  visibleL = commonVisible

instance UserL Way (Maybe String) where
  userL = wayCommon . userL

instance UidL Way where
  uidL = wayCommon . uidL

instance TimestampL Way (Maybe String) where
  timestampL = wayCommon . timestampL

-- | Constructs a way with a list of nds, id, list of tags, changeset, visible, user&uid and timestamp.
way ::
  [Nd] -- ^ The list of nds (@nd@ elements).
  -> String -- ^ The @id@ attribute.
  -> [Tag] -- ^ The list of tags (@tag@ elements).
  -> Maybe String -- ^ The @changeset@ attribute.
  -> Bool -- ^ The @visible@ attribute.
  -> (Maybe String, Maybe String) -- ^ The @user@ and @uid@ attributes.
  -> Maybe String -- ^ The @timestamp@ attribute.
  -> Maybe String -- ^ The @version@ attribute.
  -> Way
way =
  (. nwrCommon) . (.) . (.) . (.) . (.) . (.) . (.) . Way
