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
import Data.Geo.OSM.Lens.NdL
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

instance NdL Way where
  ndL =
    Lens $ \(Way nds common) -> store (\nds -> Way nds common) nds

-- not exported
commonL ::
  Lens Way NWRCommon
commonL =
  Lens (\(Way nds common) -> store (\common -> Way nds common) common)

instance IdL Way where
  idL =
    idL . commonL

instance TagsL Way where
  tagsL =
    tagsL . commonL

instance ChangesetL Way where
  changesetL =
    changesetL . commonL

instance VisibleL Way where
  visibleL = 
    visibleL . commonL

instance UserL Way (Maybe String) where
  userL =
    userL . commonL

instance UidL Way where
  uidL =
    uidL . commonL

instance TimestampL Way (Maybe String) where
  timestampL =
    timestampL . commonL

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
