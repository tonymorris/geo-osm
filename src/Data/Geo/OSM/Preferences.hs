-- | The @preferences@ element of a OSM file.
module Data.Geo.OSM.Preferences
(
  Preferences
, preferences
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Tag
import Data.Geo.OSM.Accessor.Tags

-- | The @preferences@ element of a OSM file.
newtype Preferences =
  Preferences [Tag]
  deriving Eq

-- | Constructs a @preferences@ with tags.
preferences ::
  [Tag] -- ^ The list of tags (@tag@ elements).
  -> Preferences
preferences =
  Preferences

instance XmlPickler Preferences where
  xpickle =
    xpElem "preferences" (xpWrap (preferences, \(Preferences r) -> r) (xpList xpickle))

instance Show Preferences where
  show =
    showPickled []

instance Tags Preferences where
  tags (Preferences x) =
    x
  setTags a (Preferences _) =
    preferences a
