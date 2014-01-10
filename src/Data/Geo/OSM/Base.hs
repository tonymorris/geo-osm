module Data.Geo.OSM.Base
(
  Base
, unBase
) where

import Text.XML.HXT.Arrow.Pickle

-- | The @note@ element of a OSM file.
data Base = Base { unBase :: String }
  deriving Eq

instance XmlPickler Base where
  xpickle =
    xpElem "meta" (xpWrap (Base, unBase) (xpAttr "osm_base" xpText))

instance Show Base where
  show =
    showPickled []