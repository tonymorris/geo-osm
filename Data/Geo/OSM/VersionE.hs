-- | The @version@ element of a OSM file.
module Data.Geo.OSM.VersionE where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.Accessor.Minimum
import Data.Geo.OSM.Accessor.Maximum

-- | The @version@ element of a OSM file.
data VersionE = VersionE String String
  deriving Eq

-- | Constructs a @version@ with minimum and maximum.
versionE :: String -> String -> VersionE
versionE = VersionE

instance XmlPickler VersionE where
  xpickle = xpElem "version" (xpWrap (uncurry versionE, \(VersionE min' max') -> (min', max')) (xpPair (xpAttr "minimum" xpText) (xpAttr "maximum" xpText)))

instance Show VersionE where
  show = showPickled []

instance Minimum VersionE where
  minimum (VersionE x _) = x
  setMinimum a (VersionE _ b) = versionE a b

instance Maximum VersionE where
  maximum (VersionE _ x) = x
  setMaximum b (VersionE a _) = versionE a b
