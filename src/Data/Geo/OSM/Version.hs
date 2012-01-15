-- | The @version@ element of a OSM file.
module Data.Geo.OSM.Version
(
  Version
, version
) where


import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Accessor.Minimum
import Data.Geo.OSM.Accessor.Maximum

-- | The @version@ element of a OSM file.
data Version =
  Version String String
  deriving Eq

-- | Constructs a @version@ with minimum and maximum.
version ::
  String -- ^ The @minimum@ attribute.
  -> String -- ^ The @maximum@ attribute.
  -> Version
version =
  Version

instance XmlPickler Version where
  xpickle =
    xpElem "version" (xpWrap (uncurry version, \(Version min' max') -> (min', max')) (xpPair (xpAttr "minimum" xpText) (xpAttr "maximum" xpText)))

instance Show Version where
  show =
    showPickled []

instance Minimum Version where
  minimum (Version x _) =
    x
  setMinimum a (Version _ b) =
    version a b

instance Maximum Version where
  maximum (Version _ x) =
    x
  setMaximum b (Version a _) =
    version a b
