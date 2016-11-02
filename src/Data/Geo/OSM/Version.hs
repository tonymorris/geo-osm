-- | The @version@ element of a OSM file.
module Data.Geo.OSM.Version
(
  Version
, version
) where


import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.MinimumL
import Data.Geo.OSM.Lens.MaximumL
import Control.Lens.Lens
import Control.Comonad.Trans.Store

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

instance MinimumL Version where
  minimumL =
    Lens $ \(Version minimum maximum) -> store (\minimum -> Version minimum maximum) minimum

instance MaximumL Version where
  maximumL =
    Lens $ \(Version minimum maximum) -> store (\maximum -> Version minimum maximum) maximum

