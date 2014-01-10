-- | The @version@ element of a OSM file.
module Data.Geo.OSM.Version
(
  Version
, verMinimum
, verMaximum
) where


import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.MinimumL
import Data.Geo.OSM.Lens.MaximumL
import Data.Lens.Common
import Control.Comonad.Trans.Store
import Prelude hiding (minimum, maximum)

-- | The @version@ element of a OSM file.
data Version = Version { 
  verMinimum :: String -- ^ The @minimum@ attribute.
  , verMaximum :: String -- ^ The @maximum@ attribute.
} deriving Eq

instance XmlPickler Version where
  xpickle =
    xpElem "version" (xpWrap (uncurry Version, \(Version min' max') -> (min', max')) (xpPair (xpAttr "minimum" xpText) (xpAttr "maximum" xpText)))

instance Show Version where
  show =
    showPickled []

instance MinimumL Version where
  minimumL =
    Lens $ \(Version minimum maximum) -> store (\minimum' -> Version minimum' maximum) minimum

instance MaximumL Version where
  maximumL =
    Lens $ \(Version minimum maximum) -> store (\maximum' -> Version minimum maximum') maximum

