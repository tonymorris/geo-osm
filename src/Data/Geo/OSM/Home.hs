-- | The @home@ element of a OSM file.
module Data.Geo.OSM.Home
(
  Home
, home
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.LatL
import Data.Geo.OSM.Lens.LonL
import Data.Geo.OSM.Lens.ZoomL
import Control.Lens.Lens
import Control.Comonad.Trans.Store

-- | The @home@ element of a OSM file.
data Home =
  Home String String String
  deriving Eq

-- | Constructs a @home@ with lat, lon and zoom.
home ::
  String -- ^ The @lat@ attribute.
  -> String -- ^ The @lon@ attribute.
  -> String -- ^ The @zoom@ attribute.
  -> Home
home =
  Home

instance XmlPickler Home where
  xpickle =
    xpElem "home" (xpWrap (\(lat', lon', zoom') -> home lat' lon' zoom', \(Home lat' lon' zoom') -> (lat', lon', zoom')) (xpTriple (xpAttr "lat" xpText) (xpAttr "lon" xpText) (xpAttr "zoom" xpText)))

instance Show Home where
  show =
    showPickled []

instance LatL Home where
  latL =
    Lens $ \(Home lat lon zoom) -> store (\lat -> Home lat lon zoom) lat

instance LonL Home where
  lonL =
    Lens $ \(Home lat lon zoom) -> store (\lon -> Home lat lon zoom) lon

instance ZoomL Home where
  zoomL =
    Lens $ \(Home lat lon zoom) -> store (\zoom -> Home lat lon zoom) zoom

