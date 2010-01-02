-- | The @home@ element of a OSM file.
module Data.Geo.OSM.Home where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.Accessor.Lat
import Data.Geo.OSM.Accessor.Lon
import Data.Geo.OSM.Accessor.Zoom

-- | The @version@ element of a OSM file.
data Home = Home String String String
  deriving Eq

-- | Constructs a @version@ with tags.
home :: String -> String -> String -> Home
home = Home

instance XmlPickler Home where
  xpickle = xpElem "home" (xpWrap (\(lat', lon', zoom') -> home lat' lon' zoom', \(Home lat' lon' zoom') -> (lat', lon', zoom')) (xpTriple (xpAttr "lat" xpText) (xpAttr "lon" xpText) (xpAttr "zoom" xpText)))

instance Show Home where
  show = showPickled []

instance Lat Home where
  lat (Home x _ _) = x
  setLat a (Home _ b c) = home a b c

instance Lon Home where
  lon (Home _ x _) = x
  setLon b (Home a _ c) = home a b c

instance Zoom Home where
  zoom (Home _ _ x) = x
  setZoom c (Home a b _) = home a b c
