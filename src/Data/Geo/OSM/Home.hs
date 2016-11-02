{-# LANGUAGE TemplateHaskell #-}
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


import Control.Lens.TH

-- | The @home@ element of a OSM file.
data Home = Home {
  _homeLat :: String,
  _homeLon :: String,
  _homeZoom :: String
  } deriving Eq
makeLenses ''Home

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
  latL = homeLat

instance LonL Home where
  lonL = homeLon

instance ZoomL Home where
  zoomL = homeZoom
