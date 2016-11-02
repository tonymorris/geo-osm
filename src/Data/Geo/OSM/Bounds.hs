{-# LANGUAGE TemplateHaskell #-}
-- | The @bounds@ element of a OSM file.
module Data.Geo.OSM.Bounds
(
  Bounds
, bounds
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.MinlatL
import Data.Geo.OSM.Lens.MaxlatL
import Data.Geo.OSM.Lens.MinlonL
import Data.Geo.OSM.Lens.MaxlonL
import Data.Geo.OSM.Lens.OriginL
import Control.Lens.TH

-- | The @bounds@ element of a OSM file.
data Bounds = Bounds {
  _boundsMinLat :: String,
  _boundsMinLon :: String,
  _boundsMaxLat :: String,
  _boundsMaxLon :: String,
  _boundsOrigin :: Maybe String
  } deriving Eq

makeLenses ''Bounds

instance XmlPickler Bounds where
  xpickle =
    xpElem "bounds" (xpWrap (\(minlat', minlon', maxlat', maxlon', origin') -> bounds minlat' minlon' maxlat' maxlon' origin', \(Bounds minlat' minlon' maxlat' maxlon' origin') -> (minlat', minlon', maxlat', maxlon', origin'))
                              (xp5Tuple (xpAttr "minlat" xpText) (xpAttr "minlon" xpText) (xpAttr "maxlat" xpText) (xpAttr "maxlon" xpText) (xpOption (xpAttr "origin" xpText))))

instance Show Bounds where
  show =
    showPickled []

instance MinlatL Bounds where
  minlatL = boundsMinLat

instance MinlonL Bounds where
  minlonL = boundsMinLon

instance MaxlatL Bounds where
  maxlatL = boundsMaxLat

instance MaxlonL Bounds where
  maxlonL = boundsMaxLon

instance OriginL Bounds where
  originL = boundsOrigin

-- | Constructs a bounds with a minlat, minlon, maxlat, maxlon and origin attributes.
bounds ::
  String -- ^ The @minlat@ attribute.
  -> String -- ^ The @minlon@ attribute.
  -> String -- ^ The @maxlat@ attribute.
  -> String -- ^ The @maxlon@ attribute.
  -> Maybe String -- ^ The @origin@ attribute.
  -> Bounds
bounds =
  Bounds
