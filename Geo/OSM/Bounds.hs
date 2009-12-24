-- | The @bounds@ element of a OSM file.
module Geo.OSM.Bounds(
                       Bounds,
                       bounds
                     ) where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Geo.OSM.Accessor.Minlat
import Geo.OSM.Accessor.Maxlat
import Geo.OSM.Accessor.Minlon
import Geo.OSM.Accessor.Maxlon
import Geo.OSM.Accessor.Origin

-- | The @bounds@ element of a OSM file.
data Bounds = Bounds String String String String (Maybe String)
  deriving Eq

instance XmlPickler Bounds where
  xpickle = xpElem "bounds" (xpWrap (\(minlat', minlon', maxlat', maxlon', origin') -> Bounds minlat' minlon' maxlat' maxlon' origin', \(Bounds minlat' minlon' maxlat' maxlon' origin') -> (minlat', minlon', maxlat', maxlon', origin'))
                              (xp5Tuple (xpAttr "minlat" xpText) (xpAttr "minlon" xpText) (xpAttr "maxlat" xpText) (xpAttr "maxlon" xpText) (xpOption (xpAttr "origin" xpText))))

instance Show Bounds where
  show = showPickled []

instance Minlat Bounds where
  minlat (Bounds x _ _ _ _) = x

instance Minlon Bounds where
  minlon (Bounds _ x _ _ _) = x

instance Maxlat Bounds where
  maxlat (Bounds _ _ x _ _) = x

instance Maxlon Bounds where
  maxlon (Bounds _ _ _ x _) = x

instance Origin Bounds where
  origin (Bounds _ _ _ _ x) = x

-- | Constructs a bounds with a minlat, minlon, maxlat, maxlon and origin attributes.
bounds :: String -- ^ The @minlat@ attribute.
          -> String -- ^ The @minlon@ attribute.
          -> String -- ^ The @maxlat@ attribute.
          -> String -- ^ The @maxlon@ attribute.
          -> Maybe String -- ^ The @origin@ attribute.
          -> Bounds
bounds = Bounds
