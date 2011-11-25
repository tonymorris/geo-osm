-- | The @bounds@ element of a OSM file.
module Data.Geo.OSM.Bounds(
                       Bounds,
                       bounds
                     ) where

import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Extras
import Data.Geo.OSM.Accessor.Minlat
import Data.Geo.OSM.Accessor.Maxlat
import Data.Geo.OSM.Accessor.Minlon
import Data.Geo.OSM.Accessor.Maxlon
import Data.Geo.OSM.Accessor.Origin

-- | The @bounds@ element of a OSM file.
data Bounds = Bounds String String String String (Maybe String)
  deriving Eq

instance XmlPickler Bounds where
  xpickle = xpElem "bounds" (xpWrap (\(minlat', minlon', maxlat', maxlon', origin') -> bounds minlat' minlon' maxlat' maxlon' origin', \(Bounds minlat' minlon' maxlat' maxlon' origin') -> (minlat', minlon', maxlat', maxlon', origin'))
                              (xp5Tuple (xpAttr "minlat" xpText) (xpAttr "minlon" xpText) (xpAttr "maxlat" xpText) (xpAttr "maxlon" xpText) (xpOption (xpAttr "origin" xpText))))

instance Show Bounds where
  show = showPickled []

instance Minlat Bounds where
  minlat (Bounds x _ _ _ _) = x
  setMinlat a (Bounds _ b c d e) = bounds a b c d e

instance Minlon Bounds where
  minlon (Bounds _ x _ _ _) = x
  setMinlon b (Bounds a _ c d e) = bounds a b c d e

instance Maxlat Bounds where
  maxlat (Bounds _ _ x _ _) = x
  setMaxlat c (Bounds a b _ d e) = bounds a b c d e

instance Maxlon Bounds where
  maxlon (Bounds _ _ _ x _) = x
  setMaxlon d (Bounds a b c _ e) = bounds a b c d e

instance Origin Bounds where
  origin (Bounds _ _ _ _ x) = x
  setOrigin e (Bounds a b c d _) = bounds a b c d e

-- | Constructs a bounds with a minlat, minlon, maxlat, maxlon and origin attributes.
bounds :: String -- ^ The @minlat@ attribute.
          -> String -- ^ The @minlon@ attribute.
          -> String -- ^ The @maxlat@ attribute.
          -> String -- ^ The @maxlon@ attribute.
          -> Maybe String -- ^ The @origin@ attribute.
          -> Bounds
bounds = Bounds
