-- | The @waynodes@ element of a OSM file.
module Data.Geo.OSM.Waynodes(
                              Waynodes,
                              waynodes
                            ) where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.Accessor.Maximum

-- | The @waynodes@ element of a OSM file.
newtype Waynodes = Waynodes String
  deriving Eq

-- | Constructs a @waynodes@ with maximum.
waynodes :: String -> Waynodes
waynodes = Waynodes

instance XmlPickler Waynodes where
  xpickle = xpElem "waynodes" (xpWrap (waynodes, \(Waynodes r) -> r) (xpAttr "maximum" xpText))

instance Show Waynodes where
  show = showPickled []

instance Maximum Waynodes where
  maximum (Waynodes x) = x
  setMaximum a (Waynodes _) = waynodes a
