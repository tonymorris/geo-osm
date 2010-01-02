-- | The @waynode@ element of a OSM file.
module Data.Geo.OSM.Waynode where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.Accessor.Maximum

-- | The @waynode@ element of a OSM file.
newtype Waynode = Waynode String
  deriving Eq

-- | Constructs a @waynode@ with tags.
waynode :: String -> Waynode
waynode = Waynode

instance XmlPickler Waynode where
  xpickle = xpElem "waynode" (xpWrap (waynode, \(Waynode r) -> r) (xpAttr "maximum" xpText))

instance Show Waynode where
  show = showPickled []

instance Maximum Waynode where
  maximum (Waynode x) = x
  setMaximum a (Waynode _) = waynode a
