-- | The @tracepoints@ element of a OSM file.
module Data.Geo.OSM.Tracepoints where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.Accessor.PerPage

-- | The @tracepoints@ element of a OSM file.
newtype Tracepoints = Tracepoints String
  deriving Eq

-- | Constructs a @tracepoints@ with tags.
tracepoints :: String -> Tracepoints
tracepoints = Tracepoints

instance XmlPickler Tracepoints where
  xpickle = xpElem "tracepoints" (xpWrap (tracepoints, \(Tracepoints r) -> r) (xpAttr "per_page" xpText))

instance Show Tracepoints where
  show = showPickled []

instance PerPage Tracepoints where
  perPage (Tracepoints x) = x
  setPerPage a (Tracepoints _) = tracepoints a
