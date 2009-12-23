-- | The @bound@ element of a OSM file.
module Geo.OSM.Bound(
                      Bound,
                      bound
                    ) where

import Text.XML.HXT.Arrow
import Geo.OSM.Util
import Geo.OSM.Accessor.Box
import Geo.OSM.Accessor.Origin

-- | The @bound@ element of a OSM file.
data Bound = Bound String (Maybe String)
  deriving Eq

instance XmlPickler Bound where
  xpickle = xpElem "bound" (xpWrap (uncurry Bound, \(Bound b o) -> (b, o)) (xpPair (xpAttr "box" xpText) (xpOption (xpAttr "origin" xpText))))

instance Show Bound where
  show = showPickled []

instance Box Bound where
  box (Bound x _) = x

instance Origin Bound where
  origin (Bound _ x) = x

-- | Constructs a bound with a box and origin attributes.
bound :: String -- ^ The @box@ attribute.
         -> Maybe String -- ^ The @origin@ attribute.
         -> Bound
bound = Bound
