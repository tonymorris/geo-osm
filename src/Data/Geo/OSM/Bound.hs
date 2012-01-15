-- | The @bound@ element of a OSM file.
module Data.Geo.OSM.Bound(
                      Bound,
                      bound
                    ) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Accessor.Box
import Data.Geo.OSM.Accessor.Origin

-- | The @bound@ element of a OSM file.
data Bound = Bound String (Maybe String)
  deriving Eq

instance XmlPickler Bound where
  xpickle = xpElem "bound" (xpWrap (uncurry bound, \(Bound b o) -> (b, o)) (xpPair (xpAttr "box" xpText) (xpOption (xpAttr "origin" xpText))))

instance Show Bound where
  show = showPickled []

instance Box Bound where
  box (Bound x _) = x
  setBox a (Bound _ b) = bound a b

instance Origin Bound where
  origin (Bound _ x) = x
  setOrigin b (Bound a _) = bound a b

-- | Constructs a bound with a box and origin attributes.
bound :: String -- ^ The @box@ attribute.
         -> Maybe String -- ^ The @origin@ attribute.
         -> Bound
bound = Bound
