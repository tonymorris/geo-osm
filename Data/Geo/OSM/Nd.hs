-- | The @nd@ element of a OSM file.
module Data.Geo.OSM.Nd(
                   Nd,
                   nd
                 ) where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.Accessor.Ref

-- | The @nd@ element of a OSM file.
newtype Nd = Nd String
  deriving Eq

instance XmlPickler Nd where
  xpickle = xpElem "nd" (xpWrap (Nd, \(Nd r) -> r) (xpAttr "ref" xpText))

instance Show Nd where
  show = showPickled []

instance Ref Nd where
  ref (Nd x) = x

-- | Constructs a nd with a ref.
nd :: String -- ^ The @ref@ attribute.
      -> Nd
nd = Nd
