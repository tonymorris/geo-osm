-- | The @waynodes@ element of a OSM file.
module Data.Geo.OSM.Waynodes
(
  Waynodes
, waynodes
) where


import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.MaximumL
import Data.Lens.Common
import Control.Comonad.Trans.Store

-- | The @waynodes@ element of a OSM file.
newtype Waynodes =
  Waynodes String
  deriving Eq

-- | Constructs a @waynodes@ with maximum.
waynodes ::
  String -- ^ The @maximum@ attribute.
  -> Waynodes
waynodes =
  Waynodes

instance XmlPickler Waynodes where
  xpickle =
    xpElem "waynodes" (xpWrap (waynodes, \(Waynodes r) -> r) (xpAttr "maximum" xpText))

instance Show Waynodes where
  show =
    showPickled []

instance MaximumL Waynodes where
  maximumL =
    Lens $ \(Waynodes maximum) -> store (\maximum -> Waynodes maximum) maximum
