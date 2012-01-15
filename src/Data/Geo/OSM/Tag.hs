-- | The @tag@ element of a OSM file.
module Data.Geo.OSM.Tag
(
  Tag
, tag
) where


import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.KL
import Data.Geo.OSM.Lens.VL
import Data.Lens.Common
import Control.Comonad.Trans.Store

-- | The @tag@ element of a OSM file.
data Tag =
  Tag String String
  deriving Eq

instance XmlPickler Tag where
  xpickle =
    xpElem "tag" (xpWrap (uncurry tag, \(Tag k' v') -> (k', v')) (xpPair (xpAttr "k" xpText) (xpAttr "v" xpText)))

instance Show Tag where
  show =
    showPickled []

instance KL Tag where
  kL =
    Lens $ \(Tag k v) -> store (\k -> Tag k v) k

instance VL Tag where
  vL =
    Lens $ \(Tag k v) -> store (\v -> Tag k v) v

-- | Constructs a tag with a key and value.
tag ::
  String -- ^ The key (@k@ attribute).
  -> String -- ^ The value (@v@ attribute).
  -> Tag
tag =
  Tag
