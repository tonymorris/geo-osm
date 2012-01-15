-- | The @tag@ element of a OSM file.
module Data.Geo.OSM.Tag
(
  Tag
, tag
) where


import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Accessor.K
import Data.Geo.OSM.Accessor.V

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

instance K Tag where
  k (Tag x _) =
    x
  setK a (Tag _ b) =
    tag a b

instance V Tag where
  v (Tag _ x) =
    x
  setV b (Tag a _) =
    tag a b

-- | Constructs a tag with a key and value.
tag ::
  String -- ^ The key (@k@ attribute).
  -> String -- ^ The value (@v@ attribute).
  -> Tag
tag =
  Tag
