-- | The @tag@ element of a OSM file.
module Data.Geo.OSM.Tag(
                    Tag,
                    tag
                  ) where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.Accessor.K
import Data.Geo.OSM.Accessor.V

-- | The @tag@ element of a OSM file.
data Tag = Tag String String
  deriving Eq

instance XmlPickler Tag where
  xpickle = xpElem "tag" (xpWrap (uncurry Tag, \(Tag k' v') -> (k', v')) (xpPair (xpAttr "k" xpText) (xpAttr "v" xpText)))

instance Show Tag where
  show = showPickled []

instance K Tag where
  k (Tag x _) = x

instance V Tag where
  v (Tag _ x) = x

-- | Constructs a tag with a key and value.
tag :: String -- ^ The key (@k@ attribute).
       -> String -- ^ The value (@v@ attribute).
       -> Tag
tag = Tag
