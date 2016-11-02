{-# LANGUAGE TemplateHaskell #-}
-- | The @tag@ element of a OSM file.
module Data.Geo.OSM.Tag
(
  Tag
, tag
) where


import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.KL
import Data.Geo.OSM.Lens.VL
import Control.Lens.TH


-- | The @tag@ element of a OSM file.
data Tag = Tag {
  _tagKey :: String,
  _tagValue :: String
  } deriving Eq

makeLenses ''Tag

instance XmlPickler Tag where
  xpickle =
    xpElem "tag" (xpWrap (uncurry tag, \(Tag k' v') -> (k', v')) (xpPair (xpAttr "k" xpText) (xpAttr "v" xpText)))

instance Show Tag where
  show =
    showPickled []

instance KL Tag where
  kL = tagKey

instance VL Tag where
  vL = tagValue


-- | Constructs a tag with a key and value.
tag ::
  String -- ^ The key (@k@ attribute).
  -> String -- ^ The value (@v@ attribute).
  -> Tag
tag =
  Tag
