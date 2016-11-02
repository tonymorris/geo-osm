{-# LANGUAGE TemplateHaskell #-}
-- | The @bound@ element of a OSM file.
module Data.Geo.OSM.Bound
(
  Bound
, bound
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.BoxL
import Data.Geo.OSM.Lens.OriginL
import Control.Lens.TH

-- | The @bound@ element of a OSM file.
data Bound = Bound {
  _boundBox :: String,
  _boundOrigin :: Maybe String
  } deriving Eq
makeLenses ''Bound


instance XmlPickler Bound where
  xpickle =
    xpElem "bound" (xpWrap (uncurry bound, \(Bound b o) -> (b, o)) (xpPair (xpAttr "box" xpText) (xpOption (xpAttr "origin" xpText))))

instance Show Bound where
  show =
    showPickled []

instance BoxL Bound where
  boxL = boundBox

instance OriginL Bound where
  originL = boundOrigin

-- | Constructs a bound with a box and origin attributes.
bound ::
  String -- ^ The @box@ attribute.
  -> Maybe String -- ^ The @origin@ attribute.
  -> Bound
bound =
  Bound
