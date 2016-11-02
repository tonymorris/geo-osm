{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

-- | The @waynodes@ element of a OSM file.
module Data.Geo.OSM.Waynodes
(
  Waynodes
, waynodes
) where


import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.MaximumL
import Control.Lens.Lens
import Control.Newtype

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
    lens unpack (const pack)

instance Newtype Waynodes String where
  pack =
    Waynodes
  unpack (Waynodes x) =
    x
