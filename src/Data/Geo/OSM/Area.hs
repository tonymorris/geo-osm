{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

-- | The @area@ element of a OSM file.
module Data.Geo.OSM.Area
(
  Area
, area
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.MaximumL
import Control.Lens.Lens

import Control.Newtype

-- | The @area@ element of a OSM file.
newtype Area =
  Area String
  deriving Eq

-- | Constructs a @area@ with maximum.
area ::
  String -- ^ The @area@ element.
  -> Area
area =
  Area

instance XmlPickler Area where
  xpickle =
    xpElem "area" (xpWrap (area, \(Area r) -> r) (xpAttr "maximum" xpText))

instance Show Area where
  show =
    showPickled []

instance MaximumL Area where
  maximumL =
    lens unpack (const pack)

instance Newtype Area String where
  pack =
    Area
  unpack (Area x) =
    x
