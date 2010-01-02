{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

-- | The @home@ element of a OSM file.
module Data.Geo.OSM.Api where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.VersionE
import Data.Geo.OSM.Area
import Data.Geo.OSM.Tracepoints
import Data.Geo.OSM.Waynodes
import Data.Geo.OSM.Accessor.Version
import Data.Geo.OSM.Accessor.Ar

-- | The @api@ element of a OSM file.
data Api = Api VersionE Area Tracepoints Waynodes
  deriving Eq

-- | Constructs a @version@ with tags.
api :: VersionE -> Area -> Tracepoints -> Waynodes -> Api
api = Api

instance XmlPickler Api where
  xpickle = xpElem "api" (xpWrap (\(version', area', tracepoints', waypoints') -> api version' area' tracepoints' waypoints', \(Api version' area' tracepoints' waynodes') -> (version', area', tracepoints', waynodes')) (xp4Tuple (xpElem "version" xpickle) (xpElem "area" xpickle) (xpElem "tracepoints" xpickle) (xpElem "waynodes" xpickle)))

instance Show Api where
  show = showPickled []

instance Version Api VersionE where
  version (Api x _ _ _) = x
  setVersion a (Api _ b c d) = api a b c d

instance Ar Api where
  ar (Api _ x _ _) = x
  setAr b (Api a _ c d) = api a b c d
