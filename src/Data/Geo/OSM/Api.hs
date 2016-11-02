{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

-- | The @api@ element of a OSM file.
module Data.Geo.OSM.Api
(
  Api
, api
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Version
import Data.Geo.OSM.Area
import Data.Geo.OSM.Tracepoints
import Data.Geo.OSM.Waynodes
import Control.Lens.TH

import Data.Geo.OSM.Lens.VersionL
import Data.Geo.OSM.Lens.AreaL
import Data.Geo.OSM.Lens.TracepointsL
import Data.Geo.OSM.Lens.WaynodesL

-- | The @api@ element of a OSM file.
data Api = Api {
  _apiVersion :: Version,
  _apiArea :: Area,
  _apiTracepoints :: Tracepoints,
  _apiWaynodes :: Waynodes
  } deriving Eq

makeLenses ''Api

-- | Constructs a @api@ with version, area, tracepoints and waynodes.
api ::
  Version -- ^ The @version@ element.
  -> Area -- ^ The @area@ element.
  -> Tracepoints -- ^ The @tracepoints@ element.
  -> Waynodes -- ^ The @waynodes@ element.
  -> Api
api =
  Api

instance XmlPickler Api where
  xpickle =
    xpElem "api" (xpWrap (\(version', area', tracepoints', waypoints') -> api version' area' tracepoints' waypoints', \(Api version' area' tracepoints' waynodes') -> (version', area', tracepoints', waynodes')) (xp4Tuple (xpElem "version" xpickle) (xpElem "area" xpickle) (xpElem "tracepoints" xpickle) (xpElem "waynodes" xpickle)))

instance Show Api where
  show =
    showPickled []

instance VersionL Api Version where
  versionL = apiVersion

instance AreaL Api where
  areaL = apiArea

instance TracepointsL Api where
  tracepointsL = apiTracepoints

instance WaynodesL Api where
  waynodesL = apiWaynodes
