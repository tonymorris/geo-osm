-- | Values with a @tracepoints@ string accessor.
module Data.Geo.OSM.Lens.TracepointsL where

import Data.Geo.OSM.Tracepoints
import Data.Lens.Common

class TracepointsL a where
  tracepointsL ::
    Lens a Tracepoints

