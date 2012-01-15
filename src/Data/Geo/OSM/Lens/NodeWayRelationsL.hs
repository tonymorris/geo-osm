-- | Values with a @nwrs@ accessor that is a list of @NodeWayRelation@.
module Data.Geo.OSM.Lens.NodeWayRelationsL where

import Data.Geo.OSM.NodeWayRelation
import Data.Geo.OSM.Node
import Data.Geo.OSM.Way
import Data.Geo.OSM.Relation
import Data.Lens.Common

class NodeWayRelationsL a where
  nwrsL ::
    Lens a [NodeWayRelation]

