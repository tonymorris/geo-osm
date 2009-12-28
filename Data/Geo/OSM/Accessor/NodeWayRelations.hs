-- | Values with a @nwrs@ accessor that is a list of @NodeWayRelation@.
module Data.Geo.OSM.Accessor.NodeWayRelations where

import Data.Geo.OSM.NodeWayRelation
import Data.Geo.OSM.Node
import Data.Geo.OSM.Way
import Data.Geo.OSM.Relation

class NodeWayRelations a where
  nwrs :: a -> [NodeWayRelation]

nodes :: (NodeWayRelations a) => a -> [Node]
nodes k = nwrs k >>= \t -> foldNodeWayRelation t return (const []) (const [])

ways :: (NodeWayRelations a) => a -> [Way]
ways k = nwrs k >>= \t -> foldNodeWayRelation t (const []) return (const [])

relations :: (NodeWayRelations a) => a -> [Relation]
relations k = nwrs k >>= \t -> foldNodeWayRelation t (const []) (const []) return
