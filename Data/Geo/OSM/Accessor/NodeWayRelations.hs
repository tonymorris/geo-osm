-- | Values with a @nwrs@ accessor that is a list of @NodeWayRelation@.
module Data.Geo.OSM.Accessor.NodeWayRelations where

import Data.Geo.OSM.NodeWayRelation
import Data.Geo.OSM.Node
import Data.Geo.OSM.Way
import Data.Geo.OSM.Relation
import Data.Geo.OSM.Accessor.Accessor

class NodeWayRelations a where
  nwrs :: a -> [NodeWayRelation]
  setNwrs :: [NodeWayRelation] -> a -> a

  setNwr :: NodeWayRelation -> a -> a
  setNwr = setNwrs . return

  usingNwrs :: ([NodeWayRelation] -> [NodeWayRelation]) -> a -> a
  usingNwrs = nwrs `using` setNwrs

  usingNwr :: (NodeWayRelation -> NodeWayRelation) -> a -> a
  usingNwr = usingNwrs . map

  usingNode :: (Node -> Node) -> a -> a
  usingNode f = usingNwr (\p -> foldNodeWayRelation p (node' . f) way' relation')

  usingWay :: (Way -> Way) -> a -> a
  usingWay f = usingNwr (\p -> foldNodeWayRelation p node' (way' . f) relation')

  usingRelation :: (Relation -> Relation) -> a -> a
  usingRelation f = usingNwr (\p -> foldNodeWayRelation p node' way' (relation' . f))

nodes :: (NodeWayRelations a) => a -> [Node]
nodes k = nwrs k >>= \t -> foldNodeWayRelation t return (const []) (const [])

ways :: (NodeWayRelations a) => a -> [Way]
ways k = nwrs k >>= \t -> foldNodeWayRelation t (const []) return (const [])

relations :: (NodeWayRelations a) => a -> [Relation]
relations k = nwrs k >>= \t -> foldNodeWayRelation t (const []) (const []) return
