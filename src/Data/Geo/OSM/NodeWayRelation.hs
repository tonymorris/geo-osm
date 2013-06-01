-- | The @node@, @way@, or @relation@ element of a OSM file.
module Data.Geo.OSM.NodeWayRelation
(
  NodeWayRelation
, foldNodeWayRelation
, asNode
, asWay
, asRelation
, nodes
, ways
, relations
, node'
, way'
, relation'
, isNode
, isWay
, isRelation
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Node
import Data.Geo.OSM.Way
import Data.Geo.OSM.Relation
import Data.Maybe

-- | The @node@, @way@, or @relation@ element of a OSM file.
data NodeWayRelation =
  N Node
  | W Way
  | R Relation
  deriving Eq

-- | Folds a node-way-relation (catamorphism).
foldNodeWayRelation ::
  NodeWayRelation -- ^ The @node@, @way@ or @relation@ element.
  -> (Node -> x) -- ^ If this is a @node@ element.
  -> (Way -> x) -- ^ If this is a @way@ element.
  -> (Relation -> x) -- ^ If this is a @relation@ element.
  -> x
foldNodeWayRelation (N n) x _ _ =
  x n
foldNodeWayRelation (W w) _ x _ =
  x w
foldNodeWayRelation (R r) _ _ x =
  x r

instance XmlPickler NodeWayRelation where
  xpickle =
    xpAlt (\r -> case r of
                   N _ -> 0
                   W _ -> 1
                   R _ -> 2)
                     [xpWrap (N, \(N n) -> n) xpickle, xpWrap (W, \(W w) -> w) xpickle, xpWrap (R, \(R r) -> r) xpickle]

instance Show NodeWayRelation where
  show =
    showPickled []

asNode ::
  NodeWayRelation
  -> Maybe Node
asNode (N n) =
  Just n
asNode (W _) =
  Nothing
asNode (R _) =
  Nothing

asWay ::
  NodeWayRelation
  -> Maybe Way
asWay (N _) =
  Nothing
asWay (W w) =
  Just w
asWay (R _) =
  Nothing

asRelation ::
  NodeWayRelation
  -> Maybe Relation
asRelation (N _) =
  Nothing
asRelation (W _) =
  Nothing
asRelation (R r) =
  Just r

nodes ::
  [NodeWayRelation]
  -> [Node]
nodes =
  mapMaybe asNode

ways ::
  [NodeWayRelation]
  -> [Way]
ways =
  mapMaybe asWay

relations ::
  [NodeWayRelation]
  -> [Relation]
relations =
  mapMaybe asRelation

-- | Construct a @node@ element value.
node' ::
  Node
  -> NodeWayRelation
node' =
  N

-- | Construct a @way@ element value.
way' ::
  Way
  -> NodeWayRelation
way' =
  W

-- | Construct a @relation@ element value.
relation' ::
  Relation
  -> NodeWayRelation
relation' =
  R

-- | Returns whether the @node@, @way@ or @relation@ element is a node.
isNode ::
  NodeWayRelation
  -> Bool
isNode (N _) =
  True
isNode _ =
  False

-- | Returns whether the @node@, @way@ or @relation@ element is a way.
isWay ::
  NodeWayRelation
  -> Bool
isWay (W _) =
  True
isWay _ =
  False

-- | Returns whether the @node@, @way@ or @relation@ element is a relation.
isRelation ::
  NodeWayRelation
  -> Bool
isRelation (R _) =
  True
isRelation _ =
  False
