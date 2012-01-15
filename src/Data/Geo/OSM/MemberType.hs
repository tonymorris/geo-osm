-- | The @type@ attribute of a @member@ element of a OSM file.
module Data.Geo.OSM.MemberType
(
  MemberType
, foldMemberType
, wayType
, nodeType
, relationType
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Char

-- | The @type@ attribute of a @member@ element of a OSM file.
data MemberType =
  WayType
  | NodeType
  | RelationType
  deriving Eq

-- | Folds a member-type (catamorphism).
foldMemberType ::
  MemberType -- ^ The member-type to fold.
  -> x -- ^ If the type is a way.
  -> x -- ^ If the type is a node.
  -> x -- ^ If the type is a relation.
  -> x
foldMemberType WayType x _ _ =
  x
foldMemberType NodeType _ x _ =
  x
foldMemberType RelationType _ _ x =
  x

instance XmlPickler MemberType where
  xpickle =
    xpWrapMaybe (\s -> case fmap toLower s of
                         "way" -> Just WayType
                         "node" -> Just NodeType
                         "relation" -> Just RelationType
                         _ -> Nothing,
                         \t -> case t of
                                 WayType -> "way"
                                 NodeType -> "node"
                                 RelationType -> "relation") (xpAttr "type" xpText)

instance Show MemberType where
  show =
    showPickled []

-- | Constructs a member-type that is a way.
wayType ::
  MemberType
wayType =
  WayType

-- | Constructs a member-type that is a node.
nodeType ::
  MemberType
nodeType =
  NodeType

-- | Constructs a member-type that is a relation.
relationType ::
  MemberType
relationType =
  RelationType
