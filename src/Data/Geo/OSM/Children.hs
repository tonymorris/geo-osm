-- | The children elements of the @osm@ element of a OSM file.
module Data.Geo.OSM.Children
(
  Children
, osmUser
, osmGpxFile
, osmApi
, osmChangeset
, osmNodeWayRelation
, foldChildren
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.User
import Data.Geo.OSM.Preferences
import Data.Geo.OSM.GpxFile
import Data.Geo.OSM.Api
import Data.Geo.OSM.Changeset
import Data.Geo.OSM.NodeWayRelation

-- | The children elements of the @osm@ element of a OSM file.
data Children =
  User User
  | Preferences Preferences
  | GpxFile GpxFile
  | Api Api
  | Changeset Changeset
  | NWR [NodeWayRelation]
  deriving Eq

instance XmlPickler Children where
  xpickle =
    xpAlt (\r -> case r of
                   User _        -> 0
                   Preferences _ -> 1
                   GpxFile _     -> 2
                   Api _         -> 3
                   Changeset _   -> 4
                   NWR _         -> 5) [xpWrap (User, \(User u) -> u) xpickle,
                                        xpWrap (Preferences, \(Preferences p) -> p) xpickle,
                                        xpWrap (GpxFile, \(GpxFile f) -> f) xpickle,
                                        xpWrap (Api, \(Api a) -> a) xpickle,
                                        xpWrap (Changeset, \(Changeset c) -> c) xpickle,
                                        xpWrap (NWR, \(NWR k) -> k) (xpList xpickle)]

instance Show Children where
  show =
    showPickled []

-- | A @user@ element.
osmUser ::
  User
  -> Children
osmUser =
  User

-- | A @gpx_file@ element.
osmGpxFile ::
  GpxFile
  -> Children
osmGpxFile =
  GpxFile

-- | A @api@ element.
osmApi ::
  Api
  -> Children
osmApi =
  Api

-- | A @changeset@ element.
osmChangeset ::
  Changeset
  -> Children
osmChangeset =
  Changeset

-- | A list of @node@, @way@ or @relation@ elements.
osmNodeWayRelation ::
  [NodeWayRelation]
  -> Children
osmNodeWayRelation =
  NWR

-- | Folds OSM child elements (catamorphism).
foldChildren ::
  (User -> a) -- ^ If a @user@ element.
  -> (Preferences -> a) -- ^ If a @preferences@ element.
  -> (GpxFile -> a) -- ^ If a @gpx_file@ element.
  -> (Api -> a) -- ^ If a @api@ element.
  -> (Changeset -> a) -- ^ If a @changeset@ element.
  -> ([NodeWayRelation] -> a) -- ^ If a list of @node@, @way@ or @relation@ elements.
  -> Children -- ^ The disjunctive type of child elements.
  -> a
foldChildren z _ _ _ _ _ (User u) =
  z u
foldChildren _ z _ _ _ _ (Preferences p) =
  z p
foldChildren _ _ z _ _ _ (GpxFile f) =
  z f
foldChildren _ _ _ z _ _ (Api a) =
  z a
foldChildren _ _ _ _ z _ (Changeset c) =
  z c
foldChildren _ _ _ _ _ z (NWR k) =
  z k
