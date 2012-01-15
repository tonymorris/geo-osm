-- | The children elements of the @osm@ element of a OSM file.
module Data.Geo.OSM.OSMChildren
(
  OSMChildren
, osmUser
, osmGpxFile
, osmApi
, osmChangeset
, osmNodeWayRelation
, foldOSMChildren
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.UserE
import Data.Geo.OSM.Preferences
import Data.Geo.OSM.GpxFile
import Data.Geo.OSM.Api
import Data.Geo.OSM.Changeset
import Data.Geo.OSM.NodeWayRelation

-- | The children elements of the @osm@ element of a OSM file.
data OSMChildren =
  UserE UserE
  | Preferences Preferences
  | GpxFile GpxFile
  | Api Api
  | Changeset Changeset
  | NWR [NodeWayRelation]
  deriving Eq

instance XmlPickler OSMChildren where
  xpickle =
    xpAlt (\r -> case r of
                   UserE _       -> 0
                   Preferences _ -> 1
                   GpxFile _     -> 2
                   Api _         -> 3
                   Changeset _   -> 4
                   NWR _         -> 5) [xpWrap (UserE, \(UserE u) -> u) xpickle,
                                        xpWrap (Preferences, \(Preferences p) -> p) xpickle,
                                        xpWrap (GpxFile, \(GpxFile f) -> f) xpickle,
                                        xpWrap (Api, \(Api a) -> a) xpickle,
                                        xpWrap (Changeset, \(Changeset c) -> c) xpickle,
                                        xpWrap (NWR, \(NWR k) -> k) (xpList xpickle)]

instance Show OSMChildren where
  show =
    showPickled []

-- | A @user@ element.
osmUser ::
  UserE
  -> OSMChildren
osmUser =
  UserE

-- | A @gpx_file@ element.
osmGpxFile ::
  GpxFile
  -> OSMChildren
osmGpxFile =
  GpxFile

-- | A @api@ element.
osmApi ::
  Api
  -> OSMChildren
osmApi =
  Api

-- | A @changeset@ element.
osmChangeset ::
  Changeset
  -> OSMChildren
osmChangeset =
  Changeset

-- | A list of @node@, @way@ or @relation@ elements.
osmNodeWayRelation ::
  [NodeWayRelation]
  -> OSMChildren
osmNodeWayRelation =
  NWR

-- | Folds OSM child elements (catamorphism).
foldOSMChildren ::
  (UserE -> a) -- ^ If a @user@ element.
  -> (Preferences -> a) -- ^ If a @preferences@ element.
  -> (GpxFile -> a) -- ^ If a @gpx_file@ element.
  -> (Api -> a) -- ^ If a @api@ element.
  -> (Changeset -> a) -- ^ If a @changeset@ element.
  -> ([NodeWayRelation] -> a) -- ^ If a list of @node@, @way@ or @relation@ elements.
  -> OSMChildren -- ^ The disjunctive type of child elements.
  -> a
foldOSMChildren z _ _ _ _ _ (UserE u) =
  z u
foldOSMChildren _ z _ _ _ _ (Preferences p) =
  z p
foldOSMChildren _ _ z _ _ _ (GpxFile f) =
  z f
foldOSMChildren _ _ _ z _ _ (Api a) =
  z a
foldOSMChildren _ _ _ _ z _ (Changeset c) =
  z c
foldOSMChildren _ _ _ _ _ z (NWR k) =
  z k
