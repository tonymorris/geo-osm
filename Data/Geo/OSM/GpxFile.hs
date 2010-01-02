{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

-- | The @gpx_file@ element of a OSM file.
module Data.Geo.OSM.GpxFile(
                             GpxFile,
                             gpxFile
                           ) where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Char
import Data.Geo.OSM.Accessor.Id
import Data.Geo.OSM.Accessor.Name
import Data.Geo.OSM.Accessor.Lat
import Data.Geo.OSM.Accessor.Lon
import Data.Geo.OSM.Accessor.User
import Data.Geo.OSM.Accessor.Public
import Data.Geo.OSM.Accessor.Pending
import Data.Geo.OSM.Accessor.Timestamp

-- | The @gpx_file@ element of a OSM file.
data GpxFile = GpxFile String String String String String Bool Bool String
  deriving Eq

-- | Constructs a @gpx_file@ with an id, name, lat, lon, user, public, pending and timestamp.
gpxFile
  :: String -- ^ The id.
     -> String -- ^ The name.
     -> String -- ^ The lat.
     -> String -- ^ The lon.
     -> String -- ^ The user.
     -> Bool -- ^ The public.
     -> Bool -- ^ The pending.
     -> String -- ^ The timestamp.
     -> GpxFile
gpxFile = GpxFile

instance XmlPickler GpxFile where
  xpickle = let b = xpWrapMaybe (\s -> case fmap toLower s of "true" -> Just True
                                                              "false" -> Just False
                                                              _ -> Nothing, (fmap toLower) . show)
            in xpElem "gpx_file" (xpWrap (\(id', name', lat', lon', user', public', pending', timestamp') -> gpxFile id' name' lat' lon' user' public' pending' timestamp', undefined) (xp8Tuple (xpAttr "id" xpText) (xpAttr "name" xpText) (xpAttr "lat" xpText) (xpAttr "lon" xpText) (xpAttr "user" xpText) (xpDefault False (b (xpAttr "public" xpText))) (xpDefault False (b (xpAttr "pending" xpText))) (xpAttr "timestamp" xpText)))

instance Show GpxFile where
  show = showPickled []

instance Id GpxFile where
  id (GpxFile x _ _ _ _ _ _ _) = x
  setId a (GpxFile _ b c d e f g h) = gpxFile a b c d e f g h

instance Name GpxFile where
  name (GpxFile _ x _ _ _ _ _ _) = x
  setName b (GpxFile a _ c d e f g h) = gpxFile a b c d e f g h

instance Lat GpxFile where
  lat (GpxFile _ _ x _ _ _ _ _) = x
  setLat c (GpxFile a b _ d e f g h) = gpxFile a b c d e f g h

instance Lon GpxFile where
  lon (GpxFile _ _ _ x _ _ _ _) = x
  setLon d (GpxFile a b c _ e f g h) = gpxFile a b c d e f g h

instance User GpxFile String where
  user (GpxFile _ _ _ _ x _ _ _) = x
  setUser e (GpxFile a b c d _ f g h) = gpxFile a b c d e f g h

instance Public GpxFile where
  public (GpxFile _ _ _ _ _ x _ _) = x
  setPublic f (GpxFile a b c d e _ g h) = gpxFile a b c d e f g h

instance Pending GpxFile where
  pending (GpxFile _ _ _ _ _ _ x _) = x
  setPending g (GpxFile a b c d e f _ h) = gpxFile a b c d e f g h

instance Timestamp GpxFile String where
  timestamp (GpxFile _ _ _ _ _ _ _ x) = x
  setTimestamp h (GpxFile a b c d e f g _) = gpxFile a b c d e f g h
