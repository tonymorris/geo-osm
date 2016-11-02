{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

-- | The @gpx_file@ element of a OSM file.
module Data.Geo.OSM.GpxFile
(
  GpxFile
, gpxFile
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Char
import Data.Geo.OSM.Lens.IdL
import Data.Geo.OSM.Lens.NameL
import Data.Geo.OSM.Lens.LatL
import Data.Geo.OSM.Lens.LonL
import Data.Geo.OSM.Lens.UserL
import Data.Geo.OSM.Lens.PublicL
import Data.Geo.OSM.Lens.PendingL
import Data.Geo.OSM.Lens.TimestampL
import Control.Lens.Lens
import Control.Comonad.Trans.Store

-- | The @gpx_file@ element of a OSM file.
data GpxFile =
  GpxFile String String String String String Bool Bool String
  deriving Eq

-- | Constructs a @gpx_file@ with an id, name, lat, lon, user, public, pending and timestamp.
gpxFile ::
  String -- ^ The @id@ attribute.
  -> String -- ^ The @name@ attribute.
  -> String -- ^ The @lat@ attribute.
  -> String -- ^ The @lon@ attribute.
  -> String -- ^ The @user@ attribute.
  -> Bool -- ^ The @public@ attribute.
  -> Bool -- ^ The @pending@ attribute.
  -> String -- ^ The @timestamp@ attribute.
  -> GpxFile
gpxFile =
  GpxFile

instance XmlPickler GpxFile where
  xpickle =
    let b = xpWrapMaybe (\s -> case fmap toLower s
                                 of "true"  -> Just True
                                    "false" -> Just False
                                    _       -> Nothing, fmap toLower . show)
    in xpElem "gpx_file" (xpWrap (\(id'', name', lat', lon', user', public', pending', timestamp') -> gpxFile id'' name' lat' lon' user' public' pending' timestamp', \(GpxFile id'' name' lat' lon' user' public' pending' timestamp') -> (id'', name', lat', lon', user', public', pending', timestamp')) (xp8Tuple (xpAttr "id" xpText) (xpAttr "name" xpText) (xpAttr "lat" xpText) (xpAttr "lon" xpText) (xpAttr "user" xpText) (xpDefault False (b (xpAttr "public" xpText))) (xpDefault False (b (xpAttr "pending" xpText))) (xpAttr "timestamp" xpText)))

instance Show GpxFile where
  show =
    showPickled []

instance IdL GpxFile where
  idL =
    Lens $ \(GpxFile id name lat lon user public pending timestamp) -> store (\id -> GpxFile id name lat lon user public pending timestamp) id

instance NameL GpxFile where
  nameL =
    Lens $ \(GpxFile id name lat lon user public pending timestamp) -> store (\name -> GpxFile id name lat lon user public pending timestamp) name

instance LatL GpxFile where
  latL =
    Lens $ \(GpxFile id name lat lon user public pending timestamp) -> store (\lat -> GpxFile id name lat lon user public pending timestamp) lat

instance LonL GpxFile where
  lonL =
    Lens $ \(GpxFile id name lat lon user public pending timestamp) -> store (\lon -> GpxFile id name lat lon user public pending timestamp) lon

instance UserL GpxFile String where
  userL =
    Lens $ \(GpxFile id name lat lon user public pending timestamp) -> store (\user -> GpxFile id name lat lon user public pending timestamp) user

instance PublicL GpxFile where
  publicL =
    Lens $ \(GpxFile id name lat lon user public pending timestamp) -> store (\public -> GpxFile id name lat lon user public pending timestamp) public

instance PendingL GpxFile where
  pendingL =
    Lens $ \(GpxFile id name lat lon user public pending timestamp) -> store (\pending -> GpxFile id name lat lon user public pending timestamp) pending

instance TimestampL GpxFile String where
  timestampL =
    Lens $ \(GpxFile id name lat lon user public pending timestamp) -> store (\timestamp -> GpxFile id name lat lon user public pending timestamp) timestamp

