{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, CPP #-}

-- | The common attributes between the @node@, @way@ and @relation@ elements.
module Data.Geo.OSM.NWRCommon
(
  NWRCommon
, nwrCommon
) where

#if __GLASGOW_HASKELL__ >= 800
import Control.Applicative ()
#else
import Control.Applicative
#endif
import Text.XML.HXT.Arrow.Pickle
import Data.Char
import Data.Geo.OSM.Tag
import Data.Geo.OSM.Lens.IdL
import Data.Geo.OSM.Lens.TagsL
import Data.Geo.OSM.Lens.ChangesetL
import Data.Geo.OSM.Lens.VisibleL
import Data.Geo.OSM.Lens.UserL
import Data.Geo.OSM.Lens.UidL
import Data.Geo.OSM.Lens.TimestampL
import Data.Geo.OSM.Lens.VersionL
import Data.Lens.Common
import Control.Comonad.Trans.Store

-- | The common attributes between the @node@, @way@ and @relation@ elements.
data NWRCommon =
  NWRCommon String [Tag] (Maybe String) Bool (Maybe String, Maybe String) (Maybe String) (Maybe String)
  deriving Eq

instance XmlPickler NWRCommon where
  xpickle =
    xpWrap (\(a, b, c, d, e, f, g) -> nwrCommon a b c d e f g, \(NWRCommon a b c d e f g) -> (a, b, c, d, e, f, g))
           (xp7Tuple (xpAttr "id" xpText)
                     (xpList xpickle)
                     (xpOption (xpAttr "changeset" xpText))
                     (xpDefault True (xpWrapMaybe (\s -> case toLower <$> s of
                                                           "true" -> Just True
                                                           "false" -> Just False
                                                           _ -> Nothing, (toLower <$>) . show) (xpAttr "visible" xpText)))
                     (xpPair (xpOption (xpAttr "user" xpText)) (xpOption (xpAttr "uid" xpText)))
            (xpOption (xpAttr "timestamp" xpText))
            (xpOption (xpAttr "version" xpText))
           )

instance Show NWRCommon where
  show =
    showPickled []

instance IdL NWRCommon where
  idL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp version) -> store (\id -> NWRCommon id tags changeset visible (user, uid) timestamp version) id

instance TagsL NWRCommon where
  tagsL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp version) -> store (\tags -> NWRCommon id tags changeset visible (user, uid) timestamp version) tags

instance ChangesetL NWRCommon where
  changesetL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp version) -> store (\changeset -> NWRCommon id tags changeset visible (user, uid) timestamp version) changeset

instance VisibleL NWRCommon where
  visibleL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp version) -> store (\visible -> NWRCommon id tags changeset visible (user, uid) timestamp version) visible

instance UserL NWRCommon (Maybe String) where
  userL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp version) -> store (\user -> NWRCommon id tags changeset visible (user, uid) timestamp version) user

instance UidL NWRCommon where
  uidL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp version) -> store (\uid -> NWRCommon id tags changeset visible (user, uid) timestamp version) uid

instance TimestampL NWRCommon (Maybe String) where
  timestampL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp version) -> store (\timestamp -> NWRCommon id tags changeset visible (user, uid) timestamp version) timestamp

instance VersionL NWRCommon (Maybe String) where
  versionL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp version) -> store (\version -> NWRCommon id tags changeset visible (user, uid) timestamp version) version

-- | Constructs with id, list of tags, changeset, visible, user&uid and timestamp.
nwrCommon ::
  String -- ^ The @id@ attribute.
  -> [Tag] -- ^ The list of tags (@tag@ elements).
  -> Maybe String -- ^ The @changeset@ attribute.
  -> Bool -- ^ The @visible@ attribute.
  -> (Maybe String, Maybe String) -- ^ The @user@ and @uid@ attributes.
  -> Maybe String -- ^ The @timestamp@ attribute.
  -> Maybe String -- ^ The @version@ attribute.
  -> NWRCommon
nwrCommon =
  NWRCommon
