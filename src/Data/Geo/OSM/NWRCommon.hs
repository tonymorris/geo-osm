{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | The common attributes between the @node@, @way@ and @relation@ elements.
module Data.Geo.OSM.NWRCommon
(
  NWRCommon
, nwrCommon
) where

import Text.XML.HXT.Arrow.Pickle
import Control.Applicative
import Data.Char
import Data.Geo.OSM.Tag
import Data.Geo.OSM.Lens.IdL
import Data.Geo.OSM.Lens.TagsL
import Data.Geo.OSM.Lens.ChangesetL
import Data.Geo.OSM.Lens.VisibleL
import Data.Geo.OSM.Lens.UserL
import Data.Geo.OSM.Lens.UidL
import Data.Geo.OSM.Lens.TimestampL
import Data.Lens.Common
import Control.Comonad.Trans.Store

-- | The common attributes between the @node@, @way@ and @relation@ elements.
data NWRCommon =
  NWRCommon String [Tag] (Maybe String) Bool (Maybe String, Maybe String) (Maybe String)
  deriving Eq

instance XmlPickler NWRCommon where
  xpickle =
    xpWrap (\(a, b, c, d, e, f) -> nwrCommon a b c d e f, \(NWRCommon a b c d e f) -> (a, b, c, d, e, f))
           (xp6Tuple (xpAttr "id" xpText)
                     (xpList xpickle)
                     (xpOption (xpAttr "changeset" xpText))
                     (xpDefault True (xpWrapMaybe (\s -> case toLower <$> s of
                                                           "true" -> Just True
                                                           "false" -> Just False
                                                           _ -> Nothing, (toLower <$>) . show) (xpAttr "visible" xpText)))
                     (xpPair (xpOption (xpAttr "user" xpText)) (xpOption (xpAttr "uid" xpText)))
                     (xpOption (xpAttr "timestamp" xpText)))

instance Show NWRCommon where
  show =
    showPickled []

instance IdL NWRCommon where
  idL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp) -> store (\id -> NWRCommon id tags changeset visible (user, uid) timestamp) id

instance TagsL NWRCommon where
  tagsL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp) -> store (\tags -> NWRCommon id tags changeset visible (user, uid) timestamp) tags

instance ChangesetL NWRCommon where
  changesetL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp) -> store (\changeset -> NWRCommon id tags changeset visible (user, uid) timestamp) changeset

instance VisibleL NWRCommon where
  visibleL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp) -> store (\visible -> NWRCommon id tags changeset visible (user, uid) timestamp) visible

instance UserL NWRCommon (Maybe String) where
  userL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp) -> store (\user -> NWRCommon id tags changeset visible (user, uid) timestamp) user

instance UidL NWRCommon where
  uidL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp) -> store (\uid -> NWRCommon id tags changeset visible (user, uid) timestamp) uid

instance TimestampL NWRCommon (Maybe String) where
  timestampL =
    Lens $ \(NWRCommon id tags changeset visible (user, uid) timestamp) -> store (\timestamp -> NWRCommon id tags changeset visible (user, uid) timestamp) timestamp

-- | Constructs with id, list of tags, changeset, visible, user&uid and timestamp.
nwrCommon ::
  String -- ^ The @id@ attribute.
  -> [Tag] -- ^ The list of tags (@tag@ elements).
  -> Maybe String -- ^ The @changeset@ attribute.
  -> Bool -- ^ The @visible@ attribute.
  -> (Maybe String, Maybe String) -- ^ The @user@ and @uid@ attributes.
  -> Maybe String -- ^ The @timestamp@ attribute.
  -> NWRCommon
nwrCommon =
  NWRCommon
