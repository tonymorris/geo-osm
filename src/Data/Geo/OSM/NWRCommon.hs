{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, CPP, TemplateHaskell #-}

-- | The common attributes between the @node@, @way@ and @relation@ elements.
module Data.Geo.OSM.NWRCommon
(
  NWRCommon, HasNWRCommon(..)
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
import Control.Lens.TH
import Control.Lens.Tuple

-- | The common attributes between the @node@, @way@ and @relation@ elements.

data NWRCommon = NWRCommon {
  _commonId :: String,
  _commonTags :: [Tag],
  _commonChangeset :: Maybe String,
  _commonVisible :: Bool,
  _commonUserUid :: (Maybe String, Maybe String),
  _commonTimestamp :: Maybe String,
  _commonVersion :: Maybe String
  } deriving (Eq)

makeClassy ''NWRCommon

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
  idL = commonId

instance TagsL NWRCommon where
  tagsL = commonTags

instance ChangesetL NWRCommon where
  changesetL = commonChangeset

instance VisibleL NWRCommon where
  visibleL = commonVisible

instance UserL NWRCommon (Maybe String) where
  userL =  commonUserUid . _1

instance UidL NWRCommon where
 uidL = commonUserUid . _2

instance TimestampL NWRCommon (Maybe String) where
  timestampL = commonTimestamp

instance VersionL NWRCommon (Maybe String) where
  versionL = commonVersion


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
