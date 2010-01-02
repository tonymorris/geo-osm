{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | The common attributes between the @node@, @way@ and @relation@ elements.
module Data.Geo.OSM.NWRCommon(
                          NWRCommon,
                          nwrCommon
                        ) where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Control.Applicative
import Data.Char
import Data.Geo.OSM.Tag
import Data.Geo.OSM.Accessor.Id
import Data.Geo.OSM.Accessor.Tags
import Data.Geo.OSM.Accessor.Changeset
import Data.Geo.OSM.Accessor.Visible
import Data.Geo.OSM.Accessor.User
import Data.Geo.OSM.Accessor.Uid
import Data.Geo.OSM.Accessor.Timestamp

-- | The common attributes between the @node@, @way@ and @relation@ elements.
data NWRCommon = NWRCommon String [Tag] (Maybe String) Bool (Maybe String, Maybe String) (Maybe String)
  deriving Eq

instance XmlPickler NWRCommon where
  xpickle = xpWrap (\(a, b, c, d, e, f) -> nwrCommon a b c d e f, \(NWRCommon a b c d e f) -> (a, b, c, d, e, f))
              (xp6Tuple (xpAttr "id" xpText)
                        (xpList xpickle)
                        (xpOption (xpAttr "changeset" xpText))
                        (xpDefault True (xpWrapMaybe (\s -> case toLower <$> s of "true" -> Just True
                                                                                  "false" -> Just False
                                                                                  _ -> Nothing, (toLower <$>) . show) (xpAttr "visible" xpText)))
                        (xpPair (xpOption (xpAttr "user" xpText)) (xpOption (xpAttr "uid" xpText)))
                        (xpOption (xpAttr "timestamp" xpText)))

instance Show NWRCommon where
  show = showPickled []

instance Id NWRCommon where
  id (NWRCommon x _ _ _ _ _) = x
  setId a (NWRCommon _ b c d e f) = nwrCommon a b c d e f

instance Tags NWRCommon where
  tags (NWRCommon _ x _ _ _ _) = x
  setTags b (NWRCommon a _ c d e f) = nwrCommon a b c d e f

instance Changeset NWRCommon where
  changeset (NWRCommon _ _ x _ _ _) = x
  setChangeset c (NWRCommon a b _ d e f) = nwrCommon a b c d e f

instance Visible NWRCommon where
  visible (NWRCommon _ _ _ x _ _) = x
  setVisible d (NWRCommon a b c _ e f) = nwrCommon a b c d e f

instance User NWRCommon (Maybe String) where
  user (NWRCommon _ _ _ _ (x, _) _) = x
  setUser e (NWRCommon a b c d ee f) = nwrCommon a b c d ((first . const) e ee) f

instance Uid NWRCommon where
  uid (NWRCommon _ _ _ _ (_, x) _) = x
  setUid e (NWRCommon a b c d ee f) = nwrCommon a b c d ((second . const) e ee) f

instance Timestamp NWRCommon (Maybe String) where
  timestamp (NWRCommon _ _ _ _ _ x) = x
  setTimestamp f (NWRCommon a b c d e _) = nwrCommon a b c d e f

-- | Constructs with id, list of tags, changeset, visible, user&uid and timestamp.
nwrCommon :: String -- ^ The @id@ attribute.
             -> [Tag] -- ^ The list of tags (@tag@ elements).
             -> Maybe String -- ^ The @changeset@ attribute.
             -> Bool -- ^ The @visible@ attribute.
             -> (Maybe String, Maybe String) -- ^ The @user@ and @uid@ attributes.
             -> Maybe String -- ^ The @timestamp@ attribute.
             -> NWRCommon
nwrCommon = NWRCommon
