-- | The @changeset@ element of a OSM file.
module Data.Geo.OSM.ChangesetE(
                                ChangesetE,
                                changesetE
                              ) where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.Tag
import Data.Geo.OSM.Accessor.Tags

-- | The @changeset@ element of a OSM file.
newtype ChangesetE = ChangesetE [Tag]
  deriving Eq

-- | Constructs a @changeset@ with tags.
changesetE :: [Tag] -> ChangesetE
changesetE = ChangesetE

instance XmlPickler ChangesetE where
  xpickle = xpElem "changeset" (xpWrap (changesetE, \(ChangesetE r) -> r) (xpList xpickle))

instance Show ChangesetE where
  show = showPickled []

instance Tags ChangesetE where
  tags (ChangesetE x) = x
  setTags a (ChangesetE _) = changesetE a
