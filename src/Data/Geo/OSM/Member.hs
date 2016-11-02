{-# LANGUAGE TemplateHaskell #-}
-- | The @member@ element of a OSM file.
module Data.Geo.OSM.Member
(
  Member
, member
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.MemberType
import Data.Geo.OSM.Lens.TypeL
import Data.Geo.OSM.Lens.RefL
import Data.Geo.OSM.Lens.RoleL
import Control.Lens.TH

-- | The @member@ element of a OSM file.
data Member = Member {
  _memberType :: MemberType,
  _memberRef :: String,
  _memberRole :: String
  } deriving Eq

makeLenses ''Member
instance XmlPickler Member where
  xpickle =
    xpElem "member" (xpWrap (\(mtype', mref', mrole') -> member mtype' mref' mrole', \(Member mtype' mref' mrole') -> (mtype', mref', mrole'))
                    (xpTriple xpickle (xpAttr "ref" xpText) (xpAttr "role" xpText0)))

instance Show Member where
  show =
    showPickled []

instance TypeL Member where
  typeL = memberType

instance RefL Member where
  refL = memberRef

instance RoleL Member where
  roleL = memberRole

-- | Constructs a member with a type, ref and role.
member ::
  MemberType -- ^ The member @type@ attribute.
  -> String -- ^ The member @ref@ attribute.
  -> String -- ^ The member @role@ attribute.
  -> Member
member =
  Member
