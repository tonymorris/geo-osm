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
import Data.Lens.Common
import Control.Comonad.Trans.Store

-- | The @member@ element of a OSM file.
data Member = 
  Member MemberType String String
  deriving Eq

instance XmlPickler Member where
  xpickle =
    xpElem "member" (xpWrap (\(mtype', mref', mrole') -> member mtype' mref' mrole', \(Member mtype' mref' mrole') -> (mtype', mref', mrole'))
                    (xpTriple xpickle (xpAttr "ref" xpText) (xpAttr "role" xpText)))

instance Show Member where
  show =
    showPickled []

instance TypeL Member where
  typeL =
    Lens $ \(Member typ ref role) -> store (\typ -> Member typ ref role) typ

instance RefL Member where
  refL =
    Lens $ \(Member typ ref role) -> store (\ref -> Member typ ref role) ref

instance RoleL Member where
  roleL =
    Lens $ \(Member typ ref role) -> store (\role -> Member typ ref role) role

-- | Constructs a member with a type, ref and role.
member ::
  MemberType -- ^ The member @type@ attribute.
  -> String -- ^ The member @ref@ attribute.
  -> String -- ^ The member @role@ attribute.
  -> Member
member =
  Member
