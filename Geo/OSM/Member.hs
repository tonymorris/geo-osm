-- | The @member@ element of a OSM file.
module Geo.OSM.Member(
                       Member,
                       member
                     ) where

import Text.XML.HXT.Arrow
import Geo.OSM.MemberType
import Geo.OSM.Util
import Geo.OSM.Accessor.Mtype
import Geo.OSM.Accessor.Ref
import Geo.OSM.Accessor.Role

-- | The @member@ element of a OSM file.
data Member = Member MemberType String String
  deriving Eq

instance XmlPickler Member where
  xpickle = xpElem "member" (xpWrap (\(mtype', mref', mrole') -> Member mtype' mref' mrole', \(Member mtype' mref' mrole') -> (mtype', mref', mrole'))
                              (xpTriple xpickle (xpAttr "ref" xpText) (xpAttr "role" xpText)))

instance Show Member where
  show = showPickled []

instance Mtype Member where
  mtype (Member x _ _) = x

instance Ref Member where
  ref (Member _ x _) = x

instance Role Member where
  role (Member _ _ x) = x

-- | Constructs a member with a type, ref and role.
member :: MemberType -- ^ The member @type@ attribute.
          -> String -- ^ The member @ref@ attribute.
          -> String -- ^ The member @role@ attribute.
          -> Member
member = Member
