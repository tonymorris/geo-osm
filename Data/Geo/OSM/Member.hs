-- | The @member@ element of a OSM file.
module Data.Geo.OSM.Member(
                       Member,
                       member
                     ) where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Data.Geo.OSM.MemberType
import Data.Geo.OSM.Accessor.Mtype
import Data.Geo.OSM.Accessor.Ref
import Data.Geo.OSM.Accessor.Role

-- | The @member@ element of a OSM file.
data Member = Member MemberType String String
  deriving Eq

instance XmlPickler Member where
  xpickle = xpElem "member" (xpWrap (\(mtype', mref', mrole') -> member mtype' mref' mrole', \(Member mtype' mref' mrole') -> (mtype', mref', mrole'))
                              (xpTriple xpickle (xpAttr "ref" xpText) (xpAttr "role" xpText)))

instance Show Member where
  show = showPickled []

instance Mtype Member where
  mtype (Member x _ _) = x
  setMtype a (Member _ b c) = member a b c

instance Ref Member where
  ref (Member _ x _) = x
  setRef b (Member a _ c) = member a b c

instance Role Member where
  role (Member _ _ x) = x
  setRole c (Member a b _) = member a b c

-- | Constructs a member with a type, ref and role.
member :: MemberType -- ^ The member @type@ attribute.
          -> String -- ^ The member @ref@ attribute.
          -> String -- ^ The member @role@ attribute.
          -> Member
member = Member
