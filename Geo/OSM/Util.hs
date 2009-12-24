-- | Utilities for parsing OSM files.
module Geo.OSM.Util(
                     showPickled,
                     textAttr
                   ) where

import Text.XML.HXT.Arrow

-- | Pickles a value then writes the document to a string.
showPickled :: (XmlPickler a) => Attributes -> a -> String
showPickled a = concat . (pickleDoc xpickle >>> runLA (writeDocumentToString a))

textAttr :: String -> PU String
textAttr = flip xpAttr xpText
