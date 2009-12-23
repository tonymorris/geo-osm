-- | Utilities for parsing OSM files.
module Geo.OSM.Util(
                     showPickled
                   ) where

import Text.XML.HXT.Arrow

-- | Pickles a value then writes the document to a string.
showPickled :: (XmlPickler a) => Attributes -> a -> String
showPickled a = concat . (pickleDoc xpickle >>> runLA (writeDocumentToString a))
