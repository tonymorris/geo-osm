module Data.Geo.OSM.Note
(
  Note
, unNote
) where

import Text.XML.HXT.Arrow.Pickle

-- | The @note@ element of a OSM file.
data Note = Note { unNote :: String }
  deriving Eq

instance XmlPickler Note where
  xpickle =
    xpElem "note" (xpWrap (Note, unNote) (xpText))

instance Show Note where
  show =
    showPickled []


