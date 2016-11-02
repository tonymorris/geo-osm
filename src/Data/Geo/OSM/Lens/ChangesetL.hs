-- | Values with a @changeset@ optional string accessor.
module Data.Geo.OSM.Lens.ChangesetL where

import Control.Lens.Lens

class ChangesetL a where
  changesetL ::
    Lens' a (Maybe String)
