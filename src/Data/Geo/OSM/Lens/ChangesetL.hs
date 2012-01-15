-- | Values with a @changeset@ optional string accessor.
module Data.Geo.OSM.Lens.ChangesetL where

import Data.Lens.Common

class ChangesetL a where
  changesetL ::
    Lens a (Maybe String)

