-- | Values with a @tags@ accessor that is a list of tags.
module Data.Geo.OSM.Lens.TagsL where

import Data.Geo.OSM.Tag
import Data.Lens.Common

class TagsL a where
  tagsL ::
    Lens a [Tag]

