-- | Values with a @tags@ accessor that is a list of tags.
module Geo.OSM.Accessor.Tags where

import Geo.OSM.Tag
import Geo.OSM.Accessor.K
import Geo.OSM.Accessor.V
import qualified Data.Map as M
import Control.Arrow
import Data.Foldable
import Prelude hiding (any)

class Tags a where
  tags :: a -> [Tag]

tagMap :: (Tags a) => a -> M.Map String String
tagMap = M.fromList . map (k &&& v) . tags

hasTagValue :: (Tags a) => String -> String -> a -> Bool
hasTagValue k' v' n = any (== v') (k' `M.lookup` tagMap n)
