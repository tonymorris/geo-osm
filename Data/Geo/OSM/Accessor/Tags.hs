-- | Values with a @tags@ accessor that is a list of tags.
module Data.Geo.OSM.Accessor.Tags where

import Data.Geo.OSM.Tag
import Data.Geo.OSM.Accessor.K
import Data.Geo.OSM.Accessor.V
import qualified Data.Map as M
import Control.Arrow
import Data.Foldable
import Prelude hiding (any)
import Data.Geo.OSM.Accessor.Accessor

class Tags a where
  tags :: a -> [Tag]
  setTags :: [Tag] -> a -> a

  setTag :: Tag -> a -> a
  setTag = setTags . return

  usingTags :: ([Tag] -> [Tag]) -> a -> a
  usingTags = tags `using` setTags

  usingTag :: (Tag -> Tag) -> a -> a
  usingTag = usingTags . map

  usingTag' :: ((String, String) -> (String, String)) -> a -> a
  usingTag' f = usingTag (\t -> uncurry tag (f (k t, v t)))

tagMap :: (Tags a) => a -> M.Map String String
tagMap = M.fromList . map (k &&& v) . tags

tagValue :: (Tags a) => a -> String -> Maybe String
tagValue = flip lookup . map (k &&& v) . tags

hasTagValue :: (Tags a) => String -> String -> a -> Bool
hasTagValue k' v' n = any (== v') (k' `M.lookup` tagMap n)
