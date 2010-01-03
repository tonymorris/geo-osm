module Data.Geo.OSM.Example.WayTags where

import Data.Geo.OSM
import Data.List

-- Updates the given OSM file with a new OSM file by replacing specific suffixes of ways tagged with "name".
-- e.g. A way such as name="George St" will become name="George Street".
wayTags :: FilePath -> FilePath -> IO ()
wayTags = flip interactsOSM [" St"  ==> " Street",
                             " Pl"  ==> " Place",
                             " Tce" ==> " Terrace",
                             " Cct" ==> " Circuit"]

-- Updates the a given name suffix with a new suffix
(==>) :: (NodeWayRelations a)
         => String -- The suffix to fix with the new suffix.
         -> String -- The new suffix.
         -> a -- The OSM value.
         -> a -- The new OSM value.
(==>) x = flip usingWay . flip usingTag' . (\y (k, v) ->
  let v' = reverse v
  in (k, if k == "name" && reverse x `isPrefixOf` v'
            then reverse (reverse y ++ drop (length x) v')
            else v))