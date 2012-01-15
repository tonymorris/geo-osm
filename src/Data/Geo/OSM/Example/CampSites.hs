module Data.Geo.OSM.Example.CampSites where

import Data.Geo.OSM

-- Return all nodes tagged as camp-sites (tourism=camp_site) in the given OSM file.
campSites :: FilePath -> IO [Node]
campSites f = let p = filter ("tourism" `hasTagValue` "camp_site") . (nodes =<<)
              in fmap p (runX (xunpickleDocument (xpickle :: PU OSM) [(a_remove_whitespace, v_1)] f))
