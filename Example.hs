import Geo.OSM

-- Return all nodes tagged as camp-sites (tourism=camp_site) in the give OSM file.
campSites :: FilePath -> IO [Node]
campSites f = let p = filter ("tourism" `hasTagValue` "camp_site") . (nodes =<<)
              in fmap p (runX (xunpickleDocument (xpickle :: PU OSM) [(a_remove_whitespace, v_1)] f))



{-


*Main> campSites "MountBarney.osm"
[<node lat="-28.309999" lon="152.460006" id="316587114" changeset="108027" user="Spindoc Bob" uid="11037" timestamp="2008-12-04T11:00:06Z"><tag k="name" v="Koreelah Creek Campground - Koreelah NP"/><tag k="tourism" v="camp_site"/></node>,<node lat="-28.2131914" lon="152.4264518" id="596132847" changeset="3406101" user="Tony Morris" uid="66376" timestamp="2009-12-19T11:19:14Z"><tag k="name" v="Emu Creek"/><tag k="tourism" v="camp_site"/></node>,<node lat="-28.389999" lon="152.61" id="316587113" changeset="108027" user="Spindoc Bob" uid="11037" timestamp="2008-12-04T11:00:05Z"><tag k="name" v="Woodenbong Campground"/><tag k="tourism" v="camp_site"/></node>,<node lat="-28.2819309" lon="152.690921" id="440615858" changeset="3405819" user="Tony Morris" uid="66376" timestamp="2009-12-19T10:37:58Z"><tag k="name" v="Rum Jungle"/><tag k="tourism" v="camp_site"/></node>,<node lat="-28.2812388" lon="152.6916212" id="596096271" changeset="3405991" user="Tony Morris" uid="66376" timestamp="2009-12-19T11:02:19Z"><tag k="name" v="Old Hut Site"/><tag k="tourism" v="camp_site"/></node>,<node lat="-28.2961173" lon="152.7150912" id="596098699" changeset="3405772" user="Tony Morris" uid="66376" timestamp="2009-12-19T10:31:22Z"><tag k="name" v="Site 10"/><tag k="tourism" v="camp_site"/></node>,<node lat="-28.299066" lon="152.7198659" id="596098700" changeset="3405772" user="Tony Morris" uid="66376" timestamp="2009-12-19T10:31:22Z"><tag k="name" v="Site 9"/><tag k="tourism" v="camp_site"/></node>,<node lat="-28.2056648" lon="152.6679486" id="442564691" changeset="1844269" user="Tony Morris" uid="66376" timestamp="2009-07-16T12:44:51Z"><tag k="tourism" v="camp_site"/></node>,<node lat="-28.219999" lon="152.86" id="316607159" changeset="108027" user="Spindoc Bob" uid="11037" timestamp="2008-12-04T11:57:12Z"><tag k="name" v="EM Tilley Park"/><tag k="tourism" v="camp_site"/></node>,<node lat="-28.209999" lon="152.770004" id="316607169" changeset="1824310" user="Tony Morris" uid="66376" timestamp="2009-07-14T08:21:51Z"><tag k="name" v="Flanagan Reserve"/><tag k="tourism" v="camp_site"/></node>,<node lat="-28.2" lon="152.779998" id="316607407" changeset="108027" user="Spindoc Bob" uid="11037" timestamp="2008-12-04T11:57:28Z"><tag k="name" v="Bigriggen Park Reserve"/><tag k="tourism" v="camp_site"/></node>]


-}
