-- | The @bounds@ element of a OSM file.
module Data.Geo.OSM.Bounds
(
  Bounds
, bounds
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.MinlatL
import Data.Geo.OSM.Lens.MaxlatL
import Data.Geo.OSM.Lens.MinlonL
import Data.Geo.OSM.Lens.MaxlonL
import Data.Geo.OSM.Lens.OriginL
import Data.Lens.Common
import Control.Comonad.Trans.Store

-- | The @bounds@ element of a OSM file.
data Bounds =
  Bounds String String String String (Maybe String)
  deriving Eq

instance XmlPickler Bounds where
  xpickle =
    xpElem "bounds" (xpWrap (\(minlat', minlon', maxlat', maxlon', origin') -> bounds minlat' minlon' maxlat' maxlon' origin', \(Bounds minlat' minlon' maxlat' maxlon' origin') -> (minlat', minlon', maxlat', maxlon', origin'))
                              (xp5Tuple (xpAttr "minlat" xpText) (xpAttr "minlon" xpText) (xpAttr "maxlat" xpText) (xpAttr "maxlon" xpText) (xpOption (xpAttr "origin" xpText))))

instance Show Bounds where
  show =
    showPickled []

instance MinlatL Bounds where
  minlatL =
    Lens $ \(Bounds minlat minlon maxlat maxlon origin) -> store (\minlat -> Bounds minlat minlon maxlat maxlon origin) minlat

instance MinlonL Bounds where
  minlonL =
    Lens $ \(Bounds minlat minlon maxlat maxlon origin) -> store (\minlon -> Bounds minlat minlon maxlat maxlon origin) minlon

instance MaxlatL Bounds where
  maxlatL =
    Lens $ \(Bounds minlat minlon maxlat maxlon origin) -> store (\maxlat -> Bounds minlat minlon maxlat maxlon origin) maxlat

instance MaxlonL Bounds where
  maxlonL =
    Lens $ \(Bounds minlat minlon maxlat maxlon origin) -> store (\maxlon -> Bounds minlat minlon maxlat maxlon origin) maxlon

instance OriginL Bounds where
  originL =
    Lens $ \(Bounds minlat minlon maxlat maxlon origin) -> store (\origin -> Bounds minlat minlon maxlat maxlon origin) origin

-- | Constructs a bounds with a minlat, minlon, maxlat, maxlon and origin attributes.
bounds ::
  String -- ^ The @minlat@ attribute.
  -> String -- ^ The @minlon@ attribute.
  -> String -- ^ The @maxlat@ attribute.
  -> String -- ^ The @maxlon@ attribute.
  -> Maybe String -- ^ The @origin@ attribute.
  -> Bounds
bounds =
  Bounds
