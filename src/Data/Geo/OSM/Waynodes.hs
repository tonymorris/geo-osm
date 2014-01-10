{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

-- | The @waynodes@ element of a OSM file.
module Data.Geo.OSM.Waynodes
(
  Waynodes
, wayMaximum
) where


import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.MaximumL
import Data.Lens.Common
import Control.Comonad.Trans.Store
import Control.Newtype
import Prelude hiding (maximum)

-- | The @waynodes@ element of a OSM file.
newtype Waynodes = Waynodes {
    wayMaximum :: String -- ^ The @maximum@ attribute.
} deriving Eq

instance XmlPickler Waynodes where
  xpickle =
    xpElem "waynodes" (xpWrap (Waynodes, wayMaximum) (xpAttr "maximum" xpText))

instance Show Waynodes where
  show =
    showPickled []

instance MaximumL Waynodes where
  maximumL =
    Lens $ \(Waynodes maximum) -> store (\maximum' -> Waynodes maximum') maximum

instance Newtype Waynodes String where
  pack = 
    Waynodes
  unpack (Waynodes x) =
    x

