{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

-- | The @nd@ element of a OSM file.
module Data.Geo.OSM.Nd
(
  Nd
, nd
) where

import Text.XML.HXT.Arrow.Pickle
import Data.Geo.OSM.Lens.RefL
import Data.Lens.Common
import Control.Comonad.Trans.Store
import Control.Newtype

-- | The @nd@ element of a OSM file.
newtype Nd =
  Nd String
  deriving Eq

instance XmlPickler Nd where
  xpickle =
    xpElem "nd" (xpWrap (nd, \(Nd r) -> r) (xpAttr "ref" xpText))

instance Show Nd where
  show = 
    showPickled []

instance RefL Nd where
  refL =
    Lens $ \(Nd ref) -> store (\ref -> Nd ref) ref

instance Newtype Nd String where
  pack = 
    Nd
  unpack (Nd x) =
    x

-- | Constructs a nd with a ref.
nd ::
  String -- ^ The @ref@ attribute.
  -> Nd
nd =
  Nd
