{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

-- | The @osm@ element of a OSM file, which is the root element. <http://wiki.openstreetmap.org/wiki/API_v0.6/DTD>
module Data.Geo.OSM.OSM(
                    OSM,
                    osm,
                    readOsmFile,
                    readOsmFiles,
                    interactOSMIO,
                    interactsOSMIO,
                    interactOSMIO',
                    interactsOSMIO',
                    interactOSM,
                    interactsOSM,
                    interactOSM',
                    interactsOSM'
                  ) where

import Prelude hiding (mapM, foldr)

import Text.XML.HXT.Core
import Text.XML.HXT.Extras
import Control.Monad hiding (mapM)
import Data.Foldable
import Data.Traversable
import Data.Geo.OSM.OSMChildren
import Data.Geo.OSM.Bound
import Data.Geo.OSM.Bounds
import Data.Geo.OSM.Accessor.Version
import Data.Geo.OSM.Accessor.Generator
import Data.Geo.OSM.Accessor.BoundOrs
import Data.Geo.OSM.Accessor.NodeWayRelations
import Data.Monoid

-- | The @osm@ element of a OSM file, which is the root element.
data OSM = OSM String (Maybe String) (Maybe (Either Bound Bounds)) OSMChildren
  deriving Eq

instance XmlPickler OSM where
  xpickle =
    xpElem "osm" (xpWrap (\(version', generator', bound', nwr') -> osm version' generator' bound' nwr', \(OSM version' generator' bound' nwr') -> (version', generator', bound', nwr'))
      (xp4Tuple (xpAttr "version" xpText)
                (xpOption (xpAttr "generator" xpText))
                (xpOption (xpAlt (either (const 0) (const 1)) [xpWrap (Left, \(Left b) -> b) xpickle, xpWrap (Right, \(Right b) -> b) xpickle]))
                xpickle))

instance Show OSM where
  show =
    showPickled []

instance Version OSM String where
  version (OSM x _ _ _) =
    x
  setVersion a (OSM _ b c d) =
    osm a b c d

instance Generator OSM where
  generator (OSM _ x _ _) =
    x
  setGenerator b (OSM a _  c d) =
    osm a b c d

instance BoundOrs OSM where
  boundOrs (OSM _ _ x _) n b bs =
    case x
    of Nothing -> n
       Just (Left b') -> b b'
       Just (Right b') -> bs b'
  setBoundOrs c (OSM a b _ d) =
    osm a b c d

instance NodeWayRelations OSM where
  nwrs (OSM _ _ _ x) =
    let t = const []
    in foldOSMChildren t t t t t id x
  setNwrs d (OSM a b c _) =
    osm a b c (osmNodeWayRelation d)

-- | Constructs a osm with a version, bound or bounds, and node attributes and way or relation elements.
osm :: String -- ^ The @version@ attribute.
       -> Maybe String -- ^ The @generator@ attribute.
       -> Maybe (Either Bound Bounds) -- ^ The @bound@ or @bounds@ elements.
       -> OSMChildren -- ^ The child elements.
       -> OSM
osm =
  OSM

-- | Reads an OSM file into a list of @OSM@ values removing whitespace.
readOsmFile ::
  FilePath
  -> IO [OSM]
readOsmFile =
  runX . xunpickleDocument (xpickle :: PU OSM) ([withRemoveWS yes, withFileMimeType v_1]) -- FIXME v_1?

-- | Reads 0 or more OSM files into a list of @OSM@ values removing whitespace.
readOsmFiles ::
  [FilePath]
  -> IO [OSM]
readOsmFiles =
  fmap join . mapM readOsmFile

-- | Reads a OSM file, executes the given function on the XML, then writes the given file.
interactOSMIO' ::
  (OSM -> IO OSM) -- ^ The function to execute on the XML that is read.
  -> SysConfigList -- ^ The options for reading the OSM file.
  -> FilePath -- ^ The OSM file to read.
  -> SysConfigList -- ^ The options for writing the OSM file.
  -> FilePath -- ^ The OSM file to write.
  -> IO ()
interactOSMIO' f froma from toa to =
  runX (xunpickleDocument (xpickle :: PU OSM) froma from >>> arrIO f >>> xpickleDocument (xpickle :: PU OSM) toa to) >> return ()

-- | Reads a OSM file, executes the given functions on the XML, then writes the given file.
interactsOSMIO' ::
  Foldable t =>
  t (OSM -> IO OSM) -- ^ The function to execute on the XML that is read.
  -> SysConfigList -- ^ The options for reading the OSM file.
  -> FilePath -- ^ The OSM file to read.
  -> SysConfigList -- ^ The options for writing the OSM file.
  -> FilePath -- ^ The OSM file to write.
  -> IO ()
interactsOSMIO' =
  interactOSMIO' . sumM

-- | Reads a OSM file removing whitespace, executes the given function on the XML, then writes the given file with indentation.
interactOSMIO ::
  (OSM -> IO OSM) -- ^ The function to execute on the XML that is read.
  -> FilePath -- ^ The OSM file to read.
  -> FilePath -- ^ The OSM file to write.
  -> IO ()
interactOSMIO f from =
  interactOSMIO' f [withRemoveWS yes, withFileMimeType v_1] from [withIndent yes, withFileMimeType v_1]

-- | Reads a OSM file removing whitespace, executes the given functions on the XML, then writes the given file with indentation.
interactsOSMIO ::
  Foldable t =>
  t (OSM -> IO OSM) -- ^ The function to execute on the XML that is read.
  -> FilePath -- ^ The OSM file to read.
  -> FilePath -- ^ The OSM file to write.
  -> IO ()
interactsOSMIO =
  interactOSMIO . sumM

-- | Reads a OSM file, executes the given function on the XML, then writes the given file.
interactOSM' ::
  (OSM -> OSM) -- ^ The function to execute on the XML that is read.
  -> SysConfigList -- ^ The options for reading the OSM file.
  -> FilePath -- ^ The OSM file to read.
  -> SysConfigList -- ^ The options for writing the OSM file.
  -> FilePath -- ^ The OSM file to write.
  -> IO ()
interactOSM' f =
  interactOSMIO' (return . f)

-- | Reads a OSM file, executes the given functions on the XML, then writes the given file.
interactsOSM' ::
  Foldable t =>
  t (OSM -> OSM) -- ^ The functions to execute on the XML that is read.
  -> SysConfigList -- ^ The options for reading the OSM file.
  -> FilePath -- ^ The OSM file to read.
  -> SysConfigList -- ^ The options for writing the OSM file.
  -> FilePath -- ^ The OSM file to write.
  -> IO ()
interactsOSM' =
  interactOSM' . sum'

-- | Reads a OSM file removing whitespace, executes the given function on the XML, then writes the given file with indentation.
interactOSM ::
  (OSM -> OSM) -- ^ The function to execute on the XML that is read.
  -> FilePath -- ^ The OSM file to read.
  -> FilePath -- ^ The OSM file to write.
  -> IO ()
interactOSM f =
  interactOSMIO (return . f)

-- | Reads a OSM file removing whitespace, executes the given functions on the XML, then writes the given file with indentation.
interactsOSM ::
  Foldable t =>
  t (OSM -> OSM) -- ^ The function to execute on the XML that is read.
  -> FilePath -- ^ The OSM file to read.
  -> FilePath -- ^ The OSM file to write.
  -> IO ()
interactsOSM =
  interactOSM . sum'

-- not exported

sum' ::
  Foldable t =>
  t (a -> a)
  -> a
  -> a
sum' =
  appEndo . foldMap Endo

sumM ::
  (Monad m, Foldable t) =>
  t (a -> m a)
  -> a
  -> m a
sumM =
  foldr (>=>) return
