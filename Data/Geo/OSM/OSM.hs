{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

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

import Text.XML.HXT.Arrow
import Text.XML.HXT.Extras
import Control.Monad
import Data.List
import Data.Geo.OSM.OSMChildren
import Data.Geo.OSM.Bound
import Data.Geo.OSM.Bounds
import Data.Geo.OSM.Accessor.Version
import Data.Geo.OSM.Accessor.Generator
import Data.Geo.OSM.Accessor.BoundOrs
import Data.Geo.OSM.Accessor.NodeWayRelations

-- | The @osm@ element of a OSM file, which is the root element.
data OSM = OSM String (Maybe String) (Maybe (Either Bound Bounds)) OSMChildren
  deriving Eq

instance XmlPickler OSM where
  xpickle = xpElem "osm" (xpWrap (\(version', generator', bound', nwr') -> osm version' generator' bound' nwr', \(OSM version' generator' bound' nwr') -> (version', generator', bound', nwr'))
              (xp4Tuple (xpAttr "version" xpText)
                        (xpOption (xpAttr "generator" xpText))
                        (xpOption (xpAlt (either (const 0) (const 1)) [xpWrap (Left, \(Left b) -> b) xpickle, xpWrap (Right, \(Right b) -> b) xpickle]))
                        xpickle))

instance Show OSM where
  show = showPickled []

instance Version OSM String where
  version (OSM x _ _ _) = x
  setVersion a (OSM _ b c d) = osm a b c d

instance Generator OSM where
  generator (OSM _ x _ _) = x
  setGenerator b (OSM a _  c d) = osm a b c d

instance BoundOrs OSM where
  boundOrs (OSM _ _ x _) n b bs = case x of Nothing -> n
                                            Just (Left b') -> b b'
                                            Just (Right b') -> bs b'
  setBoundOrs c (OSM a b _ d) = osm a b c d

instance NodeWayRelations OSM where
  nwrs (OSM _ _ _ x) = let t = const []
                       in foldOSMChildren t t t t t id x
  setNwrs d (OSM a b c _) = osm a b c (osmNodeWayRelation d)

-- | Constructs a osm with a version, bound or bounds, and node attributes and way or relation elements.
osm :: String -- ^ The @version@ attribute.
       -> Maybe String -- ^ The @generator@ attribute.
       -> Maybe (Either Bound Bounds) -- ^ The @bound@ or @bounds@ elements.
       -> OSMChildren -- ^ The child elements.
       -> OSM
osm = OSM

-- | Reads an OSM file into a list of @OSM@ values removing whitespace.
readOsmFile :: FilePath -> IO [OSM]
readOsmFile = runX . xunpickleDocument (xpickle :: PU OSM) [(a_remove_whitespace, v_1)]

-- | Reads 0 or more OSM files into a list of @OSM@ values removing whitespace.
readOsmFiles :: [FilePath] -> IO [OSM]
readOsmFiles = fmap join . mapM readOsmFile

-- | Reads a OSM file, executes the given function on the XML, then writes the given file.
interactOSMIO' :: Attributes -- ^ The options for reading the OSM file.
                  -> FilePath -- ^ The OSM file to read.
                  -> (OSM -> IO OSM) -- ^ The function to execute on the XML that is read.
                  -> Attributes -- ^ The options for writing the OSM file.
                  -> FilePath -- ^ The OSM file to write.
                  -> IO ()
interactOSMIO' froma from f toa to = runX (xunpickleDocument (xpickle :: PU OSM) froma from >>> arrIO f >>> xpickleDocument (xpickle :: PU OSM) toa to) >> return ()

-- | Reads a OSM file, executes the given functions on the XML, then writes the given file.
interactsOSMIO' :: Attributes -- ^ The options for reading the OSM file.
                   -> FilePath -- ^ The OSM file to read.
                   -> [OSM -> IO OSM] -- ^ The function to execute on the XML that is read.
                   -> Attributes -- ^ The options for writing the OSM file.
                   -> FilePath -- ^ The OSM file to write.
                   -> IO ()
interactsOSMIO' froma from = interactOSMIO' froma from . sumIO'

-- | Reads a OSM file removing whitespace, executes the given function on the XML, then writes the given file with indentation.
interactOSMIO :: FilePath -- ^ The OSM file to read.
                 -> (OSM -> IO OSM) -- ^ The function to execute on the XML that is read.
                 -> FilePath -- ^ The OSM file to write.
                 -> IO ()
interactOSMIO from f = interactOSMIO' [(a_remove_whitespace, v_1)] from f [(a_indent, v_1)]

-- | Reads a OSM file removing whitespace, executes the given functions on the XML, then writes the given file with indentation.
interactsOSMIO :: FilePath -- ^ The OSM file to read.
                  -> [OSM -> IO OSM] -- ^ The function to execute on the XML that is read.
                  -> FilePath -- ^ The OSM file to write.
                  -> IO ()
interactsOSMIO from = interactOSMIO from . sumIO'

-- | Reads a OSM file, executes the given function on the XML, then writes the given file.
interactOSM' :: Attributes -- ^ The options for reading the OSM file.
                -> FilePath -- ^ The OSM file to read.
                -> (OSM -> OSM) -- ^ The function to execute on the XML that is read.
                -> Attributes -- ^ The options for writing the OSM file.
                -> FilePath -- ^ The OSM file to write.
                -> IO ()
interactOSM' froma from f = interactOSMIO' froma from (return . f)

-- | Reads a OSM file, executes the given functions on the XML, then writes the given file.
interactsOSM' :: Attributes -- ^ The options for reading the OSM file.
                 -> FilePath -- ^ The OSM file to read.
                 -> [OSM -> OSM] -- ^ The functions to execute on the XML that is read.
                 -> Attributes -- ^ The options for writing the OSM file.
                 -> FilePath -- ^ The OSM file to write.
                 -> IO ()
interactsOSM' froma from = interactOSM' froma from . sum'

-- | Reads a OSM file removing whitespace, executes the given function on the XML, then writes the given file with indentation.
interactOSM :: FilePath -- ^ The OSM file to read.
               -> (OSM -> OSM) -- ^ The function to execute on the XML that is read.
               -> FilePath -- ^ The OSM file to write.
               -> IO ()
interactOSM from f = interactOSMIO from (return . f)

-- | Reads a OSM file removing whitespace, executes the given functions on the XML, then writes the given file with indentation.
interactsOSM :: FilePath -- ^ The OSM file to read.
                -> [OSM -> OSM] -- ^ The function to execute on the XML that is read.
                -> FilePath -- ^ The OSM file to write.
                -> IO ()
interactsOSM from = interactOSM from . sum'

-- not exported

sum' :: [a -> a] -> a -> a
sum' = foldl' (.) id

sumIO' :: (Monad m) => [a -> m a] -> a -> m a
sumIO' = foldl' (>=>) return
