{-# LANGUAGE OverloadedStrings #-}

module File (mighty, progName) where

import Control.Applicative
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import Network.Web.HTTP
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import Network.TCPInfo
import Network.Web.Server
import Network.Web.Server.Basic
import Network.Web.URI
import System.Directory
import System.FilePath
import System.IO
import System.Posix.Files
import URLMap
import qualified Data.Map
import Data.IORef
import System.IO.Unsafe
import Data.Maybe
import Debug.Trace

progName :: String
progName = "Mighttpd"

progVersion :: String
progVersion = "0.4.0"

progNameVersion :: String
progNameVersion = progName ++ "/" ++ progVersion

----------------------------------------------------------------

-- mighty :: WebConfig -> URLMap -> Handle -> TCPInfo -> IO ()
mighty dict wcnf umap hdl tcpinfo = connection hdl (server dict) wcnf
--   let bcnf = BasicConfig { obtain = fileGet
--                          , info   = fileInfo
--                          , mapper = fileMapper umap
--                          , serverName = S.pack progNameVersion
--                          , tcpInfo = tcpinfo
--                          }
--  connection hdl (basicServer bcnf) wcnf

server dict (Just req)= do let res = uriPath $ reqURI req
                           a <- readIORef dict
                           trace (show req) $ trace (show a) $   
                             case reqMethod req of
                               GET -> do
                                    trace (show a) (return ())
                                    let msg = L.pack $ case Data.Map.lookup res a of
                                                              Just m -> m ++ (S.unpack res)
                                                              Nothing -> "empty: " ++ (S.unpack res)
                                    return $ makeResponse2 OK (Just msg) Nothing []
                               PUT -> do --let input = L.unpack $ fromJust $ reqBody req
                                        trace ("updated" ++ S.unpack res) (return ())    
                                        let newdict = Data.Map.insert res (L.unpack $ fromJust $ reqBody req) a
                                        writeIORef dict newdict
                                        trace ("updated" ++ show newdict) (return ())    
                                        r <- readIORef  dict
                                        trace ("updated ref" ++ show r) (return ())    
                                        return $ makeResponse2 OK (Just $ L.pack $ "updated" ++ S.unpack res ) Nothing []
                               _ ->      return $ makeResponse2 OK (Just $ L.pack "Fucked") Nothing []
  --let uri = reqURI req 

  
----------------------------------------------------------------

lookupFileMap :: URLMap -> URL -> Path
lookupFileMap [] _          = None
lookupFileMap ((from,to):xs) url
    | from `isPrefixOf` url = toPath to $ drop (length from) url
    | otherwise             = lookupFileMap xs url

toPath :: ConvInfo -> FilePath -> Path
toPath (CIFile dir)   restPath  = File $ dir </> restPath
toPath ci@(CICgi _ _) progParam = PathCGI $ CGI {
    progPath    = prog
    , scriptName  = scriptname
    , pathInfo    = path
    , queryString = query
    }
  where
    (progParam',query)  = break (== '?') progParam
    (prog',path)        = break (== '/') progParam'
    prog = progDir ci </> prog'
    scriptname = pathInURL ci </> prog'

fileMapper :: URLMap -> URI -> Path
fileMapper umap uri = fileMapper' (lookupFileMap umap url)
  where
    url = unEscapeString . S.unpack . toURLwoPort $ uri
    fileMapper' None                  = None
    fileMapper' cgi@(PathCGI _)       = cgi
    fileMapper' (File file)
      | hasTrailingPathSeparator file = File $ file </> "index.html"
      | otherwise                     = File file

fileGet :: FilePath -> Maybe (Integer,Integer) -> IO L.ByteString
fileGet file Nothing = openFile file ReadMode >>= L.hGetContents
fileGet file (Just (skip,len)) = do
    h <- openFile file ReadMode
    hSeek h AbsoluteSeek skip
    L.take (fromIntegral len) <$> L.hGetContents h

fileInfo = undefined
fileInfo :: FilePath -> IO (Maybe (Integer, UTCTime))
-- fileInfo file = do
--     doesFileExist file >>= 
--     if exist
--        then do
--          fs <- getFileStatus file
--          let size = fromIntegral . fileSize $ fs
--              mtime = posixSecondsToUTCTime . realToFrac . modificationTime $ fs
--          return $ Just (size, mtime)
--        else return Nothing
