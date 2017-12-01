{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Builder
  ( build
  , Output (..)
  ) where

import qualified Algorithms.NaturalSort as NS
import           Conduit
import           Control.Exception
import           Control.Lens.Operators
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Crypto.Hash.Conduit
import           Crypto.Hash.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.UUID as UUID
import           Data.UUID.V4
import qualified Filesystem.Path as FS
import qualified Filesystem.Path.CurrentOS as FS
import           ModuleMap
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import           Prelude hiding (FilePath)
import           SimpleConfig
import qualified System.IO
import           System.IO.Temp
import           Text.Feed.Import
import           Text.Feed.Query
import qualified Text.Printf as PF
import           Text.XML
import           Text.XML.Cursor
import           Turtle hiding (find, parent, sort, sortBy)

data Output = OutputConfig
            | OutputKernel
            | OutputFileSystem
            | OutputISO
            deriving (Eq, Ord)

build :: Setup -> S.Set Output -> FilePath -> IO ()
build setup output outDir = do
  cacheDir <- (FS.</> "oplwork") <$> pwd
  testdir outDir >>= (\exists -> if exists then rmtree outDir else return ())
  mktree outDir
  mktree cacheDir

  withSystemTempDirectory "oplstuff" $ \wd -> do
    let workDir = fromString wd
    config <- mkConfig setup workDir
    mToolchain <- if output `containsAny` [OutputKernel, OutputFileSystem, OutputISO]
                  then do Just <$> setupToolchain (machine setup) (showT cacheDir)
                  else return Nothing
                                            
    kernel <- if output `containsAny` [OutputKernel, OutputISO]
              then do
                (kernelPath, kernelVersion) <- getLatestKernelSources cacheDir
                sequence ((\tc -> buildKernel tc config kernelPath kernelVersion workDir) <$> mToolchain)
              else return Nothing

    fs <- if OutputFileSystem `S.member` output
          then return Nothing
          else return Nothing

    iso <- if OutputISO `S.member` output
           then return Nothing
           else return Nothing

    let outputsToSave = catMaybes [ Just $ ConfigWrap config
                                  , KernelWrap <$> kernel
                                  , FileSystemWrap <$> fs
                                  , ISOWrap <$> iso
                                  ]

    mapM_ (saveOutput outDir) outputsToSave

  where
    saveOutput :: FilePath -> OutputWrapper -> IO ()
    saveOutput outDir (KernelWrap (Kernel kernelPath)) = do
      let kernelOut = outDir FS.</> "kernel"
      mkdir kernelOut
      cptree kernelPath kernelOut
    saveOutput outDir (ConfigWrap (Config configPath)) = cp configPath (outDir FS.</> FS.filename configPath)
    saveOutput outDir (FileSystemWrap (FileSystem fsPath)) = cptree fsPath outDir
    saveOutput outDir (ISOWrap (ISO isoPath)) = cp isoPath outDir

    containsAny :: (Ord a) => S.Set a -> [a] -> Bool
    containsAny s as = elem True ((`S.member` s) <$> as)

    mkConfig :: Setup -> FilePath -> IO Config
    mkConfig setup workDir = do
      let configPath = workDir FS.</> "config"
      writeTextFile configPath (showKernelConfig $ mkKernelConfig setup)
      return $ Config configPath

data OutputWrapper = KernelWrap Kernel
                   | ConfigWrap Config
                   | FileSystemWrap FileSystem
                   | ISOWrap ISO

newtype Kernel = Kernel { getKernel :: FilePath }
newtype Config = Config { getConfig :: FilePath }
newtype FileSystem = FileSystem { getFileSystem :: FilePath }
newtype ISO = ISO { getISO :: FilePath }
newtype Toolchain = Toolchain { getToolchain :: FilePath }
newtype KernelSrc = KernelSrc { getKernelSrc :: FilePath }
newtype KernelVersion = KernelVersion { getKernelVersion :: T.Text }
    
buildKernel :: Toolchain
            -> Config
            -> KernelSrc
            -> KernelVersion
            -> FilePath
            -> IO Kernel
buildKernel
  (Toolchain toolchainPath)
  (Config configPath)
  (KernelSrc kernelSrc)
  (KernelVersion kernelVersion)
  workDir
  = do
  let kernelDir = toolchainPath FS.</> "kernel"
      kernelOutDir = workDir FS.</> "kernelOut"
      kernelWorkDir = workDir FS.</> "kernelWorkDir"
  mkdir kernelOutDir
  mkdir kernelWorkDir
  view $ inshell ("git archive --format=tar --remote=" <> showT kernelSrc <> " " <> kernelVersion <> " | (cd " <> showT kernelWorkDir <> " && tar xf -)") empty
  cp configPath (kernelWorkDir FS.</> FS.filename configPath)
  view $ inshell ("systemd-nspawn -D " <> showT toolchainPath
                   <> " --bind=" <> showT kernelWorkDir <> ":/linux"
                   <> " --bind=" <> showT kernelOutDir <> ":/kernelOutDir"
                   <> " --ephemeral"
                   <> " /bin/bash -c \"" <> buildCmds <> "\""
                 ) empty
  return $ Kernel kernelOutDir
  where
    buildCmds = "cd /linux && ./scripts/kconfig/merge_config.sh -n ./config && make all && INSTALL_PATH=/kernelOutDir make install"



showT :: FilePath -> T.Text
showT = T.pack . FS.encodeString

data Stage3 = Version T.Text
            | Latest

setupToolchain :: Machine -> T.Text -> IO Toolchain
setupToolchain machine baseDir = do
  (link, digest) <- getStage3Link machine Latest
  tarballLocation <- downloadTarball baseDir link digest
  toolchain <- extractTarball tarballLocation
  return $ Toolchain toolchain

newtype FileDigest = FileDigest { getDigest :: String } deriving (Show, Eq)

getStage3Link :: Machine -> Stage3 -> IO (T.Text, FileDigest)
getStage3Link machine stage3 = do
  let (baseArch, arch) = matchArch machine
      baseUrl = getBaseUrl baseArch
  tarballUrl <- (baseUrl <>) <$> getTarballUrl baseUrl arch stage3
  digest <- getDigest tarballUrl
  return (tarballUrl, digest)

  where
    matchArch :: Machine -> (T.Text, T.Text)
    matchArch m
      | m == XPS13 ||
        m == MacbookAir2012 
        = ("amd64", "amd64")
      | otherwise = ("x86", "i686")

    getTarballUrl base arch Latest = do
      latestInfoReq <- parseRequest $ T.unpack (base <> "latest-stage3-" <> arch <> ".txt")
      latestInfo <- T.decodeUtf8 . LBS.toStrict . getResponseBody <$> httpLBS latestInfoReq
      return $ (head . T.words . head . drop 2 . T.lines) latestInfo
    getTarballUrl _ arch (Version v) =
      return $ v <> "/stage3-" <> arch <> "-" <> v <> ".tar.bz2"

    getDigest tarballUrl = do
      req <- parseRequest $ T.unpack $ tarballUrl <> ".DIGESTS"
      digest <- T.decodeUtf8 . LBS.toStrict . getResponseBody <$> httpLBS req
      let checksum = (T.unpack . head . T.words . head . drop 1 . T.lines) digest
      return $ FileDigest checksum

    getBaseUrl baseArch = "http://distfiles.gentoo.org/releases/" <> baseArch <> "/autobuilds/"

extractTarball :: FilePath -> IO FilePath
extractTarball downloadedName = do
  let tarball = showT downloadedName
      outDir = FS.dropExtensions downloadedName
  isExtracted <- testdir outDir
  unless isExtracted $ do
    handle (\(SomeException _)-> rmdir outDir) $ do
      mktree outDir
      print "extracting toolchain"
      view $ inshell ( "tar xvjfp " <> tarball
                       <> " -C " <> (showT outDir)
                       <> " --xattrs --numeric-owner"
                       <> " --exclude=dev"
                     ) empty

      view $ inshell ("systemd-nspawn -D " <> (showT outDir)
                      <> " /bin/bash -c \"" <> script <> "\"") empty

  return outDir

  where
    script = " emerge-webrsync"
             <> " && emerge -o sys-kernel/gentoo-sources"
             <> " && emerge --quiet app-arch/lz4"
  

downloadTarball :: T.Text -> T.Text -> FileDigest -> IO FilePath
downloadTarball baseDir url digest = do
  let prefix = FS.fromText (baseDir <> "/toolchains/")
      filename = FS.filename $ FS.fromText url
      downloadedName = prefix FS.</> filename
      extractedName = prefix FS.</> (FS.dropExtensions filename)
  mktree prefix
  isExtracted <- testpath extractedName
  unless isExtracted $ do
    isDownloaded <- isAlreadyDownloaded downloadedName digest
    unless isDownloaded $ do
      stage3Req <- parseRequest $ T.unpack url
      print "Downloading Toolchain"
      runConduitRes $ httpSource stage3Req (\r -> do
                                               let total = (read . T.unpack . T.decodeUtf8 <$>) . listToMaybe $ getResponseHeader "Content-Length" r
                                               getResponseBody r .| printProgress total
                                           )
        .| sinkFile (FS.encodeString downloadedName)
      print "Finished Downloading Toolchain"
                            
  return downloadedName

  where
    isAlreadyDownloaded downloadedPath givenDigest = do
      exists <- testfile downloadedPath
      if exists
        then do
        print "Verifiying downloaded tarball"
        calcDigest <- (FileDigest . show) <$> (hashFile (FS.encodeString downloadedPath) :: IO (Digest SHA512))
        return (calcDigest == givenDigest)
        else return False
      
printProgress :: Maybe Int -> Conduit BS.ByteString (ResourceT IO) BS.ByteString
printProgress total =
  loop total 0
  where
    message :: Maybe Int -> Int -> String
    message Nothing downloaded = "downloaded : " ++ show downloaded
    message (Just t) downloaded =
      let percent = fromIntegral downloaded / fromIntegral t * 100
      in "complete : " ++ PF.printf "%.2f" (percent :: Double) ++ "%"

    loop total downloaded = await >>= maybe (return ())
                            (\bs -> do
                                let downloaded' = downloaded + BS.length bs
                                liftIO $ putStr $ "\r" ++ message total downloaded'
                                yield bs
                                loop total downloaded')

linuxKernelGit :: T.Text
linuxKernelGit = "https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git"

getLatestStableKernelVersion :: IO T.Text
getLatestStableKernelVersion = do
  tagsBlob <- strict $ inshell getRemoteTags empty
  let allVersions = T.lines tagsBlob <&> ((!! 2) . T.splitOn "/")
      allStableVersions = filter (not . T.isInfixOf "rc") allVersions
      latest = sortBy NS.compare allStableVersions & last
  return latest
  where
    getRemoteTags = "git ls-remote --tags --refs " <> linuxKernelGit

getLatestKernelSources :: FilePath -> IO (KernelSrc, KernelVersion)
getLatestKernelSources baseDir = do
  latestKernelVersion <- getLatestStableKernelVersion
  initKernelRepo
  fetchKernelVersion latestKernelVersion
  return (KernelSrc kernelDir, KernelVersion latestKernelVersion)

  where
    fetchKernelVersion version = do
      runInDir kernelDir $ do
        res <- shellStrictWithErr ("git rev-parse " <> version) empty
        case res of
          (ExitSuccess, _, _) -> return ()
          _ -> view $ inshell ("git fetch --depth=1 origin tag " <> version) empty

    initKernelRepo = do
      shouldSkip <- testdir kernelDir
      unless shouldSkip $ do
        view (inshell ("git init " <> (showT kernelDir)) empty)
        runInDir kernelDir $ do
          view $ inshell ("git remote add origin " <> linuxKernelGit) empty

    kernelDir = baseDir FS.</> "linux"

        
runInDir :: FilePath -> IO a -> IO a 
runInDir dest f = bracket (stepIn dest) stepOut (\_ -> f) 
  where
    stepIn dest = do
      currentDir <- pwd
      cd dest
      return currentDir

    stepOut dest = cd dest
