{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Portage
  ( Use (..)
  , PackageConfig (..)
  , PortageConfig (..)
  , MakeConf (..)
  , mkPortageConf
  , makeConfToFile
  ) where


import           Data.Monoid ((<>))
import qualified Data.Text as T
import           SimpleConfig

data Use = Use Bool T.Text

data PackageConfig = PackageConfig { use :: [Use] }

data MakeConf = MakeConf { cFlags :: T.Text
                         , cxxFlags :: T.Text
                         , cHost :: T.Text
                         , cpuFlagsX86 :: T.Text
                         , portDir :: T.Text
                         , distDir :: T.Text
                         , pkgDir :: T.Text
                         , linguas :: T.Text
                         , l10n :: T.Text
                         , globalUse :: [Use]
                         --, videoCards :: T.Text
                         --, inputDevices :: T.Text
                         --, globalAcceptKeywords :: T.Text
                         }

data PortageConfig = PortageConfig { makeConf :: MakeConf
                                   , packageConfigs :: [PackageConfig]
                                   }

mkPortageConf :: Setup -> PortageConfig
mkPortageConf setup = PortageConfig { makeConf = mkMakeConf setup
                                    , packageConfigs = []
                                    }

mkMakeConf :: Setup -> MakeConf
mkMakeConf Setup{..} =
  MakeConf { cFlags = getMArch machine <> " -O2 -pipe"
           , cxxFlags = "${CFLAGS}"
           , cHost = getCHost machine
           , cpuFlagsX86 = getCPUFlags machine
           , portDir = "/usr/portage"
           , distDir = "${PORTDIR}/distfiles"
           , pkgDir = "${PORTDIR}/packages"
           , linguas = "en_US"
           , l10n = "en-US"
           , globalUse = [Use True "systemd"]
           }

quote :: T.Text -> T.Text
quote t = "\"" <> t <> "\""

showUse :: Use -> T.Text
showUse (Use b t) = onOff b <> t
  where onOff True = ""
        onOff False = "-"

makeConfToFile :: MakeConf -> T.Text
makeConfToFile MakeConf {..} = T.unlines [ "CFLAGS=" <> quote cFlags
                                         , "CXXFLAGS=" <> quote cxxFlags
                                         , "CHOST=" <> quote cHost
                                         , "CPU_FLAGS_X86=" <> quote cpuFlagsX86
                                         , "PORTDIR=" <> quote portDir
                                         , "DISTDIR=" <> quote distDir
                                         , "PKGDIR=" <> quote pkgDir
                                         , "LINGUAS=" <> quote linguas
                                         , "L10N=" <> quote l10n
                                         , "USE=" <> quote (T.intercalate "" (showUse <$> globalUse))
                                         ]

getMArch :: Machine -> T.Text
getMArch m | m `elem` [ MacbookAir2012
                      , MacbookPro2016
                      , XPS13
                      ] = "-march=broadwell"
           | otherwise = ""

getCHost :: Machine -> T.Text
getCHost m | m `elem` [ MacbookAir2012
                      , MacbookPro2016
                      , XPS13
                      ] = "x86_64-pc-linux-gnu"
           | otherwise = "x86_64-pc-linux-gnu"

getCPUFlags :: Machine -> T.Text
getCPUFlags m | m `elem` [ MacbookAir2012
                         , MacbookPro2016
                         , XPS13
                         ] = "aes avx mmx mmxext pclmul popcnt sse sse2 sse3 sse4_1 sse4_2 ssse3"
              | otherwise = ""
