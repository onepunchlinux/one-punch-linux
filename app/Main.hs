module Main where

import SimpleConfig
import ModuleMap
import qualified Data.Text.IO as T

main :: IO ()
main = do
  let setup = Setup { machine = XPS13
                    , wm = Sway
                    , apps = []
                    , initSystem = Systemd
                    , bootOptions = []
                    , optimizations = []
                    }
  let config = mkKernelConfig setup
  T.writeFile "config" (showKernelConfig config)
  return ()
