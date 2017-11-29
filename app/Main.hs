{-# LANGUAGE OverloadedStrings #-}
module Main where

import SimpleConfig
import Builder
import qualified Data.Text.IO as T
import qualified Data.Set as S

main :: IO ()
main = do
  let setup = Setup { machine = XPS13
                    , wm = Sway
                    , apps = []
                    , initSystem = Systemd
                    , bootOptions = []
                    , optimizations = []
                    }
  build setup (S.fromList [OutputConfig, OutputKernel]) "./out/"
  return ()
