module SimpleConfig
  ( Setup (..)
  , Machine (..)
  , Motherboard
  , Processor (..)
  , VideoCard (..)
  , WindowManager (..)
  , Applications (..)
  , InitSystem (..)
  , BootOptions (..)
  , Optimizations (..)
  ) where

data Setup = Setup { machine :: Machine
                   , wm :: WindowManager
                   , apps :: [Applications]
                   , initSystem :: InitSystem
                   , bootOptions :: [BootOptions]
                   , optimizations :: [Optimizations]
                   }

data Machine = MacbookAir2012
             | MacbookPro2016
             | GalagoPro2014
             | VirtualBox
             | XPS13
             | Custom Motherboard Processor VideoCard

data Motherboard

data Processor = Intel
               | AMDCPU

data VideoCard = Nvidia
               | AMDGPU

data WindowManager = None
                   | Sway
                   | I3

data Applications = Chrome
                  | Firefox
                  | Urxvt
                  | Dmenu

data InitSystem = Systemd

data BootOptions = BootToRam
                 | ReadOnlyRoot


data Optimizations = Performance
                   | Minimal
                   | LowMemoryFootprint
                   | SmallSize
                   | PowerSaving
                   | Security
                   | LowLatency
