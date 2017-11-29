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
             deriving (Eq, Show)

data Motherboard = MB deriving (Eq, Show)

data Processor = Intel64
               | Intel32
               | AMD64
               | I686
               | I486
               | Arm7a
               | Arm6j
               | Arm4tl
               | Arm5tel
               deriving (Eq, Show)

data VideoCard = Nvidia
               | AMDGPU
               deriving (Eq, Show)

data WindowManager = NoWM
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
