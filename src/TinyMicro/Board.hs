{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TinyMicro.Board where

import TinyMicro.Video
import Language.KansasLava
import Data.Sized.Unsigned
import Data.Bits
import qualified Data.ByteString as BS

-- Memory layout:
--
-- 0x0000 - 0x3FFF: 16K RAM
-- 0x0200 - 0x03FF: 1K VRAM (on top of RAM)
-- 0xE000 - 0xFFFF: 8K ROM

type Byte = U8
type Addr = U16
type RAMAddr = U14
type ROMAddr = U13

programToROM :: Addr -> BS.ByteString -> (ROMAddr -> Byte)
programToROM startingAddr bs addr
  | offset < 0 = 0
  | offset >= BS.length bs = 0
  | otherwise = fromIntegral $ BS.index bs offset
  where
    offset = fromIntegral $ addr - fromIntegral startingAddr

data CPUSocketIn clk = CPUSocketIn
    { csMemR :: Signal clk Byte
    }

data CPUSocketOut clk = CPUSocketOut
    { csMemA :: Signal clk Addr
    , csMemW :: Signal clk (Enabled Byte)
    }

boardCircuit :: forall clk. (Clock clk)
             => (CPUSocketIn clk -> CPUSocketOut clk)
             -> (ROMAddr -> Byte)
             -> Signal clk (VAddr -> VPixel)
boardCircuit cpu romContents = vram
  where
    CPUSocketOut{..} = cpu CPUSocketIn{..}

    mpipe :: Signal clk (Pipe RAMAddr Byte)
    mpipe = packEnabled (isEnabled csMemW .&. isRAM) $
            pack (unsigned csMemA, enabledVal csMemW)
    ram = writeMemory mpipe

    ramR = syncRead ram (unsigned csMemA)

    vpipe :: Signal clk (Pipe VAddr U4)
    vpipe = packEnabled (isEnabled csMemW .&&. isVideo) $
            pack (unsigned (csMemA - 0x0200), unsigned $ enabledVal csMemW)
    vram = writeMemory vpipe

    romR = rom (unsigned csMemA) (Just . romContents)

    isVideo = 0x0200 .<=. csMemA .&&. csMemA .<. 0x0600
    isRAM = csMemA .<. 0x4000
    isROM = delay $ 0xF000 .<=. csMemA

    csMemR = mux isROM (ramR, romR)
