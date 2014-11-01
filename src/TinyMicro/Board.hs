{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TinyMicro.Board (board) where

import MOS6502.CPU
import TinyMicro.Video

import Language.KansasLava
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
import Hardware.KansasLava.VGA

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
  | addr == 0xFFFC = fromIntegral startingAddr
  | addr == 0xFFFD = fromIntegral (startingAddr `shiftR` 8)
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

board :: BS.ByteString -> Fabric ()
board prog = do
    vga . encodeVGA $ vgaOut
  where
    vram = boardCircuit (fromCPU . fst . cpu . toCPU) (programToROM 0xF000 prog)

    toCPU :: (Clock clk) => CPUSocketIn clk -> CPUIn clk
    toCPU CPUSocketIn{..} = CPUIn{..}
      where
        cpuMemR = csMemR
        cpuNMI = high
        cpuIRQ = high

        -- Slow down CPU 1024-fold
        cpuWait = runRTL $ do
            counter <- newReg (0 :: U10)
            counter := reg counter + 1
            return $ reg counter ./=. 0

    fromCPU :: (Clock clk) => CPUOut clk -> CPUSocketOut clk
    fromCPU CPUOut{..} = CPUSocketOut{..}
      where
        csMemA = cpuMemA
        csMemW = cpuMemW

    (vgaPos, VGADriverOut{..}) = vgaFB palette vgaD
    vgaD = syncRead vram (toVAddr $ enabledVal vgaPos)

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
