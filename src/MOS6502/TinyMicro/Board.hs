{-# LANGUAGE RecordWildCards #-}
module MOS6502.TinyMicro.Board where

import MOS6502.Types
import MOS6502.CPU
import MOS6502.TinyMicro.Video

import Language.KansasLava
import Language.KansasLava.Signal (shallowMapS)
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
import Hardware.KansasLava.VGA

import Data.Sized.Unsigned
import Data.Sized.Matrix (Matrix, Size)
import qualified Data.Sized.Matrix as Matrix

import Data.Bits
import Data.List (transpose)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import Control.Applicative

-- Memory layout:
--
-- 0x0000 - 0x3FFF: 16K RAM
-- 0x0200 - 0x03FF: 1K VRAM (on top of RAM)
-- 0xE000 - 0xFFFF: 8K ROM

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

program :: IO BS.ByteString
program = BS.readFile "example/FullscreenLogo.obj"

board :: BS.ByteString -> Fabric ()
board prog = do
    vga . encodeVGA $ vgaOut
  where
    vram = boardCircuit (programToROM 0xF000 prog)

    (vgaPos, VGADriverOut{..}) = vgaFB palette vgaD
    vgaD = syncRead vram (toVAddr $ enabledVal vgaPos)
    -- vgaD = pureS 0x4

boardCircuit :: (ROMAddr -> Byte) -> Signal CLK (VAddr -> VPixel)
boardCircuit romContents = vram
  where
    (CPUOut{..}, _cpuDebug) = cpu CPUIn{..}

    cpuIRQ = high
    cpuNMI = high

    mpipe :: Signal CLK (Pipe RAMAddr Byte)
    mpipe = packEnabled (isEnabled cpuMemW .&. isRAM) $
            pack (unsigned cpuMemA, enabledVal cpuMemW)
    ram = writeMemory mpipe
    cpuWait = low

    ramR = syncRead ram (unsigned cpuMemA)

    vpipe :: Signal CLK (Pipe VAddr U4)
    vpipe = packEnabled (isEnabled cpuMemW .&&. isVideo) $
            pack (unsigned (cpuMemA - 0x0200), unsigned $ enabledVal cpuMemW)
    vram = writeMemory vpipe

    romR = rom (unsigned cpuMemA) (Just . romContents)

    isVideo = 0x0200 .<=. cpuMemA .&&. cpuMemA .<. 0x0400
    isRAM = cpuMemA .<. 0x4000
    isROM = delay $ 0xF000 .<=. cpuMemA

    cpuMemR = mux isROM (ramR, romR)
