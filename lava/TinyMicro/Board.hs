{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TinyMicro.Board where

import MOS6502.Utils
import TinyMicro.Video
import Language.KansasLava
import Data.Sized.Unsigned
import Data.Bits
import qualified Data.ByteString as BS

import Language.KansasLava.Signal
import Control.Applicative

-- Memory layout:
--
-- 0x0000 - 0x3FFF: 16K x 8 RAM
-- 0x0200 - 0x05FF:  1K x 4 VRAM (mirrored in RAM)
-- 0xF000 - 0xFFFF:  4K x 8 ROM

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
             -> Signal clk (Pipe VAddr Nybble)
boardCircuit cpu romContents = vpipe
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

    romR = rom (unsigned csMemA) (Just . romContents)

    isVideo = 0x0200 .<=. csMemA .&&. csMemA .<. 0x0600
    isRAM = csMemA .<. 0x4000
    isROM = 0xF000 .<=. csMemA

    csMemR = forceDefined 0 $
             memoryMapping [ (isRAM, ramR)
                           , (isROM, romR)
                           ]

forceDefined :: (Clock clk, Rep a) => a -> Signal clk a -> Signal clk a
forceDefined def = shallowMapS (fmap (optX . (<|> Just def) . unX))
