{-# LANGUAGE RecordWildCards #-}
module TinyMicro.Board.MOS6502 (board) where

import TinyMicro.Board
import TinyMicro.Video
import qualified MOS6502.CPU as MOS6502

import Language.KansasLava
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
import Hardware.KansasLava.VGA
import Hardware.KansasLava.Rate

import Data.Sized.Ix
import qualified Data.ByteString as BS
import Data.Monoid

prepareImage :: BS.ByteString -> BS.ByteString
prepareImage bs = mconcat [bs, padding, vectors]
  where
    padding = BS.replicate (16 * 256 - BS.length bs - 6) 0
    vectors = BS.pack [0x00, 0x00, 0x00, 0xF0, 0x00, 0x00] -- NMI, Reset, IRQ

mos6502 :: (Clock clk) => CPUSocketIn clk -> CPUSocketOut clk
mos6502 = fromCPU . fst . MOS6502.cpu . toCPU
  where
    toCPU :: (Clock clk) => CPUSocketIn clk -> MOS6502.CPUIn clk
    toCPU CPUSocketIn{..} = MOS6502.CPUIn{..}
      where
        cpuMemR = csMemR
        cpuNMI = high
        cpuIRQ = high
        cpuWait = low

    fromCPU :: (Clock clk) => MOS6502.CPUOut clk -> CPUSocketOut clk
    fromCPU MOS6502.CPUOut{..} = CPUSocketOut{..}
      where
        csMemA = cpuMemA
        csMemW = cpuMemW

board :: (Clock clk) => BS.ByteString -> Signal clk (Pipe VAddr Nybble)
board prog = boardCircuit mos6502 (programToROM 0xF000 $ prepareImage prog)
