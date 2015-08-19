{-# LANGUAGE RecordWildCards #-}
module TinyMicro.Machine where

import Language.KansasLava
import Language.KansasLava.VHDL
import Language.Netlist.GenVHDL

import Development.KansasLava.Shake
import Development.KansasLava.Shake.Xilinx
import Development.Shake
import Development.Shake.FilePath
import System.Console.GetOpt
import System.Exit

import qualified Data.ByteString as BS
import Data.Char (toLower)

import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver
import Hardware.KansasLava.VGA
import Hardware.KansasLava.Rate
import qualified TinyMicro.Video as Video
import qualified TinyMicro.Board.MOS6502 as Board

synthesize :: BS.ByteString -> IO [(String, String)]
synthesize prog = do
    boardKLEG <- reifyFabric $ do
        theClk cpuClock
        wing_init
        let vpipe = Board.board prog
            (we, w) = unpackEnabled vpipe
            (wAddr, wData) = unpack w
        outStdLogic "VIDEO_WE" we
        outStdLogicVector "VIDEO_W_ADDR" wAddr
        outStdLogicVector "VIDEO_W_DATA" wData

    boardMod <- netlistCircuit "MainBoard" boardKLEG
    let boardVHDL = genVHDL boardMod ["work.lava.all", "work.all"]

    videoKLEG <- reifyFabric $ do
        theClk vidClock
        wing_init
        rData <- inStdLogicVector "VIDEO_R_DATA"
        let (rAddr, VGADriverOut{..}) = Video.vgaFB Video.palette rData
        outStdLogicVector "VIDEO_R_ADDR" $ enabledVal rAddr
        vga $ encodeVGA vgaOut
    videoMod <- netlistCircuit "Video" videoKLEG
    let videoVHDL = genVHDL videoMod ["work.lava.all", "work.all"]

    return [("MainBoard", boardVHDL), ("Video", videoVHDL)]
  where
    cpuClock = "CLK_CPU"
    vidClock = "CLK_40MHZ"
