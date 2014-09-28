{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module MOS6502.TinyMicro.Video where

import MOS6502.TinyMicro.DCM

import Language.KansasLava
import Language.KansasLava.VHDL
import Language.Netlist.GenVHDL
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.Arcade
import Hardware.KansasLava.VGA.Driver

import Data.Sized.Unsigned as Unsigned
import Data.Bits

-- import System.FilePath
-- import System.Directory

type VidX = U5
type VidY = U5

type VAddr = U10 -- TODO: compute this from VidX + VidY
type VPixel = U4

toVAddr :: (Clock clk)
        => Signal clk (VidX, VidY)
        -> Signal clk VAddr
toVAddr = uncurry appendS . unpack

drive32x32 :: (Clock clk)
           => (VPixel -> (U4, U4, U4))
           -> VGADriverIn clk VPixel () ()
           -> (Signal clk Bool, VGADriverOut clk (W VidX) (W VidY) U4 U4 U4)
drive32x32 palette VGADriverIn{..} =
    (newPixel,
     VGADriverOut{ vgaOutX = x'
                 , vgaOutY = y'
                 , ..})
  where
    VGADriverOut{..} = driveVGA vga640x480at60 (VGADriverIn r g b)

    (validX, x) = unpackEnabled vgaOutX
    (validY, y) = unpackEnabled vgaOutY

    inFieldH = validX .&&. x `betweenCO` (192, 448)
    inFieldV = validY .&&. y `betweenCO` (112, 368)
    inField = inFieldH .&&. inFieldV

    x' = mapEnabled (\x -> signed $ (x - 192) `shiftR` 3) vgaOutX
    y' = mapEnabled (\y -> signed $ (y - 112) `shiftR` 3) vgaOutY

    newPixel = bitNot vgaOutClkPhase .&&. inField .&&. ((x - 192) .&. 0x7 .==. 0)

    rgb = mux inField (pureS (maxBound, minBound, minBound),
                       funMap (Just . palette) vgaInR)
    (r, g, b) = unpack rgb

betweenCO :: (Ord a, Rep a) => Signal clk a -> (a, a) -> Signal clk Bool
x `betweenCO` (lo, hi) = pureS lo .<=. x .&&. x .<. pureS hi

vgaFB :: forall clk. (Clock clk)
      => (VPixel -> (U4, U4, U4))
      -> Signal clk VPixel
      -> (Signal clk (Enabled (VidX, VidY)), VGADriverOut clk (W VidX) (W VidY) U4 U4 U4)
vgaFB palette pixel = (packEnabled newPixel pos, vga)
  where
    (newPixel, vga@VGADriverOut{..}) = drive32x32 palette VGADriverIn{..}

    vgaInR = pixel'
    vgaInG = pureS ()
    vgaInB = pureS ()

    x = let (en, v) = unpackEnabled vgaOutX in mux en (0, v)
    y = let (en, v) = unpackEnabled vgaOutY in mux en (0, v)

    pos :: Signal clk (VidX, VidY)
    pos = pack (x, y)

    pixel' = runRTL $ do
        prevNewPixel <- newReg False
        prevNewPixel := newPixel

        p <- newReg 0
        WHEN (reg prevNewPixel) $ p := pixel
        return (var p)

palette :: VPixel -> (U4, U4, U4)
palette 0x0 = (0x0, 0x0, 0x0) -- Black
palette 0x1 = (0xf, 0xf, 0xf) -- White
palette 0x2 = (0x8, 0x0, 0x0) -- Red
palette 0x3 = (0xa, 0xf, 0xe) -- Cyan
palette 0x4 = (0xc, 0x4, 0xc) -- Purple
palette 0x5 = (0x0, 0xc, 0x5) -- Green
palette 0x6 = (0x0, 0x0, 0xa) -- Blue
palette 0x7 = (0xe, 0xe, 0x7) -- Yellow
palette 0x8 = (0xd, 0x8, 0x5) -- Orange
palette 0x9 = (0x6, 0x4, 0x0) -- Brown
palette 0xa = (0xf, 0x7, 0x7) -- Light red
palette 0xb = (0x3, 0x3, 0x3) -- Dark gray
palette 0xc = (0x7, 0x7, 0x7) -- Gray
palette 0xd = (0xa, 0xf, 0x6) -- Light green
palette 0xe = (0x0, 0x8, 0xf) -- Light blue
palette 0xf = (0xb, 0xb, 0xb) -- Light gray

synthesize :: Model -> String -> Fabric () -> IO (String, String)
synthesize model modName bench = do
    kleg <- reifyFabric $ do
        theClk clock
        wing_init
        bench

    mod <- netlistCircuit modName kleg
    let mod' = dcm50MHz clock mod
        vhdl = genVHDL mod' ["work.lava.all", "work.all"]

    ucf <- toUCF model kleg

    return (vhdl, ucf)
  where
    clock = "CLK_50MHZ"