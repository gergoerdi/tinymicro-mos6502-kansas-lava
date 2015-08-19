{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Development.KansasLava.Shake
import Development.KansasLava.Shake.Xilinx
import Development.Shake
import Development.Shake.FilePath
import System.Console.GetOpt
import System.Exit

import qualified Data.ByteString as BS
import Data.Char (toLower)

import Hardware.KansasLava.Boards.Papilio.Arcade (Model(..))
import qualified TinyMicro.Machine as Machine

data Flag = ImageFile FilePath
          | XilinxRoot FilePath
          | PapilioModel String

mkXilinxConfig :: [Flag] -> IO (XilinxConfig, Model)
mkXilinxConfig flags = do
    xilinxRoot <- case [path | XilinxRoot path <- flags] of
        [] -> return "/home/cactus/prog/fpga/Xilinx/14.2/ISE_DS/ISE/bin/lin64"
        [path] -> return path
        _ -> do
            putStrLn "Conflicting flags: --xilinx"
            exitFailure

    target <- case [model | PapilioModel model <- flags] of
        [] -> do
            putStrLn "Defaulting to Papilio Pro"
            return "pro"
        [target] ->
            return target
        _ -> do
            putStrLn "Conflicting flags: --papilio"
            exitFailure

    (xilinxTarget, papilioModel) <- case map toLower target of
        "one" -> return (papilioOne, PapilioOne)
        "pro" -> return (papilioPro, PapilioPro)
        _ -> do
            putStrLn $ unwords ["Unknown target platform in --papilio:", show target]
            exitFailure

    return (XilinxConfig{..}, papilioModel)
  where
    papilioOne = XilinxTarget "Spartan3" "xccs500e" "-5" "vq100"
    papilioPro = XilinxTarget "Spartan6" "xc6slx9" "-2" "tqg144"

main :: IO ()
main = do
    shakeArgsWith shakeOptions{ shakeFiles = "build/.shake" } flags $ \flags targets -> do
        prog <- case [fileName | ImageFile fileName <- flags] of
            [fileName] -> BS.readFile fileName
            _ -> do
                putStrLn $ unwords ["Missing flag --image, using", defaultImage]
                BS.readFile defaultImage
        genVHDLs <- Machine.synthesize prog

        (xilinxConfig, model) <- mkXilinxConfig flags

        return $ Just $ do
            want $ if null targets then ["build" </> modName <.> "bit"] else targets

            lavaRules "build" genVHDLs

            let copyFrom dir out = do
                    alwaysRerun
                    copyFileChanged (replaceDirectory out dir) out

            "build" </> "ipcore_dir//*.xco" %> copyFrom "xco"
            "build" </> "src//*.vhdl" %> copyFrom "vhdl"
            "build" </> "src//*.ucf" %> copyFrom "ucf"

            let ucf = "TinyMicro6502.ucf" -- TODO: dispatch on model
            let srcs = ["DualRAM.vhdl", "TinyMicro6502.vhdl", ucf]
            let allSrcs = concat [ [ "gensrc" </> modName <.> "vhdl" | (modName, _) <- genVHDLs ]
                                 , [ "gensrc" </> "lava-prelude.vhdl" ]
                                 , [ "src" </> src | src <- srcs ]
                                 ]
                ipcores = ["clockman.xco"]
            xilinxRules xilinxConfig "build" modName allSrcs ipcores
  where
    flags = [ Option [] ["image"] (ReqArg (Right . ImageFile) "filename") "ROM image file"
            , Option [] ["xilinx"] (ReqArg (Right . XilinxRoot) "path") "Path to Xilinx toolchain"
            , Option [] ["papilio"] (ReqArg (Right . PapilioModel) "model") "Target Papilio model (One/Pro)"
            ]

    modName = "TinyMicro6502"

    defaultImage = "example/demoscene.obj"
