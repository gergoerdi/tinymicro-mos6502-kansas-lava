{-# LANGUAGE RecordWildCards #-}
module MOS6502.TinyMicro.DCM
       ( dcm50MHz
       , dcm80MHz
       ) where

import Language.Netlist.AST

-- TODO: Move this to some library... kansas-lava-papilio, maybe?

dcm :: Ident -> Ident -> Module -> Module
dcm dcmName newClock Module{..} = Module name inputs outputs [] decls
  where
    name = module_name
    inputs = (rawClock, Nothing) : filter ((/= newClock) . fst) module_inputs
    outputs = module_outputs
    decls = routing : dcmInst : module_decls

    rawClock = "CLK_32MHZ"

    routing = NetDecl newClock Nothing Nothing

    dcmInst = InstDecl ("work." ++ dcmName) ("inst_" ++ dcmName) []
              [ ("clkin_in",        ExprVar rawClock)
              , ("clkin_ibufg_out", open)
              ]
              [ ("clkfx_out",       ExprVar newClock)
              , ("clk0_out",        open)
              ]

    open = ExprVar "open"

-- | Use 50MHz DCM to replace clock signal
dcm50MHz :: Ident -> Module -> Module
dcm50MHz = dcm "dcm_32_to_50p35"

-- | Use 80MHz DCM to replace clock signal
dcm80MHz :: Ident -> Module -> Module
dcm80MHz = dcm "dcm_32_to_80"
