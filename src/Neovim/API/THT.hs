{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{- |
Module      :  Neovim.API.String
Description :  String based API
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0
Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Note that this module is completely generated. If you're reading this on
hackage, the actual functions of this module may be different from what is
available to you. All the functions in this module depend on the neovim version
that was used when this package was compiled.
-}
module Neovim.API.THT where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Language.Haskell.TH
import Neovim.API.TH
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

-- | Default type mappings for the requested API.
typeMap :: Map String (Q Type)
typeMap = Map.fromList
    [ ("Boolean"   , [t|Bool|])
    , ("Integer"   , [t|Int64|])
    , ("Float"     , [t|Double|])
    , ("Array"     , [t|[Object]|])
    , ("Dictionary", [t|Map Object Object|])
    , ("String"    , [t|Text|])
    , ("void"      , [t|()|])
    ]

-- instance NvimObject a => NvimObject (Vector a) where
  -- toObject a = ObjectArray $ V.toList $ toObject <$> a
