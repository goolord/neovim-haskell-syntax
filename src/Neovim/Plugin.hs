{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- Template Haskell is used to remove a lot of manual boiler-plate from
-- declaring the functions you want to export.
module Neovim.Plugin
    ( plugin
    ) where

import Neovim

import Neovim.Plugin.Syntax
import Neovim.API.Text

plugin :: Neovim () NeovimPlugin
plugin = do
    -- randomPluginState <- randomNumbers
    namespace <- NameSpace 
      <$> nvim_create_namespace "haskellKeyword"
      <*> nvim_create_namespace "haskellPragma"
      <*> nvim_create_namespace "haskellVariable"
      <*> nvim_create_namespace "haskellSymbol"
      <*> nvim_create_namespace "haskellConstructor"
      <*> nvim_create_namespace "haskellOperator"
      <*> nvim_create_namespace "haskellChar"
      <*> nvim_create_namespace "haskellString"
      <*> nvim_create_namespace "haskellInteger"
      <*> nvim_create_namespace "haskellRational"
      <*> nvim_create_namespace "haskellComment"
    wrapPlugin Plugin
        { environment = namespace
        , exports =
            [ $(function' 'nvimTokenizeHaskell) Async
            , $(autocmd 'nvimTokenizeHaskell)
                "TextChanged,TexChangedI" 
                Async
                ( AutocmdOptions 
                    { acmdPattern = "*"
                    , acmdNested = False
                    , acmdGroup = Nothing
                    }
                )
            ]
        }


