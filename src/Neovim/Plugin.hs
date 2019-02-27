{-# LANGUAGE TemplateHaskell #-}
-- Template Haskell is used to remove a lot of manual boiler-plate from
-- declaring the functions you want to export.
module Neovim.Plugin
    ( plugin
    ) where

import Neovim

import Neovim.Plugin.Syntax

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
    -- randomPluginState <- randomNumbers
    wrapPlugin Plugin
        { environment = ()
        , exports =
            [ $(function' 'stringTok) Async
            , $(function' 'test) Async
            ]
        }
