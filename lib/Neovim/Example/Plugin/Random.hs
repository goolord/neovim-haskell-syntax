{-# LANGUAGE TemplateHaskell #-}
module Neovim.Example.Plugin.Random where

import Neovim
import Neovim.API.String
import Neovim.API.TH
import Data.Either
import GHC.SyntaxHighlighter
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data NVIMLOC = NVIMLOC
  { nvimLocLine :: Int64
  , nvimLocCol_start :: Int64
  , nvimLocCol_end :: Int64
  } deriving Show

convertLoc :: Loc -> NVIMLOC
convertLoc (Loc lineStart colStart lineEnd colEnd) = 
  NVIMLOC 
    (fromIntegral $ lineStart -1)
    (fromIntegral colStart) 
    (fromIntegral colEnd)

stringTok :: Neovim env ()
stringTok = do
  buff <- nvim_get_current_buf'
  ns <- nvim_create_namespace' "haskellString2"
  lc <- buffer_line_count' buff
  contents <- nvim_buf_get_lines' buff 0 lc True
  let tokens :: [(Token, Loc)]
      tokens = fromMaybe (error $ "could not tokenize " <> show contents) $ tokenizeHaskellLoc $ T.pack $ unlines contents
      tokens' = filter (\(x, _) -> x == StringTok) tokens
      tokenLocs = fmap (convertLoc . snd) tokens'
  mapM_ (highlight buff ns) tokenLocs
  -- mapM_ (delete buff) tokenLocs
  where
  highlight bf ns' (NVIMLOC ls cs ce) = nvim_buf_add_highlight' bf ns' "String" ls cs ce
  delete bf (NVIMLOC ls _ _) = nvim_buf_set_lines' bf (ls) (ls+1) False [mempty]

test :: Neovim env ()
test = do 
  buff <- nvim_get_current_buf'
  lines <- nvim_buf_get_lines' buff 0 1 False
  vim_report_error' $ unlines lines
