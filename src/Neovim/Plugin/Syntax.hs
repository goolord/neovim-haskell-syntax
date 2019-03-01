{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror  #-}

module Neovim.Plugin.Syntax where

-- import Neovim
import Neovim.API.Text
import Neovim.API.TH
import Neovim.Context
import GHC.SyntaxHighlighter
import Data.Maybe
import Control.Arrow (first)
import qualified Data.Text as T

data NVIMLOC = NVIMLOC
  { nvimLocLine      :: Int64
  , nvimLocCol_start :: Int64
  , nvimLocCol_end   :: Int64
  , nvimLocNamspace  :: Int64
  , nvimLocToken     :: Token
  } deriving Show

convertLoc :: ((Token, Int64), Loc) -> NVIMLOC
convertLoc ((tok, ns), (Loc lineStart colStart _lineEnd colEnd)) = 
  NVIMLOC 
    (fromIntegral $ lineStart - 1)
    (fromIntegral $ colStart - 1) 
    (fromIntegral $ colEnd - 1)
    ns
    tok

encodeToken :: Token -> T.Text
encodeToken x = case x of
  KeywordTok     -> "Keyword"
  PragmaTok      -> "SpecialComment"
  SymbolTok      -> "Special"
  VariableTok    -> ""
  ConstructorTok -> "Type"
  OperatorTok    -> "Operator"
  CharTok        -> "Character"
  StringTok      -> "String"
  IntegerTok     -> "Number"
  RationalTok    -> "Float"
  CommentTok     -> "Comment"
  LabelTok       -> "Identifier"
  SpaceTok       -> ""
  OtherTok       -> ""

tokenFunc :: Token -> (NameSpace -> Int64)
tokenFunc x = case x of
  KeywordTok     -> haskellKeyword
  PragmaTok      -> haskellPragma
  SymbolTok      -> haskellVariable
  VariableTok    -> haskellSymbol
  ConstructorTok -> haskellConstructor
  OperatorTok    -> haskellOperator
  CharTok        -> haskellChar
  StringTok      -> haskellString
  IntegerTok     -> haskellInteger
  RationalTok    -> haskellRational
  CommentTok     -> haskellComment
  LabelTok       -> haskellLabel
  SpaceTok       -> const (-1)
  OtherTok       -> const (-1)

data NameSpace = NameSpace
  { haskellKeyword     :: Int64
  , haskellPragma      :: Int64
  , haskellVariable    :: Int64
  , haskellSymbol      :: Int64
  , haskellConstructor :: Int64
  , haskellOperator    :: Int64
  , haskellChar        :: Int64
  , haskellString      :: Int64
  , haskellInteger     :: Int64
  , haskellRational    :: Int64
  , haskellComment     :: Int64
  , haskellLabel       :: Int64
  }

tokenizeHaskell :: Neovim NameSpace ()
tokenizeHaskell = do
  buff <- nvim_get_current_buf'
  lc <- buffer_line_count' buff
  namespace <- ask
  contents <- nvim_buf_get_lines' buff 0 lc True
  let tokens :: [(Token, Loc)]
      tokens = fromMaybe 
                 (error $ "Could not tokenize: " <> show contents) $ 
                 tokenizeHaskellLoc $ T.unlines contents
      tokens' = fmap (first (\x -> (x, tokenFunc x namespace))) tokens
      tokenLocs = fmap convertLoc tokens'
  mapM_ (highlight buff) tokenLocs
  where
  highlight bf (NVIMLOC ls cs ce ns tok) = nvim_buf_add_highlight' bf ns (encodeToken tok) ls cs ce

