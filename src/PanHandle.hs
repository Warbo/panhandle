{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module PanHandle where

import Control.Applicative
import Data.Aeson (eitherDecodeStrict)
import Data.Data
import Data.Generics
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Text.Pandoc.Definition
import Text.Pandoc.Generic (topDown)
import Text.Pandoc.JSON
import Text.Pandoc.Walk (walk, query)

-- Generic (Inline and Block) functions

noUnwrap :: Attr -> Maybe Attr
noUnwrap (x, ys, zs) = if "unwrap" `elem` ys
                          then Just (x, filter (/= "unwrap") ys, zs)
                          else Nothing

-- Block-level functions

bAttrs :: Block -> Maybe Attr
bAttrs (CodeBlock as _) = Just as
bAttrs _                = Nothing

bNoUnwrap :: Block -> Maybe Attr
bNoUnwrap b = bAttrs b >>= noUnwrap

bCode :: Block -> Maybe Text
bCode (CodeBlock _ c) = Just c
bCode _               = Nothing

blocks :: Pandoc -> [Block]
blocks (Pandoc _ bs) = bs

readJson :: Text -> Pandoc
readJson s = case eitherDecodeStrict (encodeUtf8 s) of
                  Left  x -> error (show x)
                  Right x -> x

bUnwrap' :: Block -> [Block]
bUnwrap' b = case b of
  CodeBlock (i, cs, as) x | "unwrap" `elem` cs ->
    let content = bUnwrap (blocks (readJson x))
     in case (i, filter (/= "unwrap") cs, as) of
             ("", [],  []) -> content
             (_,  cs', _)  -> [Div (i, cs', as) content]
  _                                            -> gmapM (mkM bUnwrap') b

bUnwrap :: [Block] -> [Block]
bUnwrap = concatMap bUnwrap'

-- Inline-level functions

iAttrs :: Inline -> Maybe Attr
iAttrs (Code as _) = Just as
iAttrs _           = Nothing

iNoUnwrap :: Inline -> Maybe Attr
iNoUnwrap b = iAttrs b >>= noUnwrap

iCode :: Inline -> Maybe Text
iCode (Code _ c) = Just c
iCode _          = Nothing

inlines :: Pandoc -> [Inline]
inlines (Pandoc _ bs) = let f (Plain x) = x
                            f (Para  x) = x
                            f _         = []
                         in concatMap f bs

iUnwrap :: Inline -> Inline
iUnwrap i = let is      = inlines <$> (readJson <$> iCode i)
                is'     = map iUnwrap <$> is
                wrapped = Span    <$> iNoUnwrap i <*> is'
             in fromMaybe i wrapped

transform :: Pandoc -> Pandoc
transform = topDown iUnwrap . topDown bUnwrap

panhandleMain = toJSONFilter transform
