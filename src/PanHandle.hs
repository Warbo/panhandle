module PanHandler where

import Control.Applicative
import Data.Maybe
import Text.Pandoc
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

bCode :: Block -> Maybe String
bCode (CodeBlock _ c) = Just c
bCode _               = Nothing

blocks :: Pandoc -> [Block]
blocks (Pandoc _ bs) = bs

bUnwrap :: Block -> Block
bUnwrap b = let bs      = blocks <$> (readJSON def <$> bCode b)
                bs'     = map bUnwrap <$> bs
                wrapped = Div    <$> bNoUnwrap b <*> bs'
             in fromMaybe b wrapped

-- Inline-level functions

iAttrs :: Inline -> Maybe Attr
iAttrs (Code as _) = Just as
iAttrs _           = Nothing


iNoUnwrap :: Inline -> Maybe Attr
iNoUnwrap b = iAttrs b >>= noUnwrap

iCode :: Inline -> Maybe String
iCode (Code _ c) = Just c
iCode _          = Nothing

inlines :: Pandoc -> [Inline]
inlines (Pandoc _ bs) = let f (Plain x) = x
                            f (Para  x) = x
                            f _         = []
                         in concatMap f bs

iUnwrap :: Inline -> Inline
iUnwrap i = let is      = inlines <$> (readJSON def <$> iCode i)
                is'     = map iUnwrap <$> is
                wrapped = Span    <$> iNoUnwrap i <*> is'
             in fromMaybe i wrapped

transform :: Pandoc -> Pandoc
transform = topDown iUnwrap . topDown bUnwrap
