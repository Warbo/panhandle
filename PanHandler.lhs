> module PanHandler where

> import Control.Applicative
> import Data.List
> import Data.Maybe
> import System.Directory
> import System.IO.Temp
> import System.Process
> import Text.Pandoc
> import Text.Pandoc.Builder (singleton, toList)
> import Text.Pandoc.Generic (topDown)
> import Text.Pandoc.Walk (walk, query)

> handleB :: Block -> Block
> handleB (CodeBlock as s)
>       |  Just as' <- unwrappable as = let Pandoc _ bs = readDoc s
>                                        in Div as' bs
> handleB x = walk handleB x

> handleI :: Inline -> Inline
> handleI (Code as s)
>       |  Just as' <- unwrappable as = let inlines = query singleton (readDoc s)
>                                        in Span as' (toList inlines)
> handleI x = walk handleI x

> unwrappable :: Attr -> Maybe Attr
> unwrappable (x, ys, z) = case partition (== "unwrap") ys of
>                               (_:_, ys') -> Just (x, ys', z)
>                               _          -> Nothing

Use Pandoc to parse, traverse and pretty-print our documents

> transform :: Pandoc -> Pandoc
> transform = topDown handleI . topDown handleB

> readDoc :: String -> Pandoc
> readDoc = readMarkdown def

> writeDoc :: Pandoc -> String
> writeDoc = writeMarkdown def

> processDoc :: String -> String
> processDoc = writeDoc . transform . readDoc
