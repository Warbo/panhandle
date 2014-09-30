module Main where
import Control.Applicative
import PanHandler

main = processDoc <$> getContents >>= putStr
