module Main where
import PanHandler

main = getContents >>= putStr . processDoc
