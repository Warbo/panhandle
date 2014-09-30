module Main where
import PanHandler
main = getContents >>= processDoc >>= putStr
