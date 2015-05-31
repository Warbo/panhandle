module Main where
import PanHandler
import Text.Pandoc.JSON

main = toJSONFilter transform
