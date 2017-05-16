module Main where
import PanHandle
import Text.Pandoc.JSON

main = toJSONFilter transform
