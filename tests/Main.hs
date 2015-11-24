module Main where

import Generators
import Data.Char
import Data.Maybe
import PanHandle
import LSC
import Text.JSON.Generic
import Text.Pandoc
import qualified Test.LazySmallCheck2012 as LSC
import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main = do defaultMain $ testGroup "All tests" [
              testProperty "noUnwrap ignores no .wrap" noUnwrapIgnore
            , testProperty "noUnwrap removes .wrap"    noUnwrapRemove
            , testProperty "Block IDs remain"          bWrappedId
            , testProperty "Block classes remain"      bWrappedCls
            , testProperty "Block attributes remain"   bWrappedAttr
            --, testProperty "No spurious divs"          bUnwrapped
            , lazyProperty "Pandoc JSON parses"        canParseJson
            ]

noUnwrapIgnore x ys zs = let ys' = filter (/= "unwrap") ys
                          in isNothing (noUnwrap (x, ys', zs))

noUnwrapRemove x pre post zs =
  let cls                 = stripUnwrap (pre ++ post)
      Just (x', ys', zs') = noUnwrap (x, addUnwrap pre post, zs)
   in and [x == x',
           all (`elem` ys') cls,
           all (`elem` cls) ys',
           all (`elem` zs') zs,
           all (`elem` zs)  zs']

bWrappedId id pre post as b = not (null id) ==>
  case bUnwrap' (CodeBlock (id, addUnwrap pre post, as) b) of
       [Div (id', _, _) _] -> id' == id
       _                   -> False

bWrappedCls id pre post as b = any (not . null) (stripUnwrap (pre ++ post)) ==>
  let cls = stripUnwrap (pre ++ post)
   in case bUnwrap' (CodeBlock (id, addUnwrap pre post, as) b) of
           [Div (_, cls', _) _] -> all (`elem` cls) cls' && all (`elem` cls') cls
           _                    -> False

bWrappedAttr id pre post as b = any (\(x, y) -> not (null x || null y)) as ==>
  case bUnwrap' (CodeBlock (id, addUnwrap pre post, as) b) of
       [Div (_, _, as') _] -> all (`elem` as) as' && all (`elem` as') as
       _                   -> False

canParseJson :: Pandoc -> Bool
canParseJson p = decodeJSON (encodeJSON p) == p

bUnwrapped :: Pandoc -> Bool
bUnwrapped d =
  let j = encodeJSON d
   in case readJson def j of
           Pandoc _ bs -> bs == bUnwrap' (CodeBlock ("", ["unwrap"], []) j)

-- Helpers
stripUnwrap = filter (/= "unwrap")

addUnwrap xs ys = stripUnwrap xs ++ ["unwrap"] ++ stripUnwrap ys
