{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Generators
import           Data.Aeson
import qualified Data.ByteString.Lazy as LB
import           Data.Char
import           Data.Maybe
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Enc
import           Debug.Trace
import           PanHandle
import           Text.Pandoc.Definition
import           Text.Pandoc.Arbitrary
import           Test.QuickCheck
import           Test.Tasty (defaultMain, testGroup)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
           testProperty "noUnwrap ignores no .wrap" noUnwrapIgnore
         , testProperty "noUnwrap removes .wrap"    noUnwrapRemove
         , testProperty "Block IDs remain"          bWrappedId
         , testProperty "Block classes remain"      bWrappedCls
         , testProperty "Block attributes remain"   bWrappedAttr
         , testProperty "No spurious divs"          bUnwrapped
         ]

noUnwrapIgnore x ys zs = let ys' = stripUnwrap ys
                          in isNothing (noUnwrap (x, ys', zs))

noUnwrapRemove x pre post zs =
  let cls                 = stripUnwrap (pre ++ post)
      Just (x', ys', zs') = noUnwrap (x, addUnwrap pre post, zs)
   in and [x == x',
           all (`elem` ys') cls,
           all (`elem` cls) ys',
           all (`elem` zs') zs,
           all (`elem` zs)  zs']

bWrappedId id pre post as b = nonEmpty id ==>
  case bUnwrap' (CodeBlock (id, addUnwrap pre post, as) contents) of
       [Div (id', _, _) _] -> id' == id
       _                   -> False
  where contents = encodeDoc (b :: Pandoc)

bWrappedCls id pre post as b = any nonEmpty (stripUnwrap (pre ++ post)) ==>
  let cls = stripUnwrap (pre ++ post)
      contents = encodeDoc (b :: Pandoc)
   in case bUnwrap' (CodeBlock (id, addUnwrap pre post, as) contents) of
           [Div (_, cls', _) _] -> all (`elem` cls) cls' && all (`elem` cls') cls
           _                    -> False

bWrappedAttr id pre post as b = any neitherEmpty as && id /= "" ==>
    case bUnwrap' (CodeBlock (id, addUnwrap pre post, as) contents) of
         [Div (_, _, as') _] -> all (`elem` as) as' && all (`elem` as') as
         _                   -> False
  where neitherEmpty (x, y) = not (Text.null x || Text.null y)
        contents = encodeDoc (b :: Pandoc)

encodeDoc = Enc.decodeUtf8 . LB.toStrict . Data.Aeson.encode

canParseAeson :: Pandoc -> Bool
canParseAeson p = Data.Aeson.decode (Data.Aeson.encode p) == Just p

bUnwrapped :: Pandoc -> Bool
bUnwrapped d@(Pandoc _ bs) =
  bs == bUnwrap' (CodeBlock ("", ["unwrap"], []) (encodeDoc d))

-- Helpers
stripUnwrap = filter (/= "unwrap")

addUnwrap xs ys = stripUnwrap xs ++ ["unwrap"] ++ stripUnwrap ys

nonEmpty = not . Text.null
