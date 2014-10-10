{-# Language ExistentialQuantification #-}
import           Control.Applicative
import           Data.List
import qualified Data.Map as DM
import           Data.Monoid
import           PanHandler
import           System.IO.Unsafe
import           System.Timeout
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.Shared
import           Text.Pandoc.Walk (walk, query)
import           Tests.Arbitrary

data T = forall a. Testable a => T a

instance Testable T where
  property (T x) = property x

tests = DM.fromList [

  ("noUnwrap ignores missing class", T $ \i cs as ->
   noUnwrap (i, filter (/= "unwrap") cs, as) == Nothing),

  ("noUnwrap finds unwrap class", T $ \i pre post as ->
   noUnwrap (i, pre ++ ["unwrap"] ++ post, as) /= Nothing),

  ("noUnwrap removes unwrap class", T $ \i pre post as ->
   let p x = (i, pre ++ x, as)
    in noUnwrap (p ("unwrap":post)) == Just (p post)),

  ("bAttrs skips non-code blocks", T $ \b ->
   not (bIsCode b) ==> bAttrs b == Nothing),

  ("bAttrs gets attrs", T $ \as s ->
   bAttrs (CodeBlock as s) == Just as),

  ("bUnwrap makes divs", T $ \i c c' a s ->
   let at x         = (i, c ++ x, a)
       p (Div a' _) = a' == at c'
       p _          = False
    in p (bUnwrap (CodeBlock (at ("unwrap":c')) s))),

  ("bUnwrap interprets code blocks", T $ \b i c c' a ->
   let b'   = writeJSON def . doc . fromList $ b
       cb   = CodeBlock (at ("unwrap":c')) b'
       at x = (i, c ++ x, a)
    in bUnwrap cb == Div (at c') b),

  ("bUnwrap works recursively", T $ \b1 b2 b3 i c c' a ->
   let at x = (i, c ++ x ++ c', a)
       cb1  = CodeBlock (at ["unwrap"]) s1
       cb2  = CodeBlock (at ["unwrap"]) s2
       s1   = d $ b1 ++ [cb2] ++ b2
       s2   = d $ b3
       d    = writeJSON def . doc . fromList
       d1   = Div (at []) (b1 ++ [d2] ++ b2)
       d2   = Div (at []) b3
    in bUnwrap cb1 == d1),

  ("bUnwrap leaves non-unwrap code blocks", T $ \i cs as s ->
   let cb = CodeBlock (i, filter (/= "unwrap") cs, as) s
    in bUnwrap cb == cb),

  ("bUnwrap leaves non-code blocks", T $ \b ->
   bUnwrap b == b || (noUnwrap <$> bAttrs b) /= Nothing),

  ("iUnwrap makes spans", T $ \i c c' a s ->
   let at x          = (i, c ++ x, a)
       p (Span a' _) = a' == at c'
       p _           = False
    in p (iUnwrap (Code (at ("unwrap":c')) s))),

  ("iUnwrap interprets code lines", T $ \i d c c' a ->
   let i'   = writeJSON def . doc . fromList $ [Plain i]
       u    = Code (at ("unwrap":c')) i'
       at x = (d, c ++ x, a)
    in iUnwrap u == Span (at c') i),

  ("iUnwrap works recursively", T $ \i1 i2 i3 i c c' a ->
   let at x = (i, c ++ x ++ c', a)
       cl1  = Code (at ["unwrap"]) s1
       cl2  = Code (at ["unwrap"]) s2
       s1   = d $ i1 ++ [cl2] ++ i2
       s2   = d $ i3
       d is = writeJSON def . doc . fromList $ [Plain is]
       d1   = Span (at []) (i1 ++ [d2] ++ i2)
       d2   = Span (at []) i3
    in iUnwrap cl1 == d1),

  ("iUnwrap leaves non-unwrap code lines", T $ \i cs as s ->
   let c = Code (i, filter (/= "unwrap") cs, as) s
    in iUnwrap c == c),

  ("iUnwrap leaves non-code lines", T $ \i ->
   iUnwrap i == i || (noUnwrap <$> iAttrs i) /= Nothing),

  ("transform removes unwrap class from code blocks", T $
   not . ("unwrap" `elem`) . query (maybe [] classes . bAttrs) . transform),

  ("transform removes unwrap class from code lines", T $
   not . ("unwrap" `elem`) . query (maybe [] classes . iAttrs) . transform)]

bIsCode :: Block -> Bool
bIsCode (CodeBlock _ _) = True
bIsCode _               = False

iIsCode :: Inline -> Bool
iIsCode (Code _ _) = True
iIsCode _          = False

classes :: Attr -> [String]
classes (_, cs, _) = cs

testWith f = let go name test = (putStrLn ("Testing " ++ name) >> f test >>)
              in DM.foldWithKey go (return ()) tests

test = testWith quickCheck

main = test
