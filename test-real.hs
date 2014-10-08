{-# Language ExistentialQuantification #-}
import           Control.Applicative
import           Data.List (sort)
import qualified Data.Map as DM
import           Data.Monoid
import           PanHandler
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
  -- Arbitrary stability

  ("enoughStableInlines", T $
   \i -> stable (doc (fromList [Plain i])) ==> label (show (count i)) True)]

  --("enoughStableBlocks", T $
  -- \b -> stable (doc (fromList b)) ==> True),

  --("enoughStableDocs", T $
  -- \d -> stable d ==> True)]

  -- noUnwrap

t2 =  [("noUnwrapIgnoresMissing", T $
   \i cs as -> noUnwrap (i, filter (/= "unwrap") cs, as) == Nothing),

  ("noUnwrapFindsUnwrap", T $
   \i pre post as -> noUnwrap (i, pre ++ ["unwrap"] ++ post, as) /= Nothing),

  ("noUnwrapRemovesUnwrap", T $
   \i pre post as -> let p x = (i, pre ++ x, as)
                      in noUnwrap (p ("unwrap":post)) == Just (p post)),

  -- bAttrs

  ("bAttrsSkipsNonCode", T $
   \b -> not (bIsCode b) ==> bAttrs b == Nothing),

  ("bAttrsGetsAttrs", T $
   \as s -> bAttrs (CodeBlock as s) == Just as),

  -- bUnwrap

  ("bUnwrapMakesDivs", T $
   \i c c' a s -> let at x = (i, c ++ x, a)
                      p (Div a' _) = a' == at c'
                      p _          = False
                   in p (bUnwrap (CodeBlock (at ("unwrap":c')) s))),

  ("pandocNormalises", T $
   \b -> stable b ==> True)]{-,

  {- readMarkDown and writeMarkdown aren't quite inverse, so we can't say:

     bUnwrap (CodeBlock _ (writeDoc . doc . fromList bs)) == Div _ bs

     Instead, we send both sides through a read/write cycle to normalise them.
   -}
  ("bUnwrapInterpretsCode", T $
   \b' i c c' a -> let b    = normalize b'
                       d    = doc . fromList
                       cb   = CodeBlock (at ("unwrap":c')) (writeDoc (d b))
                       at x = (i, c ++ x, a)
                       lhs  = normalise (d [bUnwrap cb])
                       rhs  = normalise (d [Div (at c') b])
                    in stable (d b) ==> lhs == rhs),

  ("bUnwrapLeavesWrapped", T $
   \i cs as s -> let cb = CodeBlock (i, filter (/= "unwrap") cs, as) s
                  in bUnwrap cb == cb),

  ("bUnwrapLeavesRest", T $
   \b -> (bUnwrap b == b) || ((noUnwrap <$> bAttrs b) /= Nothing)),

  -- transform

  ("transformRemovesUnwrapBlocks", T $
   not . ("unwrap" `elem`) . query (maybe [] classes . bAttrs) . transform),

  ("transformRemovesUnwrapInlines", T $
   not . ("unwrap" `elem`) . query (maybe [] classes . iAttrs) . transform)]
-}

testWith f = let go name test = (putStrLn ("Testing " ++ name) >> f test >>)
              in DM.foldWithKey go (return ()) tests

test = testWith quickCheck

bIsCode :: Block -> Bool
bIsCode (CodeBlock _ _) = True
bIsCode _               = False

iIsCode :: Inline -> Bool
iIsCode (Code _ _) = True
iIsCode _          = False

classes :: Attr -> [String]
classes (_, cs, _) = cs

normalise :: Pandoc -> Pandoc
normalise = dCycle . dCycle

dCycle :: Pandoc -> Pandoc
dCycle = readDoc . writeDoc

stable :: Pandoc -> Bool
stable d = dCycle (dCycle d) == dCycle (dCycle (dCycle d))

count d = bCount d + iCount d

bCount x = let Sum n = query ((\b -> Sum 1) :: Block  -> Sum Int) x in n
iCount x = let Sum n = query ((\i -> Sum 1) :: Inline -> Sum Int) x in n

-- Arbitrary instances (sized, to prevent recursive trees exploding)

arb :: Arbitrary a => Gen a
arb =  arbitrary

accum :: Int -> (Int -> Gen a) -> Gen [a]
accum 0 _ = pure []
accum n a = do n' <- choose (1, n)
               x  <- a (abs n')
               xs <- accum (abs (n - n')) a
               return (x:xs)

accumP :: (Int -> Gen a) -> (Int -> Gen b) -> Int -> Gen ([a], [b])
accumP l r n = do n' <- choose (0, n)
                  l' <- accum n'     l
                  r' <- accum (n-n') r
                  pure (l', r')

down :: Int -> Int
down n = n `div` 10

aA :: Int -> Gen Attr
aA n = do let n' = down n
          identity    <- arb
          (pre, post) <- accumP (const arb) (const arb) n
          attrs       <- accum n (const arb)
          unwrap      <- frequency [(10, pure []),
                                    (1,  pure ["unwrap"])]
          return (identity, pre ++ unwrap ++ post, attrs)

aB :: Int -> Gen Block
aB n | n < 2 = elements [HorizontalRule, Null]
aB n = do n' <- choose (0, n)
          let acc = accum (n-1)
          oneof [BlockQuote     <$> acc aB,
                 BulletList     <$> acc (`accum` aB),
                 DefinitionList <$> acc (accumP aI (`accum` aB)),
                 Para           <$> acc aI,
                 Plain          <$> acc aI,
                 CodeBlock      <$> aA n'          <*> aS n',
                 Div            <$> aA n'          <*> acc aB,
                 RawBlock       <$> arb            <*> arb,
                 OrderedList    <$> arb            <*> acc (`accum` aB),
                 Header         <$> choose (0, 10) <*> aA n' <*> acc aI,
                 aT (n-1)]

diffs = let diffs' n [] = []
            diffs' n (x:xs) = (x - n) : diffs' x xs
         in diffs' 0

dist n 0 = pure []
dist n m = do ns' <- vectorOf m (choose (0, n))
              let ns = sort ns'
              pure (diffs ns)

aT n = do ns <- dist n 5
          let (a:b:c:d:e:[]) = map accum ns
              as   = a $ aI
              bs   = b $ const arb
              cs   = c $ const (choose (0, 1))
              ds   = d $ (`accum` aB)
              es   = e $ (`accum` (`accum` aB))
          Table <$> as <*> bs <*> cs <*> ds <*> es

aC :: Int -> Gen Citation
aC n = do (l, r) <- accumP aI aI (n-1)
          Citation <$> arb <*> pure l <*> pure r <*> arb <*> choose (0, 100)
                                                         <*> choose (0, 100)

aD :: Int -> Gen Pandoc
aD n = doc . fromList <$> accum n aB

aI :: Int -> Gen Inline
aI n | n < 2 = elements [Space, LineBreak]
aI n = do l <- choose (0, n)
          let m    = n - l
              acc  = accum (n-1)
              accL = accum l
              accM = accum m
          oneof [Emph        <$> acc aI,
                 --Note        <$> acc aB,
                 SmallCaps   <$> acc aI,
                 Str         <$> arb,
                 Strong      <$> acc aI,
                 Strikeout   <$> acc aI,
                 Superscript <$> acc aI,
                 Subscript   <$> acc aI,
                 Cite        <$> accL aC <*> accM aI,
                 Code        <$> aA n    <*> arb,
                 Image       <$> acc aI  <*> arb,
                 Link        <$> acc aI  <*> arb,
                 Math        <$> arb     <*> arb,
                 Quoted      <$> arb     <*> acc aI,
                 RawInline   <$> arb     <*> arb,
                 Span        <$> aA n    <*> acc aI]

aS :: Int -> Gen String
aS n = oneof [arb, writeDoc <$> aD n]

instance Arbitrary Alignment where
  arbitrary = elements [AlignLeft, AlignRight, AlignCenter, AlignDefault]

instance Arbitrary Block where
  arbitrary = sized aB
  shrink b = case b of
                  HorizontalRule       -> []
                  Null                 -> []
                  BlockQuote xs        -> BlockQuote     <$> shrink xs
                  BulletList xs        -> BulletList     <$> shrink xs
                  DefinitionList xs    -> DefinitionList <$> shrink xs
                  Para xs              -> Para           <$> shrink xs
                  Plain xs             -> Plain          <$> shrink xs
                  CodeBlock xs y       -> CodeBlock      <$> shrink xs <*> shrink y
                  Div xs ys            -> Div            <$> shrink xs <*> shrink ys
                  RawBlock x y         -> RawBlock x     <$> shrink y
                  OrderedList x ys     -> OrderedList x  <$> shrink ys
                  Header x y zs        -> Header x y     <$> shrink zs
                  Table as bs cs ds es -> []

instance Arbitrary Citation where
  arbitrary = sized aC
  shrink (Citation a bs cs d e f) = Citation <$> shrink a
                                             <*> shrink bs
                                             <*> shrink cs
                                             <*> [d]
                                             <*> shrink e
                                             <*> shrink f

instance Arbitrary CitationMode where
  arbitrary = elements [AuthorInText, SuppressAuthor, NormalCitation]

instance Arbitrary Format where
  arbitrary = Format <$> arb

instance Arbitrary Inline where
  arbitrary = sized aI
  shrink i = case i of
                  Emph xs        -> Emph        <$> shrink xs
                  Note xs        -> Note        <$> shrink xs
                  SmallCaps xs   -> SmallCaps   <$> shrink xs
                  Str s          -> Str         <$> shrink s
                  Strong xs      -> Strong      <$> shrink xs
                  Strikeout xs   -> Strikeout   <$> shrink xs
                  Superscript xs -> Superscript <$> shrink xs
                  Subscript xs   -> Subscript   <$> shrink xs
                  Cite xs ys     -> Cite        <$> shrink xs <*> shrink ys
                  Code xs y      -> Code        <$> shrink xs <*> shrink y
                  Image xs y     -> Image       <$> shrink xs <*> shrink y
                  Link xs y      -> Link        <$> shrink xs <*> shrink y
                  Math x y       -> Math x      <$> shrink y
                  Quoted s xs    -> Quoted      <$> shrink s  <*> shrink xs
                  RawInline x y  -> RawInline   <$> shrink x  <*> shrink y
                  Span xs ys     -> Span        <$> shrink xs <*> shrink ys
                  _              -> []

instance Arbitrary ListNumberDelim where
  arbitrary = elements [DefaultDelim, Period, OneParen, TwoParens]

instance Arbitrary ListNumberStyle where
  arbitrary = elements [DefaultStyle, Example, Decimal, LowerRoman, UpperRoman,
                        LowerAlpha, UpperAlpha]

instance Arbitrary MathType where
  arbitrary = elements [DisplayMath, InlineMath]

instance Arbitrary Pandoc where
  arbitrary = sized aD
  shrink (Pandoc x ys) = Pandoc x <$> shrink ys

instance Arbitrary QuoteType where
  arbitrary = elements [SingleQuote, DoubleQuote]
