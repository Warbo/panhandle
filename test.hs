import Control.Applicative
import Data.Monoid
import PanHandler
import Test.QuickCheck
import Test.QuickCheck.Gen
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Walk (walk, query)

-- Tests

pNoUnwrapRemain d = query p (transform d) == mempty
  where p (CodeBlock (_, cs, _) _) = Any ("unwrap" `elem` cs)
        p _                        = mempty

pNoInlineRemain d = query p (transform d) == mempty
  where p (Code (_, cs, _) _) = Any ("unwrap" `elem` cs)
        p _                   = mempty

test = let m & p = m >> quickCheck p
        in quickCheck pNoUnwrapRemain &
                      pNoInlineRemain

-- Arbitrary instances (sized, to prevent recursive trees exploding)

arb :: Arbitrary a => Gen a
arb =  arbitrary

aA :: Int -> Gen Attr
aA n = do let n' = n `div` 2
          identity    <- arb
          (pre, post) <- aP (aL (const arb))
                            (aL (const arb)) n'
          attrs       <-     aL (const arb)  n'
          unwrap      <- frequency [(10, pure []),
                                    (1,  pure ["unwrap"])]
          return (identity, pre ++ unwrap ++ post, attrs)

aB :: Int -> Gen Block
aB n = let n' = n `div` 2
        in oneof [pure HorizontalRule                                     ,
                  pure Null                                               ,
                  BlockQuote     <$> aL aB n'                             ,
                  BulletList     <$> aL (aL aB) n'                        ,
                  DefinitionList <$> aL (aP (aL aI)
                                            (aL (aL aB)))
                                        n'                                ,
                  Para           <$> aL aI n'                             ,
                  Plain          <$> aL aI n'                             ,
                  CodeBlock      <$> aA n'          <*> aS n'             ,
                  Div            <$> aA n'          <*> aL aB n'          ,
                  RawBlock       <$> arb              <*> arb                 ,
                  OrderedList    <$> arb              <*> aL (aL aB) n'     ,
                  Header         <$> choose (0, 10) <*> aA n'
                                                    <*> aL aI n'          ,
                  Table          <$> aL aI n'       <*> arb
                                                    <*> aL (const (choose (0, 1))) n'
                                                    <*> aL     (aL aB)  n'
                                                    <*> aL (aL (aL aB)) n']

aC :: Int -> Gen Citation
aC n = let n' = n `div` 2
        in Citation <$> arb <*> aL aI n' <*> aL aI n' <*> arb <*> choose (0, 100)
                                                          <*> choose (0, 100)

aD :: Int -> Gen Pandoc
aD n = doc . fromList <$> aL aB n

aI :: Int -> Gen Inline
aI n = let n' = n `div` 2
        in oneof [pure Space                            ,
                  pure LineBreak                        ,
                  Emph        <$> aL aI n'              ,
                  Note        <$> aL aB n'              ,
                  SmallCaps   <$> aL aI n'              ,
                  Str         <$> arb                     ,
                  Strong      <$> aL aI n'              ,
                  Strikeout   <$> aL aI n'              ,
                  Superscript <$> aL aI n'              ,
                  Subscript   <$> aL aI n'              ,
                  Cite        <$> aL aC n'  <*> aL aI n',
                  Code        <$> aA n'     <*> arb       ,
                  Image       <$> aL aI n'  <*> arb       ,
                  Link        <$> aL aI n'  <*> arb       ,
                  Math        <$> arb         <*> arb       ,
                  Quoted      <$> arb         <*> aL aI n',
                  RawInline   <$> arb         <*> arb       ,
                  Span        <$> aA n'     <*> aL aI n']

aL :: (Int -> Gen a) -> Int -> Gen [a]
aL a n = let l = (:) <$> (a (n `div` 2)) <*> l
          in take <$> choose (0, n) <*> l

aP :: (Int -> Gen a) -> (Int -> Gen b) -> Int -> Gen (a, b)
aP l r n = let n' = n `div` 2
            in (,) <$> l n' <*> r n'

aS :: Int -> Gen String
aS n = oneof [arb, writeDoc <$> aD (n `div` 2)]

instance Arbitrary Alignment where
  arbitrary = elements [AlignLeft, AlignRight, AlignCenter, AlignDefault]

instance Arbitrary Block where
  arbitrary = sized aB

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
