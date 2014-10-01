import Control.Applicative
import PanHandler
import Test.QuickCheck
import Text.Pandoc
import Text.Pandoc.Builder

sizedB :: Int -> Gen Block
sizedB n = let arb :: Arbitrary a => Gen a
               arb = resize (max (n `div` 2) 1) arbitrary
            in oneof [pure HorizontalRule,
                      pure Null,
                      BlockQuote     <$> arb,
                      BulletList     <$> arb,
                      DefinitionList <$> arb,
                      Para           <$> arb,
                      Plain          <$> arb,
                      CodeBlock      <$> arb
                                     <*> arb,
                      Div            <$> arb
                                     <*> arb,
                      RawBlock       <$> arb
                                     <*> arb,
                      OrderedList    <$> arb
                                     <*> arb,
                      Header         <$> arb
                                     <*> arb
                                     <*> arb,
                      Table          <$> arb
                                     <*> arb
                                     <*> arb
                                     <*> arb
                                     <*> arb]

instance Arbitrary Block where
  arbitrary = sized sizedB

sizedI :: Int -> Gen Inline
sizedI n = let arb :: Arbitrary a => Gen a
               arb = resize (max (n `div` 2) 1) arbitrary
            in oneof [pure Space,
                      pure LineBreak,
                      Emph        <$> arb,
                      Note        <$> arb,
                      SmallCaps   <$> arb,
                      Str         <$> arb,
                      Strong      <$> arb,
                      Strikeout   <$> arb,
                      Superscript <$> arb,
                      Subscript   <$> arb,
                      Cite        <$> arb <*> arb,
                      Code        <$> arb <*> arb,
                      Image       <$> arb <*> arb,
                      Link        <$> arb <*> arb,
                      Math        <$> arb <*> arb,
                      Quoted      <$> arb <*> arb,
                      RawInline   <$> arb <*> arb,
                      Span        <$> arb <*> arb]

instance Arbitrary Inline where
  arbitrary = sized sizedI

instance Arbitrary Alignment where
  arbitrary = elements [AlignLeft, AlignRight, AlignCenter, AlignDefault]

instance Arbitrary ListNumberStyle where
  arbitrary = elements [DefaultStyle, Example, Decimal, LowerRoman, UpperRoman,
                        LowerAlpha, UpperAlpha]

instance Arbitrary ListNumberDelim where
  arbitrary = elements [DefaultDelim, Period, OneParen, TwoParens]

instance Arbitrary Format where
  arbitrary = Format <$> arbitrary

instance Arbitrary QuoteType where
  arbitrary = elements [SingleQuote, DoubleQuote]

instance Arbitrary MathType where
  arbitrary = elements [DisplayMath, InlineMath]

sizedC :: Int -> Gen Citation
sizedC n = let arb :: Arbitrary a => Gen a
               arb = resize (max (n `div` 2) 1) arbitrary
            in Citation <$> arb
                        <*> arb
                        <*> arb
                        <*> arb
                        <*> arb
                        <*> arb

instance Arbitrary Citation where
  arbitrary = sized sizedC

instance Arbitrary CitationMode where
  arbitrary = elements [AuthorInText, SuppressAuthor, NormalCitation]

sizedP :: Int -> Gen Pandoc
sizedP n = doc . fromList <$> resize (n `div` 2) arbitrary

instance Arbitrary Pandoc where
  arbitrary = sized sizedP
