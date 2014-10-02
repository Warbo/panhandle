import Control.Applicative
import PanHandler
import Test.QuickCheck
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Walk (walk, query)

-- Tests

pBlocksUnwrapped d = query p (transform d) == Nothing
  where p (CodeBlock (_, cs, _) _) | "unwrap" `elem` cs = Just ()
        p _                                             = Nothing

-- Arbitrary instances

aA :: Gen Attr
aA = do identity     <- arbitrary
        (cPre,cPost) <- arbitrary
        attrs        <- arbitrary
        unwrap       <- elements [[], ["unwrap"]]
        return (identity, cPre ++ unwrap ++ cPost, attrs)

aB :: Int -> Gen Block
aB 0 = elements [HorizontalRule, Null]
aB n = let n' = n `div` 2
        in frequency [(50, pure HorizontalRule                           ),
                      (50, pure Null                                     ),
                      (1,  BlockQuote     <$> aL aB n'                   ),
                      (1,  BulletList     <$> aL (aL aB) n'              ),
                      (1,  DefinitionList <$> a                          ),
                      (1,  Para           <$> a                          ),
                      (1,  Plain          <$> a                          ),
                      (1,  CodeBlock      <$> aA <*> aS                  ),
                      (1,  Div            <$> aA <*> a                   ),
                      (1,  RawBlock       <$> a  <*> a                   ),
                      (1,  OrderedList    <$> a  <*> a                   ),
                      (1,  Header         <$> a  <*> aA <*> a            ),
                      (1,  Table          <$> a  <*> a  <*> a <*> a <*> a)]

aL :: (Int -> Gen a) -> Int -> Gen [a]
aL a 0 = []
aL a n = take <$> choose (0, n) <*> a (n `div` 2)

aC :: Gen Citation
aC = let a :: Arbitrary a => Gen a
         a = arbitrary
      in Citation <$> a <*> a <*> a <*> a <*> a <*> a

aI :: Gen Inline
aI = let a :: Arbitrary a => Gen a
         a = arbitrary
      in frequency [(50, pure Space              ),
                    (50, pure LineBreak          ),
                    (1,  Emph        <$> a       ),
                    (1,  Note        <$> a       ),
                    (1,  SmallCaps   <$> a       ),
                    (50, Str         <$> a       ),
                    (1,  Strong      <$> a       ),
                    (1,  Strikeout   <$> a       ),
                    (1,  Superscript <$> a       ),
                    (1,  Subscript   <$> a       ),
                    (1,  Cite        <$> a  <*> a),
                    (5,  Code        <$> aA <*> a),
                    (1,  Image       <$> a  <*> a),
                    (1,  Link        <$> a  <*> a),
                    (1,  Math        <$> a  <*> a),
                    (1,  Quoted      <$> a  <*> a),
                    (10, RawInline   <$> a  <*> a),
                    (1,  Span        <$> aA <*> a)]

aS :: Gen String
aS = frequency [(50, arbitrary),
                (1,  writeDoc <$> arbitrary)]

instance Arbitrary Alignment where
  arbitrary = elements [AlignLeft, AlignRight, AlignCenter, AlignDefault]

instance Arbitrary Block where
  arbitrary = aB

instance Arbitrary Citation where
  arbitrary = aC

instance Arbitrary CitationMode where
  arbitrary = elements [AuthorInText, SuppressAuthor, NormalCitation]

instance Arbitrary Format where
  arbitrary = Format <$> arbitrary

instance Arbitrary Inline where
  arbitrary = aI

instance Arbitrary ListNumberDelim where
  arbitrary = elements [DefaultDelim, Period, OneParen, TwoParens]

instance Arbitrary ListNumberStyle where
  arbitrary = elements [DefaultStyle, Example, Decimal, LowerRoman, UpperRoman,
                        LowerAlpha, UpperAlpha]

instance Arbitrary MathType where
  arbitrary = elements [DisplayMath, InlineMath]

instance Arbitrary Pandoc where
  arbitrary = doc . fromList <$> arbitrary

instance Arbitrary QuoteType where
  arbitrary = elements [SingleQuote, DoubleQuote]
