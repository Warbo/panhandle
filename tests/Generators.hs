module Generators where

import Data.Text (pack, Text, unpack)
import Test.QuickCheck
import Text.Pandoc
import Data.Map
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary.Generic

deriving instance Arbitrary Block via (GenericArbitrary Block)
deriving instance Arbitrary Inline via (GenericArbitrary Inline)
deriving instance Arbitrary QuoteType via (GenericArbitrary QuoteType)
deriving instance Arbitrary MathType via (GenericArbitrary MathType)
deriving instance Arbitrary ListNumberStyle via (GenericArbitrary ListNumberStyle)
deriving instance Arbitrary Citation via (GenericArbitrary Citation)
deriving instance Arbitrary ListNumberDelim via (GenericArbitrary ListNumberDelim)
deriving instance Arbitrary CitationMode via (GenericArbitrary CitationMode)
deriving instance Arbitrary Format via (GenericArbitrary Format)
deriving instance Arbitrary Alignment via (GenericArbitrary Alignment)
deriving instance Arbitrary Pandoc via (GenericArbitrary Pandoc)
deriving instance Arbitrary Meta via (GenericArbitrary Meta)
deriving instance Arbitrary MetaValue via (GenericArbitrary MetaValue)

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary
  shrink    = fmap pack . shrink . unpack
