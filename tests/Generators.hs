module Generators where

import Data.Text (pack, Text, unpack)
import Test.QuickCheck
import Text.Pandoc.Definition
import Data.Map
import GHC.Generics (Generic)

-- deriving instance Arbitrary Format via (GenericArbitrary Format)
-- deriving instance Arbitrary MetaValue via (GenericArbitrary MetaValue)

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary
  shrink    = fmap pack . shrink . unpack
