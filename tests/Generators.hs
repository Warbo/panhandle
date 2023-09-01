{-# LANGUAGE TemplateHaskell #-}

module Generators where

import Data.DeriveTH
import Data.Text (pack, Text, unpack)
import Test.QuickCheck
import Text.Pandoc
import Data.Derive.Arbitrary
import Data.Map

$( derive makeArbitrary ''Block )
$( derive makeArbitrary ''Inline )
$( derive makeArbitrary ''QuoteType )
$( derive makeArbitrary ''MathType )
$( derive makeArbitrary ''ListNumberStyle )
$( derive makeArbitrary ''Citation )
$( derive makeArbitrary ''ListNumberDelim )
$( derive makeArbitrary ''CitationMode )
$( derive makeArbitrary ''Format )
$( derive makeArbitrary ''Alignment )
$( derive makeArbitrary ''Pandoc )
$( derive makeArbitrary ''Meta )
$( derive makeArbitrary ''MetaValue )


instance Arbitrary Text where
  arbitrary = fmap pack arbitrary
  shrink    = fmap pack . shrink . unpack
