{-# LANGUAGE TemplateHaskell #-}

module Generators where

import Data.DeriveTH
import Test.QuickCheck
import Text.Pandoc
import Data.Derive.Arbitrary
import Data.Map
import Test.LazySmallCheck2012
import Test.LazySmallCheck2012.TH

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

$( deriveSerial ''Pandoc )
$( deriveSerial ''Block )
$( deriveSerial ''Inline )
$( deriveSerial ''MetaValue )
$( deriveSerial ''QuoteType )
$( deriveSerial ''ListNumberStyle )
$( deriveSerial ''MathType )
$( deriveSerial ''ListNumberDelim )
$( deriveSerial ''CitationMode )
$( deriveSerial ''Alignment )

instance (Serial a, Ord a, Serial b) => Serial (Map a b) where
  series = cons1 fromList

instance Serial Meta where
  series = cons1 Meta

instance Serial Citation where
  series = cons5 Citation <*> series

instance Serial Double where
  series = drawnFrom $ \d -> Prelude.map (fromInteger . toInteger) [(-d)..d]

instance Serial Format where
  series = cons1 Format
