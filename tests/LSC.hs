{-# LANGUAGE RankNTypes, DeriveDataTypeable #-}
module LSC where

import Data.Data
import Data.Tagged
import Test.LazySmallCheck2012
import Test.Tasty.Providers

newtype LSC a = LSC a

instance (Testable a, Typeable a, Data a) => IsTest (LSC a) where
  run os (LSC t) p = do b <- depthCheckResult 3 t
                        return (if b then testPassed ""
                                     else testFailed "")
  testOptions = Tagged {
      unTagged = []
    }

lazyProperty n t = singleTest n (LSC t)
