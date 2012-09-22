module Test where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Hello

prop_saysHello str =
  (length str < 10) ==> (length hello > length str)
  where types = (str :: String)

case_saysHello = hello @?= "hello, world!"

tests = [
  testGroup "Hello Props" [
    testProperty "says hello" prop_saysHello
    ],
  testGroup "Hello Tests" [
    testCase "says hello" case_saysHello
    ]
  ]

-- convenient whilst in ghci
runTests = defaultMain tests
