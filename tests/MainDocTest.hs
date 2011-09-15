module Main where

import Test.Framework.Providers.DocTest
import Test.Framework

main = docTest ["Test/Cucumber.hs"] [] >>= defaultMain . return
