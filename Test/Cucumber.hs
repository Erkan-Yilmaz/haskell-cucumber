module Test.Cucumber (cuke) where

import Language.Gherkin as Gherkin
import Test.Framework

data Configuration = Configuration

cuke :: Configuration -> Gherkin.AST -> [Test]
cuke = error "tbd"
