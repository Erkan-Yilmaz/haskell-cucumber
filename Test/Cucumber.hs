module Test.Cucumber where

import qualified Language.Gherkin as Gherkin
import Test.Framework

data Arg = Pystring String
         | Table [[String]]

data CukeM = CukeM

data Step m = Step m

data Runner s m = Runner {
  run_before :: CukeM s
  run_step :: s -> m () -> Cuke s
  run_after :: s -> CukeM ()
  }

scenario :: CukeM String
tags :: CukeM [String]
failScenario :: String -> CukeM a
isFailing :: CukeM Bool
liftIO :: IO a -> CukeM a

cuke :: Runner s -> Gherkin.Feature -> [Test]
 
cukeScenario :: Runner s -> Gherkin.Scenario -> [Test]

type StepPattern = String
step :: StepPattern -> [String] -> Maybe Arg -> m () -> Step m
