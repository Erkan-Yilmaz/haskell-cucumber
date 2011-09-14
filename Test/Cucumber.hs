{-# LANGUAGE NamedFieldPuns, DeriveDataTypeable, FlexibleInstances #-}

module Test.Cucumber where

import Language.Gherkin
import Test.Framework
import Test.Framework.Providers.HUnit
import Text.Parsec.String
import Text.Regex
import Data.Maybe
import Data.List
import Data.Typeable
import Control.Exception
import Prelude hiding (catch)
import Text.PrettyPrint
import Control.Arrow

type StepDefinitions = [StepDefinition]
data StepDefinition = StepDefinition { step_pattern :: String
                                     , step_action :: [String] -> Maybe BlockArg -> IO ()
                                     }

class StepDef s where
  step :: String -> s -> StepDefinition

instance StepDef ([String] -> IO ()) where
  step p a = StepDefinition p go
    where
      go (s:ss) Nothing = a (s:ss)
      go _ _ = throw StepDidNotExpectBlockArgument

instance StepDef ([String] ->  Table -> IO ()) where
  step p a = StepDefinition p go
    where
      go (s:ss) (Just (BlockTable t)) = a (s:ss) t
      go _ _ = throw StepExpectedTable

instance StepDef ([String] ->  String -> IO ()) where
  step p a = StepDefinition p go 
    where 
      go (s:ss) (Just (BlockPystring pystr)) = a (s:ss) pystr
      go _ _ = throw StepExpectedPystring

instance StepDef (String -> IO ()) where
  step p a = StepDefinition p go
    where
      go (s:ss) _ = throw StepDidNotExpectRegexArguments
      go [] (Just (BlockPystring pystr)) = a pystr
      go _ _ = throw StepExpectedPystring

instance StepDef (Table -> IO ()) where
  step p a = StepDefinition p go
    where
      go (s:ss) _ = throw StepDidNotExpectRegexArguments
      go [] (Just (BlockTable t)) = a t
      go _ _ = throw StepExpectedTable

instance StepDef (IO ()) where
  step p a = StepDefinition p go
    where
      go [] Nothing = a
      go _ _ = throw StepDidNotExpectAnyArguments

cucumber :: StepDefinitions -> FilePath -> IO [Test]
cucumber steps path = either (error . show) (cukeFeature steps) `fmap`
                parseFromFile parseFeature path

cukeFeature :: StepDefinitions -> Feature -> [Test]
cukeFeature steps Feature { feature_scenarios }  = concat $
                      cukeScenario steps `fmap` feature_scenarios

cukeScenario :: StepDefinitions -> Scenario -> [Test]
cukeScenario steps Scenario { scenario_name
                            , scenario_steps } =
  [cukeSteps steps scenario_name scenario_steps]

cukeScenario steps ScenarioOutline { scenario_name
                                   , scenario_steps
                                   , scenario_table } =
  zipWith go [0 .. ]
  (map (table_headers scenario_table `substitute` scenario_steps) $
   table_values scenario_table)
    where
      go i s = cukeSteps steps (scenario_name ++ " (" ++ show i ++ ")") s

cukeSteps :: StepDefinitions -> String -> [Step] -> Test
cukeSteps stepDefs name steps = testCase name $
                       mapM_ (executeStep stepDefs) steps

substitute :: [String] -> [Step] -> [String] -> [Step]
substitute  header steps values = map sub steps
  where
    subStepText s = s { step_body = subString $ step_body s
                      , step_arg = fmap subArg $ step_arg s
                      }
    sub s = s { step_text = subStepText $ step_text s }
    subString str = foldl (flip ($)) str $ regexs
    subArg (BlockPystring str) = BlockPystring $ subString str
    subArg (BlockTable (Table hs vs)) =
      let hs'new = map subString hs
          vs'new = map subString `map` vs
      in BlockTable $ Table hs'new vs'new
    regexs = zipWith mkSub header values
    mkSub h v i = let r = mkRegex $ "<" ++ h ++ ">"
                  in subRegex r i v

executeStep :: StepDefinitions -> Step -> IO ()
executeStep steps step = wrapError step $ pickStep steps step

pickStep :: StepDefinitions -> Step -> IO ()
pickStep steps step = exactlyOne $ catMaybes $ map go steps
  where
    exactlyOne [] = notFound
    exactlyOne [(_, act)] = act
    exactlyOne ss = throw $ MoreThanOneMatchingStepDefinition $ map fst ss
    notFound = throw $ PendingStep
    stepArg = step_arg $ step_text step
    go StepDefinition { step_pattern, step_action }
      = (const step_pattern &&&
         flip step_action stepArg) `fmap`
        matcher step_pattern
    matcher pat = mkRegex pat `matchRegex` step_body (step_text step)

data StepFailed = StepFailed { step_failed_exception :: SomeException
                             , step_failed_step :: String
                             }
                  deriving (Show, Typeable)

data CucumberException = PendingStep
                       | StepDidNotExpectRegexArguments
                       | StepDidNotExpectBlockArgument
                       | StepExpectedTable
                       | StepExpectedPystring
                       | StepDidNotExpectAnyArguments
                       | MoreThanOneMatchingStepDefinition [String]
                       deriving (Show, Typeable)

instance Exception StepFailed
instance Exception CucumberException

wrapError :: Step -> IO () -> IO ()
wrapError step io = io `catch` go
  where
    go :: SomeException -> IO ()
    go e = throw $ StepFailed { step_failed_exception = e
                              , step_failed_step = render $ prettyStep step
                              }
