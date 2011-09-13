{-# LANGUAGE NamedFieldPuns, DeriveDataTypeable #-}

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
                                     , step_action :: [String] -> IO ()
                                     }
                    | StepWithTable { step_pattern :: String
                                   , step_action_with_table :: [String] 
                                                               -> Table
                                                               -> IO ()
                                   }
                    | StepWithString { step_pattern :: String
                                     , step_action_with_string :: [String]
                                                                  -> String
                                                                  -> IO ()
                                     }
                      
stepTable :: String -> ([String] ->  Table -> IO ()) -> StepDefinition
stepTable = StepWithTable

stepStr :: String -> ([String] ->  String -> IO ()) -> StepDefinition
stepStr = StepWithString

step :: String -> ([String] ->  IO ()) -> StepDefinition
step pattern act = StepDefinition pattern act

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
    withoutArg s params = case stepArg of
      Nothing -> s params
      _ -> throw StepDidNotExpectBlockArgument
    withTable s params = case stepArg of 
      Just (BlockTable t) -> s params t 
      _ -> throw StepExpectedTable
    withString s params = case stepArg of
      Just (BlockPystring str) -> s params str
      _ -> throw StepExpectedPystring
    go StepWithString { step_pattern
                      , step_action_with_string
                      } = (const step_pattern &&&
                           withString step_action_with_string) `fmap`
                          matcher step_pattern
    go StepDefinition { step_pattern 
                      , step_action 
                      } = (const step_pattern &&& 
                           withoutArg step_action) `fmap` 
                          matcher step_pattern 
    go StepWithTable { step_pattern
                     , step_action_with_table } = 
      (const step_pattern &&& 
       withTable step_action_with_table) `fmap`
      matcher step_pattern
    matcher pat = mkRegex pat `matchRegex` step_body (step_text step)

data StepFailed = StepFailed { step_failed_exception :: SomeException
                             , step_failed_step :: String
                             }
                  deriving (Show, Typeable)
                           
data CucumberException = PendingStep 
                       | StepDidNotExpectBlockArgument
                       | StepExpectedTable
                       | StepExpectedPystring
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
