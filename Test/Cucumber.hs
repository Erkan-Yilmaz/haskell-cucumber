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

type StepDefinitions = [StepDefinition]
data StepDefinition = StepDefinition { step_pattern :: String
                                     , step_action :: IO ()
                                     }

cucumber :: StepDefinitions -> FilePath -> IO [Test]
cucumber steps path = either (error . show) (cukeFeature steps) `fmap` 
                parseFromFile parseFeature path
    
cukeFeature :: StepDefinitions -> Feature -> [Test]
cukeFeature steps Feature { feature_scenarios }  = concat $ 
                      cukeScenario steps `fmap` feature_scenarios
    
cukeScenario :: StepDefinitions -> Scenario -> [Test]    
cukeScenario steps Scenario { scenario_name
                      , scenario_steps } = [testCase scenario_name $ mapM_ (executeStep steps) scenario_steps]  

executeStep :: StepDefinitions -> Step -> IO ()
executeStep steps step = wrapError step $ pickStep steps step  

pickStep :: StepDefinitions -> Step -> IO ()
pickStep steps step = fromMaybe notFound $ step_action `fmap` find go steps
  where
    notFound = throw $ PendingStep
    go StepDefinition { step_pattern } = isJust $ mkRegex step_pattern `matchRegex` step_body (step_text step)

data StepFailed = StepFailed { step_failed_exception :: SomeException
                             , step_failed_step :: String
                             }
                  deriving (Show, Typeable)
data Pending = PendingStep
               deriving (Show, Typeable)

instance Exception StepFailed 
instance Exception Pending

wrapError :: Step -> IO () -> IO ()
wrapError step io = io `catch` go 
  where
    go :: SomeException -> IO ()
    go e = throw $ StepFailed { step_failed_exception = e
                              , step_failed_step = render $ prettyStep step 
                              }