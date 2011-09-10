{-# LANGUAGE NamedFieldPuns #-}

module Test.Cucumber where

import Language.Gherkin
import Test.Framework
import Test.Framework.Providers.HUnit
import Text.Parsec.String
import Text.Regex
import Data.Maybe
import Data.List

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
executeStep steps step = pickStep steps step 

pickStep :: StepDefinitions -> Step -> IO ()
pickStep steps step = fromMaybe notFound $ step_action `fmap` find go steps
  where
    notFound = error $ "Could not find a step definition.\nYou need to implement it before proceeding"
    go StepDefinition { step_pattern } = isJust $ mkRegex step_pattern `matchRegex` step_body (step_text step)