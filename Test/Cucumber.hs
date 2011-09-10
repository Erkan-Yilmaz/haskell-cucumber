{-# LANGUAGE NamedFieldPuns #-}

module Test.Cucumber where

import Language.Gherkin
import Test.Framework
import Test.Framework.Providers.HUnit
import Text.Parsec.String

cucumber :: FilePath -> IO [Test]
cucumber path = either (error . show) cukeFeature `fmap` 
                parseFromFile parseFeature path
    
cukeFeature :: Feature -> [Test]
cukeFeature Feature { feature_scenarios }  = concat $ 
                      cukeScenario `fmap` feature_scenarios
    
cukeScenario :: Scenario -> [Test]    
cukeScenario Scenario { scenario_name
                      , scenario_steps } = [testCase scenario_name $ mapM_ executeStep scenario_steps]  

executeStep :: Step -> IO ()
executeStep step = step_text step 