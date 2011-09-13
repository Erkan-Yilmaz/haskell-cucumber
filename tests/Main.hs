module Main where

import Test.Framework
import Test.Cucumber
import Control.Monad
import Language.Gherkin

(=.=) :: (Show a, Eq a) => a -> a -> Bool
a =.= b | a /= b = error $ "Expected " ++ show a ++ " Got: " ++ show b 
        | otherwise = True

ret :: Bool -> IO ()
ret True = return ()
ret _ = return ()

steps = [
  step "a step '(.*)' matches 'value'" $ \[v] ->
   ret $ "value" =.= v
  
  , stepTable "step gets the argument" $ \[] (Table hs vs) ->
   ret $ ["foo"] =.= hs && [["bar"]] =.= vs 
  
  , step "a table (.*)" $ \[v] -> 
   ret $ "first" =.= v || "second" =.= v 
  
  , stepStr "a pystring" $ \[] str ->
   ret $ "Foobar" =.= str
  
  , stepTable "expand table values" $ \[] (Table hs vs) ->
   ret $ hs =.= ["values"] && vs =.= [["foo"]]
  ]

main = cucumber steps "tests/cuke.feature" >>= defaultMain
