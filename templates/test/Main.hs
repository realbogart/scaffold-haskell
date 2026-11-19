module Main (main) where

import {{project_name_capitalized}}Spec qualified as AppSpec
import Test.Hspec

main :: IO ()
main = hspec AppSpec.spec
