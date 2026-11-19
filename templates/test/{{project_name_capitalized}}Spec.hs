module {{project_name_capitalized}}Spec (spec) where

import {{project_name_capitalized}} qualified as App
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "greeting" $ do
  it "greets the world" $
    App.greeting "world" `shouldBe` "Hello, world!"

  it "preserves the supplied name, including spaces and punctuation" $
    property $
      \name -> App.greeting name == "Hello, " ++ name ++ "!"
