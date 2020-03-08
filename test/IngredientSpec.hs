{-# LANGUAGE OverloadedStrings #-}
module IngredientSpec where
import Test.Hspec
import Ingredient (ingredients)

spec = do
 describe "ingredients" $ do
   it "create `Classic` plus `chocolate`" $
     ingredients "Classic,+chocolate" `shouldBe` ["banana", "chocolate","honey","ice","mango","peach","pineapple","strawberry", "yogurt"]

   it "create `Classic` plus `chocolate` minus `strawberry`" $
     ingredients "Classic,+chocolate,-strawberry" `shouldBe` ["banana", "chocolate","honey","ice","mango","peach","pineapple","yogurt"]

   it "create `Classic` smoothie" $
     ingredients "Classic" `shouldBe` ["banana","honey","ice","mango","peach","pineapple","strawberry","yogurt"]

   it "create `Classic` minus `strawberry`" $
     ingredients "Classic,-strawberry" `shouldBe` ["banana","honey","ice","mango","peach","pineapple","yogurt"]

   it "create `Just Desserts` smoothie" $
     ingredients "Just Desserts" `shouldBe` ["banana","cherry","chocolate","ice cream","peanut"]

   it "create `Just Desserts` smoothie without `ice cream` and `peanut`" $
     ingredients "Just Desserts,-ice cream,-peanut" `shouldBe` ["banana","cherry","chocolate"]

   it "create a smoothie without ingredients" $
     ingredients "Just Desserts,-banana,-cherry,-chocolate,-ice cream,-peanut" `shouldBe` []

   it "exclude unused ingredients" $
     ingredients "Classic,-banana,-mango,-peanut"
     `shouldBe` ["honey","ice","peach","pineapple","strawberry","yogurt"]
