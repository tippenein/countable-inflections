{-# LANGUAGE OverloadedStrings #-}

module Text.CountableSpec (spec) where

import Test.Hspec

import Text.Countable

spec :: Spec
spec = do
  irregularCases
  matchingCases

matchingCases :: Spec
matchingCases = do
  describe "laws" $ do
    let equality1 a = a `shouldBe` (singularize . pluralize) a
    let equality2 a = a `shouldBe` (pluralize . singularize) a

    it "returns itself when applied to (singularize . pluralize)" $
      mapM_ equality1 ["thing", "branch", "ox"]

    it "returns itself when applied to (pluralize . singularize)" $
      mapM_ equality2 ["things", "branches", "oxen"]

  describe "pluralize" $ do
    it "handles the normal case" $ do
      pluralize "thing" `shouldBe` "things"

    it "pluralizes regex cases" $ do
      pluralize "ox" `shouldBe` "oxen"
      pluralize "vertex" `shouldBe` "vertices"

  describe "singularize" $ do
    it "singularizes regex cases" $ do
      singularize "mice" `shouldBe` "mouse"
      singularize "oxen" `shouldBe` "ox"
      singularize "branches" `shouldBe` "branch"
      singularize "transactions" `shouldBe` "transaction"
      singularize "sessions" `shouldBe` "session"
      singularize "users" `shouldBe` "user"


irregularCases :: Spec
irregularCases =
  it "can singularize irregulars" $
    singularize "people" `shouldBe` "person"
