module TestBaseParsers
  ( checkOk
  , checkEof
  , checkSatisfy
  , checkElement
  , checkStream
  ) where

import Test.Hspec
import Hw2Block3(Parser(..), ok, eof, satisfy, element, stream)

checkOk :: SpecWith()
checkOk =
  describe "ok-parser tests" $ do
    it "empty" $
      runParser ok "" `shouldBe` Just((), "")

    it "test string" $
      runParser ok "Arina" `shouldBe` Just((), ['A', 'r', 'i', 'n', 'a'])

    it "test int-list" $
      runParser ok ([1..5]::[Int]) `shouldBe` Just((), [1..5])

checkEof :: SpecWith()
checkEof =
  describe "eof-parser tests" $ do
    it "empty" $
      runParser eof "" `shouldBe` Just((), "")

    it "test string" $
      runParser eof "Arina" `shouldBe` Nothing

    it "test int-list" $
      runParser eof ([1..5]::[Int]) `shouldBe` Nothing

checkSatisfy :: SpecWith()
checkSatisfy =
  describe "satisfy-parser tests" $ do
    it "empty" $
      runParser (satisfy (== 'a')) "" `shouldBe` Nothing

    it "test string" $ do
      runParser (satisfy (== 'A')) "Arina" `shouldBe` Just('A', ['r', 'i', 'n', 'a'])
      runParser (satisfy (== 'B')) "Arina" `shouldBe` Nothing

    it "test int-list" $ do
      runParser (satisfy even) ([2..10]::[Int]) `shouldBe` Just(2, [3..10])
      runParser (satisfy even) ([5..10]::[Int]) `shouldBe` Nothing


checkElement :: SpecWith()
checkElement =
  describe "element-parser tests" $ do
    it "empty" $
      runParser (element 'a') "" `shouldBe` Nothing

    it "test string" $ do
      runParser (element 'A') "Arina" `shouldBe` Just('A', ['r', 'i', 'n', 'a'])
      runParser (element 'B') "Arina" `shouldBe` Nothing

    it "test int-list" $ do
      runParser (element 2) ([2..10]::[Int]) `shouldBe` Just(2, [3..10])
      runParser (element 13) ([5..10]::[Int]) `shouldBe` Nothing

checkStream :: SpecWith()
checkStream =
  describe "stream-parser tests" $ do
    it "empty" $
      runParser (stream "hello") "" `shouldBe` Nothing

    it "test string" $ do
      runParser (stream "Ari") "Arina" `shouldBe` Just(['A', 'r', 'i'], ['n', 'a'])
      runParser (stream "Ai") "Arina" `shouldBe` Nothing

    it "test int-list" $ do
      runParser (stream [2..5]) ([2..10]::[Int]) `shouldBe` Just([2..5], [6..10])
      runParser (stream [11..13]) ([5..10]::[Int]) `shouldBe` Nothing
