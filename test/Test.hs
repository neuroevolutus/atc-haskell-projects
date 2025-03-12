-- \| Tests for the todo-manager
{-# LANGUAGE BlockArguments #-}

import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec

import TodoManager

main = defaultMain tests

tests ∷ TestTree
tests = testGroup "Tests" [unitTests]

unitTests ∷ TestTree
unitTests = unsafePerformIO
  $ testSpec "Unit tests"
  $ describe "Task" do
    it "should be instantiable"
      $ Task "Wash dishes" `shouldSatisfy` (const True)
