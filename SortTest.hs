module SortTest where

import Sort 

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ 
    describe "SORTTEST.HS MODULE UNIT TEST" $ do
        describe "quickSort function unit test" $ do 
            it "test_001" $ 
                quickSort [4,7,3,9,23,7] `shouldBe` [3,4,7,7,9,23]
            it "test_002" $ 
                quickSort [4,2,3,9,23,7] `shouldBe` [2,3,4,7,9,23]
        describe "merge function unit test" $ do 
            it "test_001" $ 
                merge [4,7] [1,5] `shouldBe` [1,4,5,7]
            it "test_002" $ 
                merge [4] [1] `shouldBe` [1,4]
        describe "mergeSort function unit test" $ do 
            it "test_001" $ 
                mergeSort [4,7,3,9,23,7] `shouldBe` [3,4,7,7,9,23]
            it "test_002" $ 
                mergeSort [4,2,3,9,23,7] `shouldBe` [2,3,4,7,9,23]
        describe "heapSort function unit test" $ do 
            it "test_001" $ 
                heapSort [4,7,3,9,23,7] `shouldBe` [3,4,7,7,9,23]
            it "test_002" $ 
                heapSort [4,2,3,9,23,7] `shouldBe` [2,3,4,7,9,23]
        describe "selectionSort function unit test" $ do 
            it "test_001" $ 
                selectionSort [4,7,3,9,23,7] `shouldBe` [3,4,7,7,9,23]
            it "test_002" $ 
                selectionSort [4,2,3,9,23,7] `shouldBe` [2,3,4,7,9,23]
            it "test_003" $ 
                selectionSort [4,4] `shouldBe` [4,4]
        describe "bubbleSort function unit test" $ do 
            it "test_001" $ 
                bubbleSort [4,7,3,9,23,7] `shouldBe` [3,4,7,7,9,23]
            it "test_002" $ 
                bubbleSort [4,2,3,9,23,7] `shouldBe` [2,3,4,7,9,23]
        describe "insertionSort function unit test" $ do 
            it "test_001" $ 
                insertionSort [4,7,3,9,23,7] `shouldBe` [3,4,7,7,9,23]
            it "test_002" $ 
                insertionSort [4,2,3,9,23,7] `shouldBe` [2,3,4,7,9,23]
        describe "swap function unit test" $ do 
            it "test_001" $ 
                swap 1 2 [4,7,3,9,23,7] `shouldBe` [4,3,7,9,23,7] 
            it "test_002" $ 
                swap 1 5 [4,7,3,9,23,7] `shouldBe` [4,7,3,9,23,7] 
            it "test_003" $ 
                swap 5 1 [4,7,3,9,23,7] `shouldBe` [4,7,3,9,23,7] 
            it "test_004" $ 
                swap 3 3 [4,7,3,9,23,7] `shouldBe` [4,7,3,9,23,7]
        describe "replace function unit test" $ do 
            it "test_001" $ 
                replace 1 2 [4,7,3,9,23,7] `shouldBe` [4,2,3,9,23,7] 
            it "test_002" $ 
                replace 4 0 [4,7,3,9,23,7] `shouldBe` [4,7,3,9,0,7] 
            it "test_003" $ 
                replace 0 0 [4,7,3,9,23,7] `shouldBe` [0,7,3,9,23,7] 
            it "test_004" $ 
                replace 5 0 [0,7,3,9,23,7] `shouldBe` [0,7,3,9,23,0] 
        describe "selection function unit test" $ do 
            it "test_001" $ 
                selection 8 [4,7,2,9,23,7] `shouldBe` 2 
            it "test_002" $ 
                selection 0 [4,7,3,9,23,7] `shouldBe` (-1) 
            it "test_003" $ 
                selection 3 [4,7,3,9,23,7] `shouldBe` (-1) 
            it "test_003" $ 
                selection 3 [] `shouldBe` (-1) 
        describe "deleteByIndex function unit test" $ do 
            it "test_001" $ 
                deleteByIndex 3 [4,7,2,9,23,7] `shouldBe` [4,7,2,23,7]
            it "test_002" $ 
                deleteByIndex 0 [4,7,3,9,23,7] `shouldBe` [7,3,9,23,7]
            it "test_003" $ 
                deleteByIndex 5 [4,7,3,9,23,7] `shouldBe` [4,7,3,9,23]
        describe "insertion function unit test" $ do 
            it "test_001" $ 
                insertion 3 [4,7,2] `shouldBe` [3,4,7,2]
            it "test_002" $ 
                insertion 5 [4,7,2] `shouldBe` [4,5,7,2]
            it "test_002" $ 
                insertion 10 [4,7,2] `shouldBe` [4,7,2,10]
        describe "bubbleSwap function unit test" $ do 
            it "test_001" $ 
                bubbleSwap [3,4,7,2] `shouldBe` [3,4,2,7]
            it "test_001" $ 
                bubbleSwap [9,4,7,2] `shouldBe` [4,7,2,9]
