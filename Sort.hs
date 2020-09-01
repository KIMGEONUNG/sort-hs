module Sort where

import Data.List

testLs = [4,7,3,9,23,7]

{-****** QUICK SORT ******-}
-- | Quick sort implementation
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort a ++ [x] ++ quickSort b 
                where 
                    a = filter (<x) xs
                    b = filter (>=x) xs


{-****** MERGE SORT ******-}
-- | merge operation used in mergeSort 
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs 
merge [] ys = ys 
merge (x:xs) (y:ys) = 
    if x < y 
        then x : merge xs (y:ys) 
        else y : merge (x:xs) ys 

-- | Merge sort implementation
mergeSort :: Ord a => [a] -> [a]
mergeSort xs = 
    if length xs <= 1
        then xs
        else merge left right
            where
                mid = div (length xs) 2
                tupe = splitAt mid xs
                left = mergeSort $ fst tupe
                right = mergeSort $ snd tupe


{-****** HEAP SORT ******-}
heapSort :: Ord a => [a] -> [a]
heapSort = error "Not implementated"

{-****** SELECTION SORT ******-}
selection :: Ord a => a -> [a] -> Int
selection _ [] = -1
selection v xs = if fst tupe >= v then (-1) else snd tupe
                    where 
                        tupes = zip xs [0..] 
                        tupe = minimum tupes  
                        
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort (x:xs) = if target == (-1)
                            then x : selectionSort xs
                            else xs !! target : selectionSort (replace target x xs)
                        where 
                            target = selection x xs

{-****** BOUBLE SORT ******-}
bubbleSwap :: Ord a => [a] -> [a]
bubbleSwap [] = []
bubbleSwap [x] = [x]
bubbleSwap (x:y:ys) | x < y = x:bubbleSwap (y:ys) 
                    | otherwise = y:bubbleSwap (x:ys)


bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = let ls = bubbleSwap xs 
                in bubbleSort (init ls) ++ [last ls] 

{-****** INSERTION SORT ******-}
insertion :: Ord a => a -> [a] -> [a]
insertion x [] = [x]
insertion y (x:xs) | y < x = y:x:xs
                   | otherwise = x : insertion y xs

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insertion [] 
-- insertionSort [] = []
-- insertionSort (x:xs) = insertion x (insertionSort xs) 

{-****** UTILITIES ******-}
deleteByIndex :: Int -> [a] -> [a]
deleteByIndex i xs = take i xs ++ drop (i+1) xs 

swap :: Int -> Int -> [a] -> [a]
swap n1 n2 xs = if n1 == n2 
                    then xs
                    else a ++ [xs !! j] ++ b ++ [xs !! i] ++ c 
                        where 
                            i = min n1 n2       
                            j = max n1 n2       
                            a = take i xs
                            b = take (j - i - 1) $ drop (i+1) xs
                            c = drop (j + 1) xs

replace :: Int -> a -> [a] -> [a]
replace i v xs = take i xs ++ [v] ++ drop (i+1) xs


