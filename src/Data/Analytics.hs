module Data.Analytics
(average, median) 
where
  
import Data.List

average :: (Fractional a) => [a] -> a
average xs = foldr (+) 0.0 xs / realToFrac (length xs)


median :: (Fractional a, Ord a) => [a] -> a
median xs
 | odd  $ length xs = select xs (length xs `div` 2)
 | even $ length xs = average  $ [(select xs k)] ++ [(select xs (k-1))]
 where k = length xs `div` 2


select :: (Ord a) => [a] -> Int -> a 
select xs k = (!!) (sort xs) (k-1)  
