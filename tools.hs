reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

split' :: (a -> Bool) -> [a] -> ([a], [a])
split' _ [] = ([], [])
split' p (x:xs)
    | p x = (x:a, b)
    | otherwise = (a, x:b)
    where (a, b) = split' p xs
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let (smallerSorted, biggerSorted) = split' (<=x) xs
    in (quicksort smallerSorted) ++ [x] ++ (quicksort biggerSorted)