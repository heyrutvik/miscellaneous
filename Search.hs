module Search where

-- linear search over list return index
linear :: (Eq a, Num b) => [a] -> a -> b
linear xs x = go 1 xs x
  where
    go acc (y:ys) z | z == y = acc
                    | otherwise = go (acc+1) ys z
    go _ _ _ = 0

-- return elem at index
elementAt :: Int -> [a] -> a
elementAt i (x:xs) = if i <= 1
                     then x
                     else elementAt (i-1) xs
elementAt _ _ = error "out of bound"

-- binary search
binary :: Ord a => [a] -> a -> Int
binary xs x = binary1 xs x 1 (length xs)
  where
    binary1 xs x s e
        | x < el = binary1 xs x s elemAt
        | x > el = binary1 xs x (elemAt+1) e
        | otherwise = elemAt
      where
        elemAt = (s+e) `div` 2
        el = elementAt elemAt xs

