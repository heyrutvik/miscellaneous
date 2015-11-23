module Search where

-- linear search over list return index
linear :: (Eq a, Num a1) => [a] -> a -> a1
linear xs x = go 1 xs x
  where
    go acc (y:ys) z | z == y = acc
                    | otherwise = go (acc+1) ys z
    go _ _ _ = 0
