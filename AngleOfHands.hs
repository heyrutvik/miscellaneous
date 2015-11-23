{-
I was trying to solve aptitude problems on clock degrees
which leads me to this.. ;)

Reference:
https://wiki.haskell.org/Converting_numbers
-}

-- to format minute and hour
formatTo :: Integer -> Integer -> Integer
formatTo f n = n `mod` f

-- hour digit n to dial number
-- say, 3 hour to dial number 15
hourToDialMinute :: Integer -> Integer
hourToDialMinute n = formatTo 12 n * 5

absoluteValue :: Double -> Double
absoluteValue n
  | n < 0 = -n
  | otherwise = n

-- minute to degree conversion
dialMinuteToDegree :: Integer -> Double
dialMinuteToDegree n = 6.0 * fromIntegral n

{-
degrees traversed by hour hand from last hour number
because of minute hand rotation
-}
extraDegree :: Integer -> Double
extraDegree n = dialMinuteToDegree n / 12.0

-- get angle between hour and minute hands
angleOfHands :: Integer -> Integer -> Double
angleOfHands h m = absoluteValue (
  (dialMinuteToDegree (hourToDialMinute h) + extraDegree m)
  - dialMinuteToDegree m
  )
