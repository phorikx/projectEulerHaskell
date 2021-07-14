module Module where
fac n = product [1..n]
boven n k = fac n `div` (fac k * fac (n-k))
pi = 3.1415927
n !^! k = fac n / (fac k * fac (n - k))
kwadraat x = x*x
somVanKwadraten lijst = sum( map kwadraat lijst)
-- Functions for the triangle number
triangleNumber:: Int -> Int
triangleNumber n = sum [1..n]
doesDivide:: Int -> Int -> Bool
doesDivide n k  = ((mod n k) == 0)
dividors :: Int -> [Int]
dividors k = filter (doesDivide k) [1..z] ++ [k] where z = k `div` 2
getTriangleNumber:: Int -> Int -> Int
getTriangleNumber k n
    | k > (length.dividors.triangleNumber) n      = getTriangleNumber k (n+1)
    | otherwise                                   = triangleNumber n