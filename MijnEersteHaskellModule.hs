module MijnEersteHaskellModule where
fac n = product [1..n]
boven n k = fac n `div` (fac k * fac (n-k))
pi = 3.1415927
n !^! k = fac n / (fac k * fac (n - k))
kwadraat x = x*x
somVanKwadraten lijst = sum map (kwadraat lijst)