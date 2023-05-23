f :: a -> b -> c -> d
f :: a -> (b -> (d))

--Currying
add :: Int -> Int -> Int
add x y = x + y
add x = (\y -> x + y)
add = (\x -> (\y -> x + y))