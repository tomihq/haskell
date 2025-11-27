fibonacci :: (Eq a, Num a) => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

take' :: (Eq a, Num a) => a -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' 1 (x:xs) = [x]
take' n (x:xs) = x:(take' (n-1) xs)