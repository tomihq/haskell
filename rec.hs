fibonacci :: (Eq a, Num a) => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

take' :: (Eq a, Num a) => [a] -> a -> [a]
take' _ 0 = []
take' [] _ = []
take' (x:xs) 1 = [x]
take' (x:xs) n = x:(take' xs (n-1))