nd :: Int -> [String]
nd 0 = [""]
nd 1 = [[x] | x <- ['0'..'9']]
nd (n + 1) = [x : y | y <- nd n, x <- ['0'..y!!0]]

mapfoldr :: (a -> b) -> ([a] -> [b])
mapfoldr f = foldr g []
  where g x ys = f x : ys

mapfoldr2 :: (a -> b) -> ([a] -> [b])
mapfoldr2 f = let g x ys = f x : ys
              in foldr g []


