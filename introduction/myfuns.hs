import Data.List

frequency :: [Char] -> [(Int, Char)]
frequency sentence = [(length xs, head xs) | xs <- groups]
    where letters = [c | c <- sentence, c /= ' ']
          groups = group (sort letters)

refraction :: Double -> Double -> Double -> Double
refraction θ n1 n2 = 2* asin (sin θ * n2 / n1)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

tellList :: Show a => [a] -> String
tellList [] = "This is an empty list"
tellList (x:xs) = "The head is " ++ show x ++ 
                  " the rest of it is " ++ show xs

firstThree :: [a] -> Maybe (a,a,a)
firstThree (x:y:z:_) = Just (x,y,z)
firstThree xs = Nothing

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

something = "hehe"

sharpRefraction :: Double -> Double -> Double
sharpRefraction = refraction 0.1

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


product xs = foldl (*) 1 xs

any xs = foldl (||) False xs

map f xs = foldr (\x acc -> f x:acc) [] xs

filter f xs = foldr (\x acc -> if f x then f:acc else acc) [] xs

solveRPN :: String -> Double
solveRPN exp = head (foldl foldingFunction [] (words exp))
    where  foldingFunction (x:y:ys) "*" = (y * x):ys
           foldingFunction (x:y:ys) "+" = (y + x):ys
           foldingFunction (x:y:ys) "-" = (y - x):ys
           foldingFunction xs numberString = read numberString:xs

data List a = a `Cons` (List a) | Empty deriving (Show)
