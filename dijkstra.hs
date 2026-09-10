import Debug.Trace

x = 10 
y = 20
result = x + y 



addtogether :: Integer -> Integer -> Integer
addtogether x y = (+) x y

checkNumber :: Int -> String 
checkNumber x = if x >= 10
    then show x ++ " is greater than 10"
    else show x ++ " is less than 10"


sayHello :: Int -> String
sayHello 1 = show doubled
sayHello 2 = show filtered
sayHello x = "oiia " ++ show x 


-- types of an array always have to be the same, so only ints or strings for example
numbers = [1,2,3,4,5]
strings = ["hello", "goodmorning", "oiia"]

doubled = [x * 2 | x <- numbers]

filtered = [x | x <- doubled, x > 2]

filtered2 :: [Int] -> [Int]
filtered2 xs = [x | x <- xs, x `mod` 2 == 0]

factorials = [factorial x | x <- numbers]

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

fibbonacci :: Int -> Int 
fibbonacci 0 = 0
fibbonacci 1 = 1
fibbonacci n = fibbonacci(n-1) + fibbonacci(n-2)




fibbonacci2 :: [Int]
fibbonacci2 = 0:1: zipWith (+) fibbonacci2(tail fibbonacci2)

bubbleSort :: [Int] -> [Int]
bubbleSort[] = []
bubbleSort[x] = [x]
bubbleSort (x:y:xs) = 
    if x > y
        then y:bubbleSort(x:xs)
        else x:bubbleSort(y:xs)

bubbleLoop::[Int] -> [Int]
bubbleLoop xs =
    let bubble = bubbleSort xs
    in if xs == bubble
        then xs
        else bubbleLoop bubble

quickSort::[Int] -> [Int]
quickSort[] = []
quickSort (x:xs) = quickSort small ++ [x] ++ quickSort high
    where 
        high = filter (>x) xs
        small = filter (<=x) xs


mergeSort::[Int] -> [Int]
mergeSort[] = []
mergeSort[x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort lastHalf)
    where (firstHalf, lastHalf) = splitInHalf xs


merge :: [Int] ->[Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = 
    if x <= y
        then x : merge xs (y:ys)
    else y: merge (x:xs) ys


splitInHalf:: [Int] -> ([Int], [Int])
splitInHalf xs = splitAt midpoint xs
    where
        midpoint = length xs `div` 2

main:: IO()
main = do  
    let list = [4,3,5,2]
    -- putStrLn "enter which entry of fibbonacci you want (10 = 55)"
    -- input <- getLine
    -- let x = (read input :: Int)
    -- print (take x fibbonacci2)
    -- print (filtered2 (take x fibbonacci2))
    print(mergeSort list)
