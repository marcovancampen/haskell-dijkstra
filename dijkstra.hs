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
mergeSort[] = [] --returned leeg
mergeSort[x] = [x] -- returned single waarde als er ook maar 1 terug komt. is zodat de loop niet infite door gaat
mergeSort xs = merge (mergeSort firstHalf) (mergeSort lastHalf) -- stuurt de first en last half weer terug door de merge sort heen tot dat het enkele waarde zijn. daarna gaat deze naar de merge functie
    where (firstHalf, lastHalf) = splitInHalf xs --splits de lijst in 2e en assigned deze naar de variable firsthalf en last half


merge :: [Int] ->[Int] -> [Int]
merge xs [] = xs -- return xs als de 2e list leeg is
merge [] ys = ys -- return ys als de 1e list leeg is
merge (x:xs) (y:ys) = -- splits it in (x:xs) (y:yes) 2 parts de eerste index en de rest van de list. dus je krijg x en y.
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
