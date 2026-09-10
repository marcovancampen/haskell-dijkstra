import Debug.Trace

fibbonacci :: [Int]
fibbonacci = 0 : 1 : zipWith (+) fibbonacci (tail fibbonacci)

fibbonacci2 :: Int -> Int
fibbonacci2 n = fibbonacci  !! n


listLast :: [Int] -> Int
listLast[x] = x
listLast (x:xs) = listLast xs 

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort lower ++ [x] ++ quickSort higher
    where
        lower = filter(<=x) xs
        higher = filter(>x) xs

quickSortString :: [String] -> [String]
quickSortString [] = []
quickSortString(x:xs) = quickSortString lower ++ [x] ++ quickSortString higher
    where 
        lower = filter(<=x) xs
        higher = filter(>x) xs


mergeSort:: [Int] -> [Int]
mergeSort[] = []
mergeSort[x] = [x]
mergeSort list = 
    let (leftHalf, rightHalf) = splitInHalf list
        debug = show leftHalf ++  " " ++ show rightHalf
    in trace debug (mergeSort leftHalf ++ mergeSort rightHalf)


splitInHalf :: [Int] -> ([Int], [Int])
splitInHalf xs = xs splitAt midpoint xs
    where midpoint = length xs `div` 2 

main :: IO()
main = do 
    print (fibbonacci2 10 )
    let x = [32,4,23,234,24,635,346,346,7,1]
    let s = ["fortnite","banaan","abc","duiy","duif"]
    print (quickSort x )
    print (quickSortString s)