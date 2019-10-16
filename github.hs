getd :: [a] -> Int -> a
getd [] n = error "no elements in the list"
getd (x:xs) 0 = x
getd (x:xs) n = getd xs (n - 1)

headd :: [a] -> a
headd [] = error "clean list"
headd (x:xs) = x

lastd :: [a] -> a
lastd [] = error "clean list"
lastd [x] = x
lastd (x:xs) = lastd xs

taild :: [a] -> [a]
taild [] = error "clean list"
taild (x:xs) = xs

initd :: [a] -> [a]
initd [] = error "clean list"
initd [x] = []
initd (x:xs) =x : initd xs

reversed :: [Int] -> [Int]
reversed [] = []
reversed (x:xs) = (reversed xs) ++ [x]

concatd :: [a] -> [a] -> [a]
concatd a [] = a
concatd a (x:xs) =concatd ( a ++ [x]) xs

lengthd :: [a] -> Int
lengthd [] = 0
lengthd (x:xs) = (lengthd xs) +1

appendd :: [a] -> a -> [a]
appendd [] k = [k]
appendd a k = a ++ [k]

dropd :: Int -> [a] -> [a]
dropd 0 a = a
dropd n (x:xs) = dropd (n - 1) xs

taked :: Int-> [a] -> [a]
taked 0 a = []
taked n (x:xs) = x:taked (n - 1) xs

nulld :: [a] -> Bool
nulld [] = True
nulld xs = False

splitAtd :: Int -> [a] -> ([a],[a])
splitAtd n a = taked (n-1) a, dropd (n-1) a

