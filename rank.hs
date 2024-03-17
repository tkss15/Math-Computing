{-# LANGUAGE BlockArguments #-}
import System.IO
import Data.Array.ST
import Control.Monad
import Data.Array.Unboxed
import Debug.Trace
import Control.Monad.ST
import Data.Time.Clock
import Data.Char
import Data.STRef ( modifySTRef, newSTRef, readSTRef, writeSTRef )
data Mat = Mat { arr :: UArray Int Double, rows :: Int, cols :: Int}

instance Show Mat where
    show :: Mat -> String
    show Mat { arr = arr_, rows = m, cols = n} = toStr (elems arr_) m
        where
            toStr :: [ Double] -> Int -> String
            toStr l m | m == 1 = show l
                      | otherwise = show (take n l) ++ "\n" ++ toStr (drop n l) (m - 1)

get :: Mat -> Int -> Int ->  Double
get mat i j = arr mat ! (i * cols mat + j)

fromList :: [[Double]] -> Mat
fromList l = Mat {arr = listArray (0, m * n - 1) (concat l), rows = m, cols = n}
    where
        m = length l
        n = length (head l)

createMat :: Int -> Mat
createMat n = Mat {arr = listArray (0, n * n - 1) (replicate (n * n) 0), rows = n, cols = n}

createIdentity :: Int -> Mat
createIdentity n = Mat {arr = listArray (0, n * n - 1) (concatMap (\i -> replicate i 0 ++ [1] ++ replicate (n - i - 1) 0) [0 .. n - 1]), rows = n, cols = n}
-- Given Mat nxn we want from i postion until end in colum i 

matrixElimination :: Mat -> Int -> Mat
matrixElimination mat i = Mat { arr = _arr , rows = n, cols = n}
    where
        n = rows mat
        arr1 = arr mat
        _arr = runSTUArray $ do
            arr <- newArray (0, n*n - 1) 0
            forM_ [0 .. n*n-1] $ \i -> do
                writeArray arr i ( arr1 ! i)
            forM_ [i + 1 .. n - 1] $ \j -> do
                let factor = get mat j i
                forM_ [0 .. n - 1] $ \k -> do
                    let temp = get mat j k - get mat i k * factor
                    writeArray arr (j * n + k) temp
            return arr
matrixSwapRow :: Mat -> Int -> Int -> Mat
matrixSwapRow mat i j = Mat { arr = _arr, rows = rows mat, cols = cols mat}
    where
        n = rows mat
        arr1 = arr mat
        _arr = runSTUArray $ do
            arr <- newArray (0, n * n - 1) 0
            forM_ [0 .. n * n - 1] $ \k -> do
                writeArray arr k (arr1 ! k)
            forM_ [0 .. n - 1] $ \k -> do
                let temp = arr1 ! (i * n + k)
                writeArray arr (i * n + k) (arr1 ! (j * n + k))
                writeArray arr (j * n + k) temp
            return arr


stSum4 :: Mat -> ST s Int
stSum4 mat = do
    let n = rows mat
    arr <- thaw (arr mat) :: ST s (STUArray s Int Double)
    rankMat <- newSTRef 0

    forM_ [0 .. (n - 1)] $ \i -> do
        maxElement <- newSTRef (0,0)  -- Move maxElement inside the loop
        forM_ [i .. (n-1)] $ \j -> do
            let idx = j * n + i
            currentElement <- readArray arr idx
            maxElement' <- readSTRef maxElement
            when (currentElement > fst maxElement') $
                writeSTRef maxElement (currentElement, j)
        (maxVal, maxIdx) <- readSTRef maxElement  -- Only read once after the loop
        -- elements <- mapM (readArray arr) [j * n + i  | j <- [i .. n - 1]]
        -- let firstNelement = zip [i .. n - 1] elements
        -- let (fidx, fval) = head firstNelement
        -- let maxElement = foldl (\acc (idx, val) -> if val > snd acc then (idx, val) else acc) (fidx,fval) firstNelement
        -- let (maxIdx, maxVal) = maxElement
        when (maxIdx /= 0) $ do
            modifySTRef rankMat (+1)
            let swapIdx1 = i * n
                swapIdx2 = maxIdx * n
            forM_ [0 .. n - 1] $ \k -> do
                let idx1 = swapIdx1 + k
                let idx2 = swapIdx2 + k
                swapElements arr idx1 idx2
            normalizeRow arr (i * n) i n
            eliminateColumn arr i n

    readSTRef rankMat
stSum3 :: Mat -> ST s Int
stSum3 mat = do
    let n = rows mat
    arr <- thaw (arr mat) :: ST s (STUArray s Int Double)
    rankMat <- newSTRef 0

    forM_ [0 .. (n - 1)] $ \i -> do
        -- maxElement <- newSTRef (0,0)  -- Move maxElement inside the loop
        -- forM_ [i .. (n-1)] $ \j -> do
        --     let idx = j * n + i
        --     currentElement <- readArray arr idx
        --     maxElement' <- readSTRef maxElement
        --     when (currentElement > fst maxElement') $
        --         writeSTRef maxElement (currentElement, j)

        -- (maxVal, maxIdx) <- readSTRef maxElement  -- Only read once after the loop
        elements <- mapM (readArray arr) [j * n + i  | j <- [i .. n - 1]]
        let firstNelement = zip [i .. n - 1] elements
        let (fidx, fval) = head firstNelement
        let maxElement = foldl (\acc (idx, val) -> if val > snd acc then (idx, val) else acc) (fidx,fval) firstNelement
        let (maxIdx, maxVal) = maxElement
        when (maxIdx /= 0) $ do
            modifySTRef rankMat (+1)
            let swapIdx1 = i * n
                swapIdx2 = maxIdx * n
            forM_ [0 .. n - 1] $ \k -> do
                let idx1 = swapIdx1 + k
                let idx2 = swapIdx2 + k
                swapElements arr idx1 idx2
            normalizeRow arr (i * n) i n
            eliminateColumn arr i n

    readSTRef rankMat

swapElements :: STUArray s Int Double -> Int -> Int -> ST s ()
swapElements arr idx1 idx2 = do
    val1 <- readArray arr idx1
    val2 <- readArray arr idx2
    writeArray arr idx1 val2
    writeArray arr idx2 val1

normalizeRow :: STUArray s Int Double -> Int -> Int -> Int -> ST s ()
normalizeRow arr startIdx i n = do
    let idx = startIdx + i
    firstElement <- readArray arr idx
    forM_ [0 .. n - 1] $ \j -> do
        let idx' = startIdx + j
        currentElement <- readArray arr idx'
        writeArray arr idx' (currentElement / firstElement)

eliminateColumn :: STUArray s Int Double -> Int -> Int -> ST s ()
eliminateColumn arr i n = do
    forM_ [(i+1) .. n - 1] $ \j -> do
        factor <- readArray arr (j * n + i)
        forM_ [0 .. n - 1] $ \k -> do
            let idx1 = j * n + k
            let idx2 = i * n + k
            currentElement <- readArray arr idx1
            pivotElement <- readArray arr idx2
            writeArray arr idx1 (currentElement - (factor * pivotElement))
stSum2 :: Mat -> ST s Int
stSum2 mat = do
    let n = rows mat  -- Assuming `rows` is a function that retrieves the number of rows in `mat`
    -- Initialize a mutable array `arr` based on `arr mat`
    arr <- thaw (arr mat) :: ST s (STUArray s Int Double)  -- `thaw` creates a mutable copy of an immutable array
    maxElementVal <- newSTRef 0
    maxElementIdx <- newSTRef 0
    rankMat <- newSTRef 0

    forM_ [0 .. (n - 1)] $ \i -> do
        writeSTRef maxElementVal 0
        writeSTRef maxElementIdx 0
        forM_ [i .. (n-1)] $ \j -> do
            currentElement <- readArray arr (j * n + i)
            maxValue <- readSTRef maxElementVal
            when (currentElement > maxValue) $ do
                writeSTRef maxElementVal currentElement
                writeSTRef maxElementIdx j
        maxIdx <- readSTRef maxElementIdx
        when (maxIdx /= 0) $ do
            modifySTRef rankMat (+1)
            forM_ [0 .. n - 1] $ \k -> do
                let idx1 = i * n + k
                let idx2 = maxIdx * n + k
                val1 <- readArray arr idx1
                val2 <- readArray arr idx2
                writeArray arr idx1 val2
                writeArray arr idx2 val1
            firstElement <- readArray arr (i * n + i)
            forM_ [0 .. n - 1] $ \j -> do
                currentElement <- readArray arr (i * n + j)
                writeArray arr (i * n + j) (currentElement / firstElement)
            forM_ [(i+1) .. n - 1] $ \j -> do
                factor <- readArray arr (j * n + i)
                forM_ [0 .. n - 1] $ \k -> do
                    currentElement <- readArray arr (j * n + k)
                    pivotElement <- readArray arr (i * n + k)
                    writeArray arr (j * n + k) (currentElement - (factor * pivotElement))
    readSTRef rankMat
stSum :: forall s. Mat -> ST s Int
stSum mat = do
    let n = rows mat
    let arr1 = arr mat
    arr <- newArray (0, n * n - 1) 0 :: ST s (STUArray s Int  Double)
    maxElement <- newSTRef (0,0)
    rankMat <- newSTRef 0
    i <- newSTRef 0
    forM_ [0 .. n * n - 1] $ \i -> do
        writeArray arr i (arr1 ! i)
    forM_ [0 .. (n - 1)] $ \i -> do
        writeSTRef maxElement (0,0)
        forM_ [i .. (n-1)] $ \j -> do
            currentElement <- readArray arr (j * n + i)
            maxElement' <- readSTRef maxElement
            when (currentElement > fst maxElement') $ do
                    writeSTRef maxElement (currentElement, j)
        maxElement' <- readSTRef maxElement
        -- forM_ [0 .. (n - 1)] $ \row -> do
        --     -- Accumulate each row's elements in a string
        --     rowString <- fmap concat $ forM [0 .. (n - 1)] $ \col -> do
        --         element <- readArray arr (row * n + col)
        --         return $ show element ++ " "
        --     -- Print the entire row at once
        --     trace rowString return ()
        -- trace (show maxElement') return ()
        when (snd maxElement' /= 0) $ do
            currentRank <- readSTRef rankMat
            writeSTRef rankMat (currentRank + 1)
            forM_ [0 .. n - 1] $ \k -> do
                temp <- readArray arr ( i * n + k)
                swapElement <- readArray arr (snd maxElement' * n + k)
                writeArray arr (i * n + k) swapElement
                writeArray arr (snd maxElement' * n + k) temp
            firstElement <- readArray arr (i * n + i)
            -- forM_ [0 .. (n - 1)] $ \row -> do
            --     -- Accumulate each row's elements in a string
            --     rowString <- fmap concat $ forM [0 .. (n - 1)] $ \col -> do
            --         element <- readArray arr (row * n + col)
            --         return $ show element ++ " "
            --     -- Print the entire row at once
            --     trace rowString return ()
            forM_ [0 .. n - 1] $ \j -> do
                currentElement <- readArray arr (i * n + j)
                writeArray arr (i * n + j) (currentElement / firstElement)
            forM_ [(i+1) .. n - 1] $ \j -> do
                factor <- readArray arr (j * n + i)
                forM_ [0 .. n - 1] $ \k -> do
                    currentElement <- readArray arr (j * n + k)
                    pivotElement <- readArray arr (i * n + k)
                    writeArray arr (j * n + k) (currentElement - (factor * pivotElement))
    readSTRef rankMat

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
createMatrix :: Int -> [a] -> [[a]]
createMatrix n xs = [take n (drop (n*i) xs) | i <- [0..n-1]]
main = do
    let input_file = "D:/Coding/Python/MathComputing/rank_matrix_out.txt"
    f <- openFile input_file ReadMode
    -- Convert the contents to a list of lists of Floats
    contents <- hGetContents f
    let n = 1000
    let arrayWords = wordsWhen (==',') contents
    let first = map read arrayWords :: [Double]
    let l1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Double]]
    -- let l2 = createMat 3
    -- let a = fromList l1
    let b = createMatrix 1000 first
    let c = fromList b
    tt_start <- getCurrentTime
    let f = runST $ stSum3 c
    print f
    tt_end <- getCurrentTime
    print $ diffUTCTime tt_end tt_start
    tt_start <- getCurrentTime
    let f = runST $ stSum4 c
    print f
    tt_end <- getCurrentTime
    print $ diffUTCTime tt_end tt_start
    -- let b = get a 0 2 
    -- let c = pivotIndex a 0
    -- let d = matrixSwapRow a 0 c
    -- let e = normalizeRow d 0
    -- let f = matrixElimination e 0
    -- print f