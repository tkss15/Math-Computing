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
fromList :: [[Double]] -> Mat
fromList l = Mat {arr = listArray (0, m * n - 1) (concat l), rows = m, cols = n}
    where
        m = length l
        n = length (head l)

createIdentity :: Int -> Mat
createIdentity n = Mat {arr = listArray (0, n * n - 1) (concatMap (\i -> replicate i 0 ++ [1] ++ replicate (n - i - 1) 0) [0 .. n - 1]), rows = n, cols = n}
-- Given Mat nxn we want from i postion until end in colum i 

stSum3 :: Mat -> Mat
stSum3 mat = Mat { arr = arr_, rows = n, cols = n}
    where 
        n = rows mat
        arr1 = arr mat
        arr_ = runSTUArray $ do 
            identity <- thaw (arr (createIdentity n)) :: ST s (STUArray s Int Double)
            arr <- thaw (arr mat) :: ST s (STUArray s Int Double)
            forM_ [0 .. (n - 1)] $ \i -> do
                maxElement <- newSTRef (0,0)  -- Move maxElement inside the loop
                forM_ [i .. (n-1)] $ \j -> do
                    let idx = j * n + i
                    currentElement <- readArray arr idx
                    maxElement' <- readSTRef maxElement
                    when (currentElement > fst maxElement') $
                        writeSTRef maxElement (currentElement, j)
                (maxVal, maxIdx) <- readSTRef maxElement  -- Only read once after the loop
                when (maxIdx /= 0) $ do
                    let swapIdx1 = i * n
                        swapIdx2 = maxIdx * n
                    forM_ [0 .. n - 1] $ \k -> do
                        let idx1 = swapIdx1 + k
                        let idx2 = swapIdx2 + k
                        swapElements arr identity idx1 idx2
                    normalizeRow arr identity (i * n) i n
                    eliminateColumn arr identity i n
            return identity

swapElements :: STUArray s Int Double -> STUArray s Int Double -> Int -> Int -> ST s ()
swapElements arr identity idx1 idx2 = do
    val1 <- readArray arr idx1
    valIdentity1 <- readArray identity idx1
    val2 <- readArray arr idx2
    valIdentity2 <- readArray identity idx2
    writeArray arr idx1 val2
    writeArray arr idx2 val1
    writeArray identity idx1 valIdentity2
    writeArray identity idx2 valIdentity1

normalizeRow :: STUArray s Int Double -> STUArray s Int Double -> Int -> Int -> Int -> ST s ()
normalizeRow arr identity startIdx i n = do
    let idx = startIdx + i
    firstElement <- readArray arr idx
    forM_ [0 .. n - 1] $ \j -> do
        let idx' = startIdx + j
        currentElement <- readArray arr idx'
        currentIdentity <- readArray identity idx'
        writeArray arr idx' (currentElement / firstElement)
        writeArray identity idx' (currentIdentity / firstElement)


eliminateColumn :: STUArray s Int Double -> STUArray s Int Double -> Int -> Int -> ST s ()
eliminateColumn arr identity i n = do
    forM_ [0 .. n - 1] $ \j -> do
        when( j /= i) do
            factor <- readArray arr (j * n + i)
            forM_ [0 .. n - 1] $ \k -> do
                let idx1 = j * n + k
                let idx2 = i * n + k
                currentIdentity <- readArray identity idx1
                currentElement <- readArray arr idx1
                pivotIdentity <- readArray identity idx2
                pivotElement <- readArray arr idx2
                writeArray identity idx1 (currentIdentity - (factor * pivotIdentity))   
                writeArray arr idx1 (currentElement - (factor * pivotElement))   
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
createMatrix :: Int -> [a] -> [[a]]
createMatrix n xs = [take n (drop (n*i) xs) | i <- [0..n-1]]
main = do
    let input_file = "D:/Coding/Python/MathComputing/inv_eig_matrix(800 x 800)_out.txt"
    f <- openFile input_file ReadMode
    -- Convert the contents to a list of lists of Floats
    contents <- hGetContents f
    let n = 800
    let arrayWords = wordsWhen (==',') contents
    let first = map read arrayWords :: [Double]
    -- let l1 = [[-1,1.5],[1,-1]] :: [[Double]]
    let a = createMatrix n first
    let c = fromList a
    let f = stSum3 c
    print f