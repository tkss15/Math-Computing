{-# LANGUAGE BlockArguments #-}
import System.IO
import Data.Array.ST
import Control.Monad
import Data.Array.Unboxed
import Debug.Trace
import Control.Monad.ST
import Data.Time.Clock ( diffUTCTime, getCurrentTime )
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
-- modify 
-- foldl
-- determinate3 :: Mat -> ST s Double
-- determinate3 mat = do
--             let n = rows mat
--             matSign <- newSTRef 1
--             detValue <- newSTRef 1
--             arr <- thaw (arr mat) :: ST s (STUArray s Int Double)
--             forM_ [0 .. (n - 2)] $ \i -> do
--                 elements <- mapM (readArray arr) [j * n + i  | j <- [0 .. n - 1]]
--                 let firstNelement = zip [j * n + i  | j <- [0 .. n - 1]] elements
--                 let maxElement = foldl (\acc (idx, val) -> if val > snd acc then (idx, val) else acc) (0,0) firstNelement
--                 let (maxIdx, maxVal) = maxElement
--                 when (maxIdx /= 0) $ do
--                         let swapIdx1 = i * n
--                             swapIdx2 = maxIdx * n
--                         elementIdx1 <- mapM (\k -> readArray arr (swapIdx1 + k)) [0 .. n - 1]
--                         elementIdx2 <- mapM (\k -> readArray arr (swapIdx2 + k)) [0 .. n - 1]
--                         mapM_ (\(idx1, val1) -> writeArray arr idx1 val1) (zip (swapIdx2 + [0 .. n - 1]) elementIdx1)
--                         writeArray arr (swapIdx1 + [0 .. n - 1]) elementIdx2
--                         writeArray arr (swapIdx2 + [0 .. n - 1]) elementIdx1
--                         modifySTRef matSign (* (-1))

--                 trace (show firstNelement) $ return ()
--             readSTRef detValue

determinate4 :: Mat -> ST s Double
determinate4 mat = do
            let n = rows mat
            matSign <- newSTRef 1
            detValue <- newSTRef 1
            arr <- thaw (arr mat) :: ST s (STUArray s Int Double)
            forM_ [0 .. (n - 2)] $ \i -> do
                luelement <- readArray arr (i * n + i)
                when (luelement == 0) $ do
                    elements <- mapM (readArray arr) [j * n + i  | j <- [0 .. n - 1]]
                    let firstNelement = zip [j * n + i  | j <- [0 .. n - 1]] elements
                    let maxElement = foldl (\acc (idx, val) -> if val > snd acc then (idx, val) else acc) (0,0) firstNelement
                    let (maxIdx, maxVal) = maxElement
                    when (maxIdx /= 0) $ do
                        let swapIdx1 = i * n
                            swapIdx2 = maxIdx * n
                        trace (show swapIdx1) $ return ()
                        elementIdx1 <- mapM (\k -> readArray arr (swapIdx1 + k)) [0 .. n - 1]
                        elementIdx2 <- mapM (\k -> readArray arr (swapIdx2 + k)) [0 .. n - 1]
                        trace (show elementIdx1 ++ " ") $ return ()
                        trace (show elementIdx2) $ return ()
                        forM_ [0 .. n - 1] $ \k -> do
                            let idx1 = swapIdx1 + k
                            let idx2 = swapIdx2 + k
                            swapElements arr idx1 idx2
                        modifySTRef matSign (* (-1))
                diagElement <- readArray arr (i * n + i)
                elements <- mapM (readArray arr) [j * n + i  | j <- [(i + 1) .. n - 1]]
                let firstNelement = zip [j * n + i  | j <- [0 .. n - 1]] elements
                let factorElements = map (\(idx, value) -> (idx, value / diagElement)) firstNelement
                trace (show factorElements) $ return ()
                forM_ [i + 1 .. (n - 1)] $ \j -> do
                    let idx = j * n + i
                    currentElement <- readArray arr idx
                    let factor = currentElement / diagElement
                    forM_ [i + 1 .. (n - 1)] $ \k -> do
                        let idx1 = j * n + k
                        let idx2 = i * n + k
                        currentElement <- readArray arr idx1
                        diagElement <- readArray arr idx2
                        writeArray arr idx1 (currentElement - factor * diagElement)
            forM_ [0 .. (n - 1)] $ \k -> do
                val <- readArray arr (k * n + k)
                modifySTRef detValue (*val)
            sign <- readSTRef matSign
            det <- readSTRef detValue
            let finaldet = sign * det
            return finaldet  
determinate3 :: Mat -> ST s Double
determinate3 mat = do
            let n = rows mat
            matSign <- newSTRef 1
            detValue <- newSTRef 1
            arr <- thaw (arr mat) :: ST s (STUArray s Int Double)
            forM_ [0 .. (n - 2)] $ \i -> do
                luelement <- readArray arr (i * n + i)
                when (luelement == 0) $ do
                    elements <- mapM (readArray arr) [j * n + i  | j <- [0 .. n - 1]]
                    let firstNelement = zip [j * n + i  | j <- [0 .. n - 1]] elements
                    let maxElement = foldl (\acc (idx, val) -> if val > snd acc then (idx, val) else acc) (0,0) firstNelement
                    let (maxIdx, maxVal) = maxElement
                    when (maxIdx /= 0) $ do
                        let swapIdx1 = i * n
                            swapIdx2 = maxIdx * n
                        forM_ [0 .. n - 1] $ \k -> do
                            let idx1 = swapIdx1 + k
                            let idx2 = swapIdx2 + k
                            swapElements arr idx1 idx2
                        modifySTRef matSign (* (-1))
                diagElement <- readArray arr (i * n + i)
                forM_ [i + 1 .. (n - 1)] $ \j -> do
                    let idx = j * n + i
                    currentElement <- readArray arr idx
                    let factor = currentElement / diagElement
                    forM_ [i + 1 .. (n - 1)] $ \k -> do
                        let idx1 = j * n + k
                        let idx2 = i * n + k
                        currentElement <- readArray arr idx1
                        diagElement <- readArray arr idx2
                        writeArray arr idx1 (currentElement - factor * diagElement)
            forM_ [0 .. (n - 1)] $ \k -> do
                val <- readArray arr (k * n + k)
                modifySTRef detValue (*val)
            sign <- readSTRef matSign
            det <- readSTRef detValue
            let finaldet = sign * det
            return finaldet  
    
determinate :: Mat -> ST s Double
determinate mat = do
            let n = rows mat
            matSign <- newSTRef 1
            detValue <- newSTRef 1
            arr <- thaw (arr mat) :: ST s (STUArray s Int Double)
            forM_ [0 .. (n - 2)] $ \i -> do
                luelement <- readArray arr (i * n + i)
                when (luelement == 0) $ do
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
                            swapElements arr idx1 idx2
                        modifySTRef matSign (* (-1))
                diagElement <- readArray arr (i * n + i)
                forM_ [i + 1 .. (n - 1)] $ \j -> do
                    let idx = j * n + i
                    currentElement <- readArray arr idx
                    let factor = currentElement / diagElement
                    forM_ [i + 1 .. (n - 1)] $ \k -> do
                        let idx1 = j * n + k
                        let idx2 = i * n + k
                        currentElement <- readArray arr idx1
                        diagElement <- readArray arr idx2
                        writeArray arr idx1 (currentElement - factor * diagElement)
            forM_ [0 .. (n - 1)] $ \k -> do
                val <- readArray arr (k * n + k)
                modifySTRef detValue (*val)
            sign <- readSTRef matSign
            det <- readSTRef detValue
            let finaldet = sign * det
            return finaldet


swapElements :: STUArray s Int Double -> Int -> Int -> ST s ()
swapElements arr idx1 idx2 = do
    val1 <- readArray arr idx1
    val2 <- readArray arr idx2
    writeArray arr idx1 val2
    writeArray arr idx2 val1

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
createMatrix :: Int -> [a] -> [[a]]
createMatrix n xs = [take n (drop (n*i) xs) | i <- [0..n-1]]
main = do
    -- let input_file = "D:/Coding/Python/MathComputing/det_matrix(800 x 800)_out.txt"
    -- f <- openFile input_file ReadMode
    -- -- Convert the contents to a list of lists of Floats
    -- contents <- hGetContents f
    -- let n = 800
    -- let arrayWords = wordsWhen (==',') contents
    -- let first = map read arrayWords :: [Double]
    let l1 = [[2,-4],[3,7]] :: [[Double]]
    -- let a = createMatrix n first
    let c = fromList l1
    -- let f = runST $ determinate2 c
    tt_start <- getCurrentTime
    let e = runST $ determinate4 c
    print e
    tt_end <- getCurrentTime
    print $ diffUTCTime tt_end tt_start
    let f = runST $ determinate c
    print f
