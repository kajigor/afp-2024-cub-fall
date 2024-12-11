module MatrixIO where

import Data.Array.IO.Safe
    ( readArray, writeArray, MArray(getBounds, newArray), IOArray )
import Control.Monad.Trans ( MonadTrans(lift) )
import Data.List (intercalate)
import Control.Monad.Trans.Maybe ( MaybeT )
import Data.List.Split ( splitOn )
import Control.Monad ( forM_, when )

-- | Reads a CSV of integers from a file and stores it into a 2D IOArray.
--   Returns the IOArray and its dimensions (rows, cols).
readCsvIntoArray :: String -> MaybeT IO (IOArray (Int, Int) Int, Int, Int)
readCsvIntoArray csvPath = do
    -- Read the entire file
    contents <- lift $ readFile csvPath
    let allLines = lines contents
    
    -- Filter out empty lines if any (just in case)
    let csvLines = filter (not . null) allLines
    
    when (null csvLines) (fail "CSV file is empty.")
    
    -- Split each line by comma and parse integers
    let parsed :: [[Int]]
        parsed = map (map read . splitOn ",") csvLines
    
    let rows = length parsed
        cols = length (head parsed)
    
    -- Verify all rows have the same length
    when (any (\r -> length r /= cols) parsed) $
        fail "Inconsistent number of columns in CSV."

    -- Create a new IOArray
    arr <- lift $ newArray ((0,0),(rows-1,cols-1)) 0
    
    -- Fill the array
    forM_ [0..rows-1] $ \r ->
        forM_ [0..cols-1] $ \c ->
            lift $ writeArray arr (r,c) (parsed !! r !! c)
    
    return (arr, rows, cols)

-- | Writes a 2D IOArray of Ints to a CSV file.
writeArrayToCsv :: FilePath -> IOArray (Int, Int) Int -> IO ()
writeArrayToCsv csvPath arr = do
    ((r0,c0),(r1,c1)) <- getBounds arr

    -- For each row, read all elements in that row, convert to String, 
    -- and create a comma-separated line.
    linesToWrite <- mapM (\r -> do
        rowVals <- mapM (\c -> readArray arr (r,c)) [c0..c1]
        return (intercalate "," (map show rowVals))
      ) [r0..r1]

    -- Write all lines to the CSV file
    writeFile csvPath (unlines linesToWrite)