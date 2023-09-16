-- Copyright 2023 Tobias de Bruijn

-- module Main where

import Data.Char
import Data.List
import Data.Maybe

-- | Model

type Field = String
type Row   = [Field]
type Table = [Row]

-- | Main

main :: IO ()
main = interact (unlines . exercise . lines)

exercise :: [String] -> [String]
exercise = printTable
         . project ["last", "first", "salary"]
         . select "gender" "male"
         . parseTable

-- | Parsing

-- * Exercise 1

parseTable :: [String] -> Table
parseTable = map words

-- | Printing

-- * Exercise 2

printWord :: Int -> String
printWord numChars = replicate numChars '-'

printLine :: [Int] -> String
printLine = (++ "+") . ("+" ++) . intercalate "+" . map printWord

-- * Exercise 3

printField :: Int -> String -> String
printField len content
  | all isDigit content = replicate (len - length content) ' ' ++ content
  | otherwise           = content ++ replicate (len - length content) ' '

-- * Exercise 4              

printRow :: [(Int, String)] -> String
printRow = ("|" ++ ) . (++ "|") . intercalate "|" . map (uncurry printField)

-- * Exercise 5

columnWidths :: Table -> [Int]
columnWidths = map (maximum . map length) . transpose

-- * Exercise 6

printTable :: Table -> [String]
printTable [] = error "Cannot print empty table";
printTable table@(header:rows) = line : headerRow : line : restOfRows ++ [line]
    where
        colWidth            = columnWidths table
        line                = printLine colWidth
        headerContentsUpper = map (map toUpper) header
        headerRow           = printRow (zip colWidth headerContentsUpper)
        restOfRows          = map (printRow . zip colWidth ) rows

-- | Querying

-- * Exercise 7

select :: Field -> Field -> Table -> Table
select column value table@(header:rows) = maybe table filterTableOrFullTable columnIndex
    where
        -- Index of the column to match
        columnIndex             = elemIndex column header
        -- Table with rows not matching the predicate filtered out
        filterTable index       = filter (\row -> row !! index == map toLower value) rows
        -- If the filtered list is empty, return the full table, otherwhise return the filtered table
        filterTableOrFullTable index
            | length filtered == 0  = table
            | otherwise = header : filtered
            where
                -- The table filtered by column value
                filtered = filterTable index
-- * Exercise 8
project :: [Field] -> Table -> Table
project columns table@(header:_) = transpose $ removeIndex $ sortByProjectionIndex $ filteredCols $ addIndex $ transpose table
    where
        -- Sort the list by the projection order requested
        sortByProjectionIndex :: [([a], Int)] -> [([a], Int)]
        sortByProjectionIndex = sortBy (\(_, a) (_, b) -> compare (projectionIndexOf a) (projectionIndexOf b))
            where   indexes = colIndexesToInclude
                    projectionIndexOf index = elemIndex index indexes

        -- Take a list of items with indexes and return only the item
        removeIndex :: [([a], Int)] -> [[a]]
        removeIndex = map fst -- Equal to (a, _) -> a

        -- Filter the a list to include only the columns in the projection
        filteredCols :: [([a], Int)] -> [([a], Int)]
        filteredCols = filter (\(_, index) -> index `elem` colIndexesToInclude)

        -- Add an index to each item in the list
        addIndex :: [[a]] -> [([a], Int)]
        addIndex x = zip x [0..]

        -- Compute the indexes of columns to keep
        colIndexesToInclude :: [Int]
        colIndexesToInclude = mapMaybe (`elemIndex` header) columns