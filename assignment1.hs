-- module Main where

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

import Data.Char
import Data.List
import Data.Maybe

-- | Model

type Field = String
type Row   = [Field]
type Table = [Row]

-- | Main

-- main :: IO ()
-- main = interact (unlines . exercise . lines)

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
printRow = undefined

-- * Exercise 5

columnWidths :: Table -> [Int]
columnWidths = undefined

-- * Exercise 6

printTable :: Table -> [String]
printTable table@(header:rows)
    = undefined

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
            | null filtered  = table
            | otherwise = filtered
            where
                -- The table filtered by column value
                filtered = filterTable index
-- * Exercise 8
project :: [Field] -> Table -> Table
project columns table@(header:_) = transpose $ removeIndex $ sortByProjectionIndex $ filteredCols $ addIndex $ transpose table
    where
        -- Sort the list by the projection order requested
        sortByProjectionIndex :: [([Field], Int)] -> [([Field], Int)]
        sortByProjectionIndex = sortBy (\(_, indexOfA) (_, indexOfB) -> compare (projectionIndexOf indexOfA) (projectionIndexOf indexOfB))
            where   indexes = colIndexesToInclude
                    projectionIndexOf index = fromJust $ elemIndex index indexes

        -- Take a list of items with indexes and return only the item
        removeIndex :: [([a], Int)] -> [[a]]
        removeIndex = map fst -- Equal to (a, _) -> a

        -- Filter the a list to include only the columns in the projection
        filteredCols :: [([a], Int)] -> [([a], Int)]
        filteredCols = filter (\(_, index) -> index `elem` colIndexesToInclude)

        -- Add an index to each item in the list
        addIndex :: [[Field]] -> [([Field], Int)]
        addIndex x = zip x [0..]

        -- Compute the indexes of columns to keep
        colIndexesToInclude :: [Int]
        colIndexesToInclude = mapMaybe (`elemIndex` header) columns