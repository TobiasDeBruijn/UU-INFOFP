module Main where
import Data.List

main = interact ( intercalate " / " . map reverse . lines)