-- Copyright 2023 Tobias de Bruijn

module Main where
import Data.List ( intercalate )

main = interact ( intercalate " / " . map reverse . lines)