module Main where
import Data.List
main = interact work 
  where work text = intercalate " / " (map reverse(lines text))
