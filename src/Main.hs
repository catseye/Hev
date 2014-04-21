module Main where

import System.Environment
import Hev

main = do
    [command, fileName] <- getArgs
    c <- readFile fileName
    case command of
        "parse" ->
            putStr $ show $ parse c
        "compile" ->
            putStr $ show $ compile c
        "getbinding" ->
            putStr $ show $ getBinding c (UnifierBinding "-" TreeLeaf (UnifierBinding "+" (TreeBranch TreeLeaf TreeLeaf) UnifierNil))
        "run" ->
            putStr $ show $ run $ compile c
