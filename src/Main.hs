module Main where

import System.Environment
import System.Exit
import Hev

main = do
    args <- getArgs
    case args of
        ["test"] ->
            case test of
                Nothing -> exitWith ExitSuccess
                Just error -> do
                    putStr "Internal test failed: "
                    putStrLn error
                    exitWith $ ExitFailure 1
        [command, fileName] -> do
            c <- readFile fileName
            case command of
                "parse" ->
                    putStr $ show $ parse c
                "compile" ->
                    putStr $ show $ compile c
                "getbinding" ->
                    putStr $ show $ getBinding c (
                        UnifierBinding "-" TreeLeaf (
                            UnifierBinding "+" (TreeBranch TreeLeaf TreeLeaf) UnifierNil
                        )
                    )
                "run" ->
                    putStr $ show $ run $ compile c
