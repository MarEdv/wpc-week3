module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)

import WpcWeek3.NormalMazeSolver.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ solverSuite
            ]
