module WpcWeek3.NormalMazeSolver.Test
    (solverSuite)
where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import WpcWeek3.NormalMazeSolver

solverSuite :: TestTree
solverSuite = testGroup "NormalMazeSolver"
    [
      testCase "load file 1" testLoadFile1
    , testCase "convert to maze" testConvertToMaze
    , testCase "find start" testFindStart
    , testCase "find path" testFindPath
    ]

testLoadFile1 :: Assertion
testLoadFile1 = do
          let a = ["#######", "O   # #","### # #", "#   # #", "# ### #", "#     X", "#######"]
          textMaze <- loadFile "maze.txt"
          assertEqual "" a textMaze

testConvertToMaze :: Assertion
testConvertToMaze = do
          let a = Start {
                    top = Wall,
                    right = defJunction {
                      right = defJunction {
                        right = defJunction {
                          bottom = defJunction {
                            bottom = defJunction {
                              left = defJunction {
                                left = defJunction {
                                  bottom = defJunction {
                                    bottom = defJunction {
                                      right = defJunction {
                                        right = defJunction {
                                          right = defJunction {
                                            right = defJunction {
                                              top = defJunction {
                                                top = defJunction {
                                                  top = defJunction {
                                                    top = defJunction
                                                  }
                                                }
                                              },
                                              right = End
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      },
                      left = Start {
                        top = Wall,
                        right = Wall,
                        bottom = Wall,
                        left = Wall
                      }
                     },
                    bottom = Wall,
                    left = Wall
                  }
          textMaze <- loadFile "maze.txt"
          let maze = convertToMaze textMaze
          assertEqual "" a maze

testFindStart :: Assertion
testFindStart = do
          textMaze <- loadFile "maze.txt"
          let startPoint = findStart textMaze
          assertEqual "" [(0,1)] startPoint

testFindPath :: Assertion
testFindPath = do
          textMaze <- loadFile "maze.txt"
          let maze = convertToMaze textMaze
          let path = findPath maze
          assertEqual "" ["east","east","east","south","south","west","west","south","south","east","east","east","east","east"] path

