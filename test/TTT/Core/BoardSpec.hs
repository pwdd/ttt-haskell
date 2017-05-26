module TTT.Core.BoardSpec (main, spec) where

import Test.Hspec

import TTT.Core.Board as Board

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let e = Board.emptySpot
  let x = 'x'

  describe "newBoard" $ do

    it "is a board with empty spaces" $
      Board.newBoard 9 `shouldBe` [ e, e, e
                                  , e, e, e
                                  , e, e, e
                                  ]

    it "has only empty spots" $ do
      let emptyBoard = Board.newBoard 9
      all (== head emptyBoard) (tail emptyBoard) `shouldBe` True

  describe "placeMarker" $ do
    let x = 'x'

    it "places a marker on an empty board" $
      Board.placeMarker 2 x [ e, e, e
                            , e, e, e
                            , e, e, e
                            ]
        `shouldBe`
        [ e, e, x
        , e, e, e
        , e, e, e
        ]

    it "places a marker on an any board" $
      Board.placeMarker 8 x [ x, x, e
                            , e, e, e
                            , e, e, e
                            ]
        `shouldBe`
        [ x, x, e
        , e, e, e
        , e, e, x
        ]


  describe "isFull" $ do

    it "is false if there is any empty spot" $
      Board.isFull [x, x, x , e, x, x , x, x, x] `shouldBe` False

    it "is true if there is no empty spots" $
      Board.isFull [x, x, x , x, x, x , x, x, x] `shouldBe` True

  describe "isEmpty" $ do

    it "is false if there is any spot with a marker" $
      Board.isEmpty [e, e, e , e, x, e , e, e, e] `shouldBe` False

    it "is true if there is no empty spots" $
      Board.isEmpty [e, e, e , e, e, e , e, e, e] `shouldBe` True

  describe "availableSpots" $ do

    it "is an empty list when board is full" $
      null (Board.availableSpots [x, x, x , x, x, x , x, x, x]) `shouldBe` True

    it "is a list of the indices of empty spots" $
      Board.availableSpots [x, e, x , x, e, x , x, x, e] `shouldBe` [ 1, 4, 8 ]


  describe "rows" $

    it "is a nested list with indices of board rows" $
      Board.rows 3 `shouldBe` [ [0, 1, 2]
                              , [3, 4, 5]
                              , [6, 7, 8]
                              ]

  describe "columns" $

    it "is a nested list with indices of board columns" $
      Board.columns 3 `shouldBe` [ [0, 3, 6]
                                 , [1, 4, 7]
                                 , [2, 5, 8]
                                 ]

  describe "diagonals" $

    it "is a nested list with indices of board diagonals" $
      Board.diagonals 3 `shouldBe` [[0, 4, 8], [2, 4, 6]]

