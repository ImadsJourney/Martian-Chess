-- #############################################################################
-- ###### TESTS                                                       ##########
-- #############################################################################

import Board
  ( Cell (Drone, Empty, Pawn, Queen),
    Player (Bottom, Top),
    Pos (Pos),
    buildBoard,
    buildFEN,
    startingFEN,
    validateFEN,
  )
import Logic
  ( Move (Move),
    droneMoves,
    makeMove,
    pawnMoves,
    playerWon,
    queenMoves,
  )
import Test.Hspec

mv :: Char -> Int -> Char -> Int -> Move
mv a r b r2 = Move (Pos a r) (Pos b r2)

main :: IO ()
main = hspec $ do
  describe "Marsian Chess - units (coverage focus)" $ do
    describe "validateFEN" $ do
      it "accepts startingFEN" $ do
        validateFEN startingFEN `shouldBe` True

      it "rejects when row count is not 8" $ do
        validateFEN "qqd1/qdp1" `shouldBe` False

      it "rejects invalid characters" $ do
        validateFEN "//////1x2/" `shouldBe` False

      it "rejects a row that has only digits (no pieces)" $ do
        validateFEN "//////22/" `shouldBe` False

      it "rejects empty string (not 8 rows)" $ do
        validateFEN "" `shouldBe` False

      it "accepts an all-empty board string" $ do
        validateFEN "///////" `shouldBe` True

      it "rejects row with wrong field count even if characters look fine" $ do
        validateFEN "p1///////" `shouldBe` False

      it "rejects row containing digit 0 or 4" $ do
        validateFEN "///////4" `shouldBe` False

    describe "buildBoard" $ do
      it "builds an 8x4 board for startingFEN" $ do
        let b = buildBoard startingFEN
        length b `shouldBe` 8
        all ((== 4) . length) b `shouldBe` True

      it "handles completely empty rows" $ do
        let b = buildBoard "///////"
        all (all (== Empty)) b `shouldBe` True

      it "expands digits correctly inside a row" $ do
        let b = buildBoard "/////2p1//"
        (b !! 5) `shouldBe` [Empty, Empty, Pawn, Empty]

      it "parses top-left corner a7 correctly" $ do
        let b = buildBoard "q3///////"
        (b !! 0) !! 0 `shouldBe` Queen

      it "parses bottom-right corner d0 correctly" $ do
        let b = buildBoard "///////3d"
        (b !! 7) !! 3 `shouldBe` Drone

    describe "buildFEN" $ do
      it "roundtrip startingFEN" $ do
        buildFEN (buildBoard startingFEN) `shouldBe` startingFEN

      it "all-empty board stays all-empty" $ do
        buildFEN (buildBoard "///////") `shouldBe` "///////"

      it "compresses consecutive empties (p _ _ d -> p2d)" $ do
        buildFEN (buildBoard "p2d///////") `shouldBe` "p2d///////"

      it "compresses empties at the end (p _ _ _ -> p3)" $ do
        buildFEN (buildBoard "p3///////") `shouldBe` "p3///////"

      it "keeps queens and drones stable in a row" $ do
        buildFEN (buildBoard "1q1d///////") `shouldBe` "1q1d///////"

      it "handles a single queen on a0" $ do
        buildFEN (buildBoard "///////q3") `shouldBe` "///////q3"

    describe "pawnMoves" $ do
      it "Bottom pawn at c2 offers b3 and d3 in starting position" $ do
        let b = buildBoard startingFEN
        pawnMoves b Bottom (Pos 'c' 2) Nothing
          `shouldMatchList` [mv 'c' 2 'b' 3, mv 'c' 2 'd' 3]

      it "returns [] if start is Empty" $ do
        let b = buildBoard startingFEN
        pawnMoves b Bottom (Pos 'a' 3) Nothing `shouldBe` []

      it "returns [] if piece is in the other zone" $ do
        let b = buildBoard "///1p2////"
        pawnMoves b Bottom (Pos 'b' 4) Nothing `shouldBe` []

      it "filters rollback after canal cross (pawn)" $ do
        let b = buildBoard "///1p2////"
            lastMv = Just (mv 'c' 3 'b' 4)
        pawnMoves b Top (Pos 'b' 4) lastMv
          `shouldNotContain` [mv 'b' 4 'c' 3]

      it "can capture across canal" $ do
        let b = buildBoard "///1p2/2p1///"
        pawnMoves b Bottom (Pos 'c' 3) Nothing
          `shouldContain` [mv 'c' 3 'b' 4]

      it "when the piece is a Drone, pawnMoves delegates to drone logic" $ do
        let b = buildBoard "//////1d2/"
        pawnMoves b Bottom (Pos 'b' 1) Nothing
          `shouldMatchList` droneMoves b Bottom (Pos 'b' 1) Nothing

      it "when the piece is a Queen, pawnMoves delegates to queen logic" $ do
        let b = buildBoard "//////1q2/"
        pawnMoves b Bottom (Pos 'b' 1) Nothing
          `shouldMatchList` queenMoves b Bottom (Pos 'b' 1) Nothing

    describe "droneMoves" $ do
      it "drone on empty-ish board can move 1 step orthogonal" $ do
        let b = buildBoard "//////1d2/"
        droneMoves b Bottom (Pos 'b' 1) Nothing
          `shouldMatchList` [ mv 'b' 1 'a' 1,
                              mv 'b' 1 'c' 1,
                              mv 'b' 1 'b' 0,
                              mv 'b' 1 'b' 2
                            ]

      it "returns [] if the selected piece is not a Drone" $ do
        let b = buildBoard "//////1p2/"
        droneMoves b Bottom (Pos 'b' 1) Nothing `shouldBe` []

      it "returns [] if drone is in other zone" $ do
        let b = buildBoard "/1d2//////"
        droneMoves b Bottom (Pos 'b' 6) Nothing `shouldBe` []

      it "range depends on pieces in that direction (cannot reach b3 here)" $ do
        let b = buildBoard "////1p2//1d2/"
            ms = droneMoves b Bottom (Pos 'b' 1) Nothing
        ms `shouldContain` [mv 'b' 1 'b' 2]
        ms `shouldNotContain` [mv 'b' 1 'b' 3]

      it "can merge into a Pawn in same zone if there are no queens" $ do
        let b = buildBoard "/////1p2/1d2/"
        droneMoves b Bottom (Pos 'b' 1) Nothing
          `shouldContain` [mv 'b' 1 'b' 2]

      it "does not merge into a Pawn in same zone if a queen exists" $ do
        let b = buildBoard "/////1p2/1d2/q3"
        droneMoves b Bottom (Pos 'b' 1) Nothing
          `shouldNotContain` [mv 'b' 1 'b' 2]

      it "can capture across canal and stops there" $ do
        let b = buildBoard "///1p2/1d2///"
            ms = droneMoves b Bottom (Pos 'b' 3) Nothing
        ms `shouldContain` [mv 'b' 3 'b' 4]
        ms `shouldNotContain` [mv 'b' 3 'b' 5]

      it "edge: drone at a0 has no moves out of bounds" $ do
        let b = buildBoard "///////d3"
            ms = droneMoves b Bottom (Pos 'a' 0) Nothing
        ms `shouldNotContain` [mv 'a' 0 '`' 0]
        ms `shouldNotContain` [mv 'a' 0 'a' (-1)]

    describe "queenMoves" $ do
      it "queen can slide and capture across canal" $ do
        let b = buildBoard "///1p2/1q2///"
        queenMoves b Bottom (Pos 'b' 3) Nothing
          `shouldContain` [mv 'b' 3 'b' 4]

      it "returns [] if selected piece is not a Queen" $ do
        let b = buildBoard "//////1d2/"
        queenMoves b Bottom (Pos 'b' 1) Nothing `shouldBe` []

      it "blocked by own-zone piece (cannot move onto it, cannot move past it)" $ do
        let b = buildBoard "/////1p2/1q2/"
            ms = queenMoves b Bottom (Pos 'b' 1) Nothing
        ms `shouldNotContain` [mv 'b' 1 'b' 2]
        ms `shouldNotContain` [mv 'b' 1 'b' 3]

      it "filters rollback after canal-crossing move (queen)" $ do
        let b = buildBoard "///1q2////"
            lastMv = Just (mv 'b' 3 'b' 4)
        queenMoves b Top (Pos 'b' 4) lastMv
          `shouldNotContain` [mv 'b' 4 'b' 3]

      it "queen has diagonal moves on empty board" $ do
        let b = buildBoard "///////q3"
            ms = queenMoves b Bottom (Pos 'a' 0) Nothing
        ms `shouldContain` [mv 'a' 0 'd' 3]

      it "queen cannot go past a capture square" $ do
        let b = buildBoard "///p3/q3///"
            ms = queenMoves b Bottom (Pos 'a' 3) Nothing
        ms `shouldContain` [mv 'a' 3 'a' 4]
        ms `shouldNotContain` [mv 'a' 3 'a' 5]

    describe "makeMove" $ do
      it "c2->b3 updates board and gives 0 points" $ do
        let b0 = buildBoard startingFEN
            (b1, pts) = makeMove b0 (mv 'c' 2 'b' 3)
        pts `shouldBe` 0
        buildFEN b1 `shouldBe` "qqd1/qdp1/dpp1//1p2/1p1d/1pdq/1dqq"

      it "capture across canal gives Pawn points (1)" $ do
        let b0 = buildBoard "///1p2/2p1///"
            (b1, pts) = makeMove b0 (mv 'c' 3 'b' 4)
        pts `shouldBe` 1
        buildFEN b1 `shouldBe` "///1p2////"

      it "capture across canal gives Drone points (2)" $ do
        let b0 = buildBoard "///1d2/1q2///"
            (b1, pts) = makeMove b0 (mv 'b' 3 'b' 4)
        pts `shouldBe` 2
        buildFEN b1 `shouldBe` "///1q2////"

      it "capture across canal gives Queen points (3)" $ do
        let b0 = buildBoard "///1q2/1q2///"
            (b1, pts) = makeMove b0 (mv 'b' 3 'b' 4)
        pts `shouldBe` 3
        buildFEN b1 `shouldBe` "///1q2////"

      it "cross-canal move to empty gives 0 points" $ do
        let b0 = buildBoard "////2p1///"
            (b1, pts) = makeMove b0 (mv 'c' 3 'b' 4)
        pts `shouldBe` 0
        buildFEN b1 `shouldBe` "///1p2////"

      it "merge Pawn+Pawn -> Drone" $ do
        let b0 = buildBoard "/////1p2/2p1/"
            (b1, pts) = makeMove b0 (mv 'c' 1 'b' 2)
        pts `shouldBe` 0
        buildFEN b1 `shouldBe` "/////1d2//"

      it "merge Pawn+Drone -> Queen" $ do
        let b0 = buildBoard "/////1d2/2p1/"
            (b1, pts) = makeMove b0 (mv 'c' 1 'b' 2)
        pts `shouldBe` 0
        buildFEN b1 `shouldBe` "/////1q2//"

      it "normal move in same zone keeps piece type (fallback branch)" $ do
        let b0 = buildBoard "//////1p2/"
            (b1, pts) = makeMove b0 (mv 'b' 1 'a' 2)
        pts `shouldBe` 0
        buildFEN b1 `shouldBe` "/////p3//"

    describe "playerWon" $ do
      it "returns Nothing if neither zone is empty" $ do
        let b = buildBoard startingFEN
        playerWon b Bottom 0 0 `shouldBe` Nothing

      it "Top zone empty -> points decide winner" $ do
        let bEnd = buildBoard "///////p3"
        playerWon bEnd Bottom 2 1 `shouldBe` Just Top
        playerWon bEnd Bottom 1 2 `shouldBe` Just Bottom

      it "Bottom zone empty -> points decide winner" $ do
        let bEnd = buildBoard "p3///////"
        playerWon bEnd Top 3 0 `shouldBe` Just Top
        playerWon bEnd Top 0 3 `shouldBe` Just Bottom

      it "tie on ended board -> last player wins" $ do
        let bEnd = buildBoard "///////p3"
        playerWon bEnd Bottom 1 1 `shouldBe` Just Bottom

      it "all-empty board counts as ended (tie -> last player)" $ do
        let bEnd = buildBoard "///////"
        playerWon bEnd Top 0 0 `shouldBe` Just Top
        playerWon bEnd Bottom 0 0 `shouldBe` Just Bottom
