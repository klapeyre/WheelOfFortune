module Main(main) where

    import Graphics.Gloss
    import System.Random
    import Graphics.Gloss.Data.ViewPort
    import Graphics.Gloss.Interface.Pure.Game

    width, height :: Int
    width = 1200
    height = 600

    window :: Display
    window = InWindow "Wheel of Fortune!" (width, height) (0,0)

    background :: Color
    background = light (light green)

    data WheelGame = Game 
        { player1 :: Int
        , player2 :: Int
        , solution :: [Char]
        , puzzle :: [((Float,Float),(Float,Float),Color,[Char])]
        , playerturn :: Int
        , wheel :: [Int]
        , currWheel :: Int
        , guessedLetters :: [Char]
        , wheelBMP :: Picture
        , ranGen :: StdGen       
        } deriving Show
    
    initialState :: Picture -> WheelGame
    initialState image = Game 
        { player1 = 0
        , player2 = 0
        , solution = "OURGAMEISTHEBEST"
        , puzzle = puzzle2
        , playerturn = 1
        , wheel = [0,500,25,450,100,325,1,150,250,375,50,425,0,75,475,175,400,200,2,275,350,225,300,125]
        , currWheel = 25
        , guessedLetters = []
        , wheelBMP = translate 400 0 $ scale 0.3 0.3 $ image
        , ranGen = mkStdGen 1
        }

    fps :: Int
    fps = 60

    render :: WheelGame -> Picture
    render game = 
        renderedPuzzle
        where
            puzzleW = puzzle game
            complexPuzzle = [renderOne x | x <- puzzleW]
            -- Flatten lists
            flattenedPuzzle = concat complexPuzzle
            player = playerturn game
            p1 = player1 game
            p2 = player2 game
            players = renderPlayers p1 p2 player
            w = currWheel game
            wheelVal = renderWheelVal w
            wheelImage = wheelBMP game
            finalPuzzle = flattenedPuzzle ++ players ++ [wheelVal] ++ [wheelImage]
            renderedPuzzle = pictures finalPuzzle
            
    renderWheelVal :: Int -> Picture
    renderWheelVal w = translate 200 (-250) $ scale 0.5 0.5 $ text ("Spin: $" ++ wText)
            where 
                wText = show w

    renderPlayers :: Int -> Int -> Int -> [Picture]
    renderPlayers p1 p2 p = [pDisplay1,pDisplay2]
            where
                p1text = show p1
                p2text = show p2
                pDisplay1 = if (p == 1) 
                    then translate (-580) (-250) $ scale 0.5 0.5 $ Color red $ text ("P1:$" ++ p1text)
                    else translate (-580) (-250) $ scale 0.5 0.5 $ text ("P1:$" ++ p1text)
                pDisplay2 = if (p == 2)
                    then translate (-200) (-250) $ scale 0.5 0.5 $ Color red $ text ("P2:$" ++ p2text)
                    else translate (-200) (-250) $ scale 0.5 0.5 $ text ("P2:$" ++ p2text)
                
    renderOne :: ((Float,Float),(Float,Float),Color,[Char]) -> [Picture]
    renderOne nums = blockAndText
            where
                ((tx,ty),(bx,by),c,l) = nums
                blockX = translate bx by $ Color white $ rectangleSolid 100 125
                textX = translate tx ty $ Color c $ text l
                blockAndText = [blockX,textX]

    main :: IO ()
    main = do
        wheelImage <- loadBMP "Untitled.bmp"
        play window background fps (initialState wheelImage) render handleKeys update

    update :: Float -> WheelGame -> WheelGame
    update seconds = updateGame seconds

    updateGame :: Float -> WheelGame -> WheelGame
    updateGame seconds game = game

    findLetters :: [Char] -> Char -> [Int]
    findLetters sol l = helper sol l 0
        where
            helper [] _ _ = []
            helper (x:xs) l n
                | x == l = n : helper xs l (n+1)
                | otherwise = helper xs l (n+1)
    
    revealLetters :: [((Float,Float),(Float,Float),Color,[Char])] -> [Int] -> [((Float,Float),(Float,Float),Color,[Char])]
    revealLetters letters posOfLetters = helper letters posOfLetters 0
        where
            helper [] _ _ = []
            helper (x:xs) [] n = x : helper xs [] (n+1)
            helper (x:xs) (y:ys) n
                | y == n = (adjustLetter x) : helper xs ys (n+1)
                | otherwise = x : helper xs (y:ys) (n+1)
    
    adjustLetter :: ((Float,Float),(Float,Float),Color,[Char]) -> ((Float,Float),(Float,Float),Color,[Char])
    adjustLetter letter = newLetter
        where
            ((tx,ty),(bx,by),c,x) = letter
            newLetter = ((tx,ty),(bx,by),black,x)
    
    handleKeys :: Event -> WheelGame -> WheelGame
    handleKeys (EventKey (Char 'a') Down _ _) game =
        game { puzzle = letterA'
             , player1 = new1
             , player2 = new2
             , guessedLetters = newGuess
             , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'A' guesses
            posOfLetter = findLetters sol 'A'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "A"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'b') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'B' guesses
            posOfLetter = findLetters sol 'B'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "B"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'c') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'C' guesses
            posOfLetter = findLetters sol 'C'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "C"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
                    
    handleKeys (EventKey (Char 'd') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'D' guesses
            posOfLetter = findLetters sol 'D'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "D"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'e') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'E' guesses
            posOfLetter = findLetters sol 'E'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "E"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'f') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'F' guesses
            posOfLetter = findLetters sol 'F'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "F"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'g') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'G' guesses
            posOfLetter = findLetters sol 'G'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "G"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'h') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'H' guesses
            posOfLetter = findLetters sol 'H'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "H"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'i') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'I' guesses
            posOfLetter = findLetters sol 'I'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "I"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
                    
    handleKeys (EventKey (Char 'j') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'J' guesses
            posOfLetter = findLetters sol 'J'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "J"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'k') _ _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'K' guesses
            posOfLetter = findLetters sol 'K'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "K"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'l') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'L' guesses
            posOfLetter = findLetters sol 'L'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "L"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p

    handleKeys (EventKey (Char 'm') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'M' guesses
            posOfLetter = findLetters sol 'M'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "M"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'n') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'N' guesses
            posOfLetter = findLetters sol 'N'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "N"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'o') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'O' guesses
            posOfLetter = findLetters sol 'O'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "O"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
                    
    handleKeys (EventKey (Char 'p') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'P' guesses
            posOfLetter = findLetters sol 'P'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "P"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'q') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'Q' guesses
            posOfLetter = findLetters sol 'Q'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "Q"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'r') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'R' guesses
            posOfLetter = findLetters sol 'R'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "R"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 's') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'S' guesses
            posOfLetter = findLetters sol 'S'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "S"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 't') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'T' guesses
            posOfLetter = findLetters sol 'T'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "T"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'u') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'U' guesses
            posOfLetter = findLetters sol 'U'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "U"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
                    
    handleKeys (EventKey (Char 'v') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'V' guesses
            posOfLetter = findLetters sol 'V'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "V"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'w') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'W' guesses
            posOfLetter = findLetters sol 'W'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "W"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'x') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'X' guesses
            posOfLetter = findLetters sol 'X'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "X"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'y') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'Y' guesses
            posOfLetter = findLetters sol 'Y'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "Y"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p
    
    handleKeys (EventKey (Char 'z') Down _ _) game =
        game { puzzle = letterA'
                , player1 = new1
                , player2 = new2
                , guessedLetters = newGuess
                , playerturn = nextPlayer}
        where
            letterPic = puzzle game
            sol = solution game
            winBaseAmount = currWheel game
            p = playerturn game
            p1 = player1 game
            p2 = player2 game
            guesses = guessedLetters game
            check = elem 'Z' guesses
            posOfLetter = findLetters sol 'Z'
            numFound = length posOfLetter
            amountWon = numFound * winBaseAmount
            new1 = if (p == 1 && (not check)) then amountWon + p1 else p1
            new2 = if (p == 2 && (not check)) then amountWon + p2 else p2
            newGuess = if check then guesses else guesses ++ "Z"
            letterA' = revealLetters letterPic posOfLetter
            nextPlayer = if numFound == 0 then 1 - p + 2 else p

    handleKeys (EventKey (Char '1') _ _ _) game =
        game { playerturn = 1}
    
    handleKeys (EventKey (Char '2') _ _ _) game = 
        game { playerturn = 2}

    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game =
        game {currWheel = newNum
            , ranGen = updatedGen}
        where
            wheelList = wheel game
            myGen = ranGen game
            -- insert random generator here
            (num, g) = genNumber myGen game
            newNum = wheelList !! num
    --        handleBankrupt game newNum
    --        handleLoseATurn game newNum

            updatedGen = g
    
    handleKeys _ game = game    

    genNumber :: StdGen -> WheelGame -> (Int, StdGen)
    genNumber sGen game = randomR (0,23) r
            where
                r = ranGen game
    
    --handleBankrupt :: WheelGame -> Int -> 

    puzzle1 = [(((-480),(-50)),((-440),0),white,"A"),
               (((-370),(-50)),((-330),0),white,"B"),
               (((-260),(-50)),((-220),0),white,"C"),
               (((-150),(-50)),((-110),0),white,"A")]
    
    puzzle2 = [(((-580),180),((-540),230),white,"O"),
               (((-470),180),((-430),230),white,"U"),
               (((-360),180),((-320),230),white,"R"),
               (((-140),180),((-100),230),white,"G"),
               (((-30),180),(10,230),white,"A"),
               ((80,180),(120,230),white,"M"),
               ((190,180),(230,230),white,"E"),
               (((-580),45),((-540),95),white,"I"),
               (((-470),45),((-430),95),white,"S"),
               (((-250),45),((-210),95),white,"T"),
               (((-140),45),((-100),95),white,"H"),
               (((-30),45),(10,95),white,"E"),
               (((-580),(-90)),((-540),(-40)),white,"B"),
               (((-470),(-90)),((-430),(-40)),white,"E"),
               (((-360),(-90)),((-320),(-40)),white,"S"),
               (((-250),(-90)),((-210),(-40)),white,"T")]

