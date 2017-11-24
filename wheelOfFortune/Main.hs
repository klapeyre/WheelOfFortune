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
        , solution :: [Char]
        , puzzle :: [((Float,Float),(Float,Float),Color,[Char])]        
        } deriving Show
    
    initialState :: WheelGame
    initialState = Game 
        { player1 = 0
        , solution = "OURGAMEISTHEBEST"
        , puzzle = puzzle2
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
            renderedPuzzle = pictures flattenedPuzzle
            
    
    renderOne :: ((Float,Float),(Float,Float),Color,[Char]) -> [Picture]
    renderOne nums = blockAndText
            where
                ((tx,ty),(bx,by),c,l) = nums
                blockX = translate bx by $ Color white $ rectangleSolid 100 125
                textX = translate tx ty $ Color c $ text l
                blockAndText = [blockX,textX]

    main :: IO ()
    main = play window background fps initialState render handleKeys update

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
    handleKeys (EventKey (Char 'a') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'A'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'b') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'B'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'c') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'C'
            letterA' = revealLetters letterPic posOfLetter
                    
    handleKeys (EventKey (Char 'd') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'D'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'e') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'E'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'f') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'F'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'g') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'G'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'h') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'H'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'i') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'I'
            letterA' = revealLetters letterPic posOfLetter
                    
    handleKeys (EventKey (Char 'j') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'J'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'k') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'K'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'l') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'L'
            letterA' = revealLetters letterPic posOfLetter

    handleKeys (EventKey (Char 'm') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'M'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'n') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'N'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'o') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'O'
            letterA' = revealLetters letterPic posOfLetter
                    
    handleKeys (EventKey (Char 'p') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'P'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'q') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'Q'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'r') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'R'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 's') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'S'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 't') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'T'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'u') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'U'
            letterA' = revealLetters letterPic posOfLetter
                    
    handleKeys (EventKey (Char 'v') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'V'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'w') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'W'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'x') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'X'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'y') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'Y'
            letterA' = revealLetters letterPic posOfLetter
    
    handleKeys (EventKey (Char 'z') _ _ _) game =
        game { puzzle = letterA'}
        where
            letterPic = puzzle game
            sol = solution game
            posOfLetter = findLetters sol 'Z'
            letterA' = revealLetters letterPic posOfLetter
    handleKeys _ game = game

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

    
