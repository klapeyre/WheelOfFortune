To get started with development on this project:

1. Install Full Haskell Platform (not just core)
2. Clone this repository
3. From the main directory (wheelOfFortune), type 'cabal run'. This should compile and run the program, and print out the hello world.
4. Dependencies for Gloss (drawing pictures, GUI etc) already installed. Let me know if any issues.


Tracking what is done
- data types created to hold game state, player winnings, a solution and a puzzle
- handles input logic for all alphabet keys
- when a key is pressed, if it is in the puzzle, the letter should be displayed. Handles duplicates and 0 occurrences

To Do
- number generator to give random list element
- wheel graphic
- key input to switch player
- key input to spin wheel
- display current player and their winnings
- display current wheel
- make a few more puzzles

There will need to be a few assumptions to make this work, which will reflect the rules of the game
- each player spins the wheel once and then guesses a letter (on good faith, player uses the value spun)
- each player takes only one guess, then switches player turn by pressing key (again, on good faith)

Extra
- add logic for buying a vowel
- add logic for preventing multiple spins
- add logic for turn changes automatically

