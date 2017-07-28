import System.IO

type Move = Int

height :: Int
height = 5
width :: = Int
width = 5

possibleMoves :: [Move]
possibleMoves = zip [0..(height-1)] [0..(width-1)]
