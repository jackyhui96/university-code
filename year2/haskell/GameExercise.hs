{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
module GameExercise where

{-

Before running your solution in ghci or compiling it by ghc on lab machines
make sure you run

    module load ghc/7.6.3

(This is to make sure your version of GHC supports Safe language extension.)

-}

import Control.Monad (liftM, ap)
import Control.Applicative (Applicative(..))
import System.Random
import qualified Data.Map as Map

-- We have two players PV (the vertical player) and PH (the horizontal player).
-- More information is in the pdf handout on Canvas.

data Player = PV | PH deriving (Read, Show, Eq, Ord)

type Outcome = Int -- We will use only -1, 0, 1.

-- You need to define a type of boards:

data Board = Board {
		player		:: Player
		, xBound	:: Int
		, yBound	:: Int
		, outcome	:: Outcome
		, takenMoves	:: [Move]
		, pvMoves 	:: [Move]
		, phMoves 	:: [Move]
		}

instance Show Board where
   show (Board pl x y outcome taken pv ph) =
	let ls = [(x', y') | x' <- [0..x], y' <- [0..y]] in
        show pl ++ " plays next\n\n"
	++ f ls 0
     where
       f [] n = []
       f (m@(x',y'):ms) n | m `elem` taken = if x' == n then " X " ++ (f ms n) else "\n" ++ " X " ++ (f ms (n+1))
                	  | otherwise = if x' == n then " - " ++ (f ms n) else "\n" ++ " - " ++ (f ms (n+1))

-- You also need to provide a type of moves:

type Move = (Int, Int)

-- You will work with the definition of Tree as in our tic-tac-toe
-- lectures and handouts:

data Tree = Fork {root :: Board, children :: [(Move,Tree)]}

-- In order to test your program, we will need to create boards,
-- without knowing how you choose to represent boards. You have to
-- implement the following function toBoard for that purpose.
--
-- Input: a tuple (xBound, yBound, coordinates, player) where
--
--     (1) xBound and yBound are non-negative Int's
--     (2) coordinates is a list of pairs (x,y) with 0 <= x < xBound
--                                               and 0 <= y < yBound
--         which are to be occupied in the board.
--     (3) player is the Player that plays next in that board.
--
-- Output: a Board according to your choice of representation.

toBoard :: (Int, Int, [(Int,Int)], Player) -> Board
toBoard (x, y, coord, player) = Board player x y 0 coord pv ph
				where (pv,ph) =	let ls = [(x', y') | x' <- [0..x-1], y' <- [0..y-1]] in
						(findAllowedMoves x y PV ls coord, findAllowedMoves x y PH ls coord)

findAllowedMoves :: Int -> Int -> Player -> [Move] -> [Move] -> [Move]
findAllowedMoves x y _ [] coord = []
findAllowedMoves x' y' PV ((x,y):ls) coord | elem (x,y) coord 		       = findAllowedMoves x' y' PV ls coord
			 		   | elem (x,y+1) coord || (y+1) >= y' = findAllowedMoves x' y' PV ls coord
			     	     	   | otherwise 			       = (x,y) : (findAllowedMoves x' y' PV ls coord)
findAllowedMoves x' y' PH ((x,y):ls) coord | elem (x,y) coord 		       = findAllowedMoves x' y' PH ls coord
			 		   | elem (x+1,y) coord || (x+1) >= x' = findAllowedMoves x' y' PH ls coord
			     	     	   | otherwise 			       = (x,y) : (findAllowedMoves x' y' PH ls coord)

						

-- We also need to perform the opposite conversion, from your
-- representation of boards, to our naive description of boards, so
-- that we can "read" your boards whenever we need to, for testing
-- purposes:

fromBoard :: Board -> (Int, Int, [(Int, Int)], Player)
fromBoard (Board player x y _ coord _ _) = (x, y, coord, player)

-- Similarly, given a Board, we want to create a Move given
-- (x,y,player) where (x,y) is a position in the Board:

toMove :: Board -> (Int, Int, Player) -> Move
toMove  _ (x, y, _) = (x, y) 

-- And we want to do the opposite too:

fromMove' :: Board -> Move -> (Int, Int, Player)
fromMove' (Board PV _ _ _ _ _ _) (x, y) = (x, y, PV)
fromMove' (Board PH _ _ _ _ _ _) (x, y) = (x, y, PH)

fromMove :: Move -> (Int, Int, Player)
fromMove = undefined

-- The first exercise is to play an allowed move in a board, and get
-- the resulting board. Simply throw an error if the move is not
-- allowed in that board. We will only test your function with allowed
-- moves:

play :: Move -> Board -> Board
play (x,y) (Board PV x' y' outcome coord pv ph) | (elem (x,y) pv) =
					                      winning (Board PH x' y' outcome ((x,y):(x,y+1):coord) pv' ph')
					        | otherwise = error "Error in play"
					        where pv' = removeMoves [(x,y),(x,y+1),(x,y-1)] pv
					              ph' = removeMoves [(x,y),(x,y+1),(x-1,y),(x-1,y+1)] ph
play (x,y) (Board PH x' y' outcome coord pv ph) | (elem (x,y) ph) = 
							    winning (Board PV x' y' outcome ((x,y):(x+1,y):coord) pv' ph')
					        | otherwise = error "Error in play"
					        where pv' = removeMoves [(x,y),(x+1,y),(x,y-1), (x+1,y-1)] pv
					              ph' = removeMoves [(x,y),(x+1,y),(x-1,y)] ph

removeMove move [] = []
removeMove move (x:xs) | move == x = removeMove move xs
		       | otherwise = x : (removeMove move xs)
removeMoves [] [] = []
removeMoves [] ls = ls
removeMoves moves [] = []
removeMoves (x:xs) ls = removeMoves xs (removeMove x ls)

winning :: Board -> Board 
winning (Board PV x' y' outcome coord [] ph) = Board PV x' y' (-1) coord [] ph
winning (Board PH x' y' outcome coord pv []) = Board PH x' y' 1 coord pv []
winning board = board

-- Ah. But what are the allowed moves in a give board? You tell me:

allowedMoves :: Board -> [Move]
allowedMoves (Board PV _ _ _ _ pv _) = pv
allowedMoves (Board PH _ _ _ _ _ ph) = ph



-- Now build the tree of a game. You are allowed to base your
-- implementation on any of the given treeOf implementations for the
-- several tic-tac-toe programs in Canvas (discussed in the lectures):

treeOf :: Board -> Tree
treeOf board | null(allowedMoves board) = Fork board [] 
             | otherwise                = Fork board' forest
  where
    forest = [(m, treeOf(play m board)) | m <- allowedMoves board]  
    board'
     | player board == PV = board {outcome = supremum [outcome(root t) | (_,t)<-forest]}
     | otherwise          = board {outcome = infimum  [outcome(root t) | (_,t)<-forest]}


supremum :: [Outcome] -> Outcome
supremum []        = -1 
supremum (1:xs)    =  1
supremum ((-1):xs) = supremum xs
supremum (0:xs)    = supremum0 xs
  where
    supremum0 []     = 0
    supremum0 (1:xs) = 1
    supremum0 (_:xs) = supremum0 xs

infimum :: [Outcome] -> Outcome
infimum []        =  1 
infimum (1:xs)    = infimum xs
infimum ((-1):xs) = -1
infimum (0:xs)    = infimum0 xs
  where
    infimum0 []        = 0
    infimum0 ((-1):xs) = -1
    infimum0 (_:xs)    = infimum0 xs

-- Now we want to have the computer playing first, lazily against an
-- opponent. The opponent supplies the list of moves. But the computer
-- is not allowed to cheat. It has to play its first move without
-- looking at any of the moves of the opponent:

computerFirst :: Tree -> [Move] -> [Move]
computerFirst (Fork _ []) _ = []
computerFirst game xs = let computerMove = fst (head (optimalMoves game)) in
			computerMove : if (xs) == [] 
				       then [] 
				       else case (findSubTree game computerMove) of
						Nothing -> []
						Just t -> case (findSubTree t (head xs)) of
								Nothing -> []
								Just t -> (computerFirst t (tail xs))
		      	

optimalOutcome :: Tree -> Outcome
optimalOutcome (Fork board forest) = outcome board

optimalMoves :: Tree -> [(Move,Tree)]
optimalMoves (Fork board forest) = [(m,t) | (m,t)<-forest, outcome(root t) == outcome board]

findSubTree :: Tree -> Move -> Maybe Tree
findSubTree (Fork b []) move = Nothing
findSubTree (Fork b ((m, t):forest)) move = if move == m then Just t else findSubTree (Fork b forest) move



-- And now you want the computer to play second. It will have to first
-- check the head move of the opponent, provided the list of moves is
-- non-empty, and base its first move (the head of the output list) on
-- that:

computerSecond :: Tree -> [Move] -> [Move]
computerSecond game [] = []
computerSecond game (x:xs) = case (findSubTree game x) of
				Nothing -> []
				Just t -> let game' = t in
					  let computerMove = fst (head (optimalMoves game')) in
						case (findSubTree game' computerMove) of
							Nothing -> []
							Just t' ->  computerMove : (computerSecond t' xs)

-- This should be done so that the following example works:

iplay :: ([Move]->[Move]) -> ([Move]->[Move]) -> [Move]
iplay f g = intercalate ys xs
  where
    ys = f xs
    xs = g ys

intercalate :: [a] -> [a] -> [a]
intercalate []     ys = ys 
intercalate (x:xs) ys = x : intercalate ys xs

-- What the following example should do is produce the list of moves
-- that results from having the computer playing against itself:

example :: Tree -> [Move]
example tree = iplay (computerFirst tree) (computerSecond tree)

-- We now move to random playing. The randomness monad we used for
-- quick sort in the lecture is not sufficiently lazy for our
-- purposes. We work with a lazy Random monad based on
--
--   https://hackage.haskell.org/package/MonadRandomLazy-0.1/docs/Control-Monad-LazyRandom.html
--
-- instead, define below.  


-- We use the standard random generator as our type of seeds for
-- random things:

type Seed = StdGen

-- We get seeds for random-thing generation from Int's:

mkSeed :: Int -> Seed
mkSeed = mkStdGen

-- See https://en.wikipedia.org/wiki/Random_seed
-- We define the monad as follows:

newtype LRand a = LRand (Seed -> a)

instance Functor LRand where
 fmap f (LRand h) = LRand (f.h)

instance Applicative LRand where
 pure  = return
 (<*>) = ap

instance Monad LRand where
 return x = LRand (\seed -> x)  -- The seed is ignored.

 LRand m >>= k =                -- The seed is not only used, but also transformed and propagated.
   LRand (\s ->
     let (s1,s2)  = split s     -- The split function is predefined in the random libraries. Hoogle it.
         LRand m' = k (m s1)
      in m' s2
   )

-- The following are to "get out" this monad:

evalRand :: LRand a -> Seed -> a
evalRand (LRand f) s = f s

-- What this says is that if you have a random element of type a (that
-- is, something of type LRand a), one way to get something of type a
-- is to provide a seed.

-- This is like the above, but also produces a new seed, if we need it:

runRand :: LRand a -> Seed -> (a, Seed)
runRand (LRand f) s = (f s1, s2)
 where (s1, s2) = split s

-- And finally we need to be able to generate random elements:

getRandom :: Random a => LRand a
getRandom = LRand $ fst . random

-- But this needs a to be in the Random type class. Most types are
-- automatically there, and it is unlikely you will need to worry
-- about this in this exercise, unless you do very sophisticated
-- things.

-- We also may need to get random elements within a range:

getRandomR :: Random a => (a,a) -> LRand a
getRandomR range = LRand $ fst . randomR range

-- This is the end of our definition of our lazy randomness monad.

randomFirst :: Tree -> [Move] -> LRand [Move]
randomFirst (Fork _ []) _ = return []
randomFirst game xs = do
			n <- getRandomR (0, (length (allowedMoves (root game)))-1)
			let computerMove = (allowedMoves (root game)) !! n in
				return (computerMove : if (xs) == []
						       then []
					               else case (findSubTree game computerMove) of
								Nothing -> []
								Just t -> case (findSubTree t (head xs)) of
										Nothing -> []
										Just t -> (computerFirst t (tail xs)))
					      
			

randomSecond :: Tree -> [Move] -> LRand [Move]
randomSecond game [] = return []
randomSecond game (x:xs) = case (findSubTree game x) of
					Nothing -> return []
					Just t -> let game' = t in
						  do 
						  n <- getRandomR (0, (length (allowedMoves (root game')))-1)
						  let computerMove = (allowedMoves (root game')) !! n in
							case (findSubTree game' computerMove) of
								Nothing -> return []
								Just t' ->  return (computerMove : (computerSecond t' xs))

heuristicMoves (Fork board forest) = let allow = [(m,length (allowedMoves (root t))) | (m,t) <- forest] in
				     foldr1 (\(a,b) (a',b') -> if b < b' then (a,b) else (a',b')) allow

computerFirstHeuristic :: Board -> [Move] -> [Move]
computerFirstHeuristic board xs = let game = (treeOf board) in 
					case game of
						Fork _ [] -> []
						Fork _ t -> let computerMove = fst (heuristicMoves game) in
							    computerMove : if (xs) == [] 
					       		    then [] 
					       		    else case (findSubTree game computerMove) of
								      Nothing -> []
								      Just t -> case (findSubTree t (head xs)) of
										Nothing -> []
										Just t -> (computerFirst t (tail xs))


computerSecondHeuristic :: Board -> [Move] -> [Move]
computerSecondHeuristic board [] = []
computerSecondHeuristic board (x:xs) = let game = (treeOf board) in
						case (findSubTree game x) of
							Nothing -> []
							Just t -> let game' = t in
								  let computerMove = fst (heuristicMoves game') in
									case (findSubTree game' computerMove) of
										Nothing -> []
										Just t' ->  computerMove : (computerSecond t' xs)

initialBoard xbound ybound = toBoard (xbound, ybound, [], PH)

domineering :: Int -> Int -> Tree
domineering x y = treeOf (initialBoard x y)

test :: Int -> Int -> [Move]
test x y = iplay (computerFirst (domineering x y)) (computerSecond (domineering x y))
test' x y = iplay (computerFirstHeuristic (root (domineering x y))) (computerSecond (domineering x y))

