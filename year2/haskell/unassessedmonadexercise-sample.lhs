SAMPLE SOLUTIONS : 
----------------

Don't read them before attempting the exercises, which is are in
another file, repeated here.

Here are some unassessed exercises on using monads, together with
solved exercises.

After you try them, confirm in Canvas that you have done so.

This is a literate Haskell file.

Read the handout lecture7-handout.lhs from Canvas.

> import Control.Monad.State
> import System.Random

Change this line to try several examples, produced by you or by me:

> main = book

From the book (Chapter 9 of the online version):

> book :: IO ()
> book = do  
>     putStrLn "Hello, what's your name?"  
>     name <- getLine  
>     putStrLn ("Hey " ++ name ++ ", you rock!")  

Run this in a terminal, with "runhaskell unassessedmonadexercise.lhs".

Guess why we are using "()" in the IO type. Perhaps ask ghci, using
":type", what the type of putStrLn is.

Warming-up exercise: Modify the above program to ask for your first
name, then your last name, and then print your full name.

> firstlast :: IO ()
> firstlast = do  
>     putStrLn "Hello, what's your first name?"  
>     firstname <- getLine  
>     putStrLn "And what's your second name?"  
>     secondname <- getLine  
>     putStrLn ("Your full name is " ++ firstname ++ " " ++ secondname)  

Now I do

> palindrome = do
>     putStrLn "Type a palindrome word"
>     word <- getLine
>     if word == reverse word
>        then putStrLn "Well done. You seem to know what that is."
>        else do
>           putStrLn "This is not a palindrome. Try again!"
>           palindrome

What is the type of palindrome? (Well, if you can't figure this out,
ask ghci.)

Change the above definition of main to be palindrome instead.  Then
Run this in a terminal, again using runhaskell.

Alternatively, just run ghci on this file, and type "main2".

Yet another possibility is to do

  $ ghc --make unassessedmonadexercise.lhs
  $ ./unassessedmonadexercise

With the above, you have compiled your program, producing an
executable file, and then run it.

When you run your program either with runhaskell or by compiling it
first, "main" is invoked (just like in Java or C).

Exercise. Write an infinite loop that asks for a word and prints its length, repeatedly.

> lengthloop = do
>     putStrLn "Type a word"
>     word <- getLine
>     putStrLn ("Its length is " ++ show(length word))
>     lengthloop

Exercise. Modify it to stop when an empty word is entered:

> lengthloop' = do
>     putStrLn "Type a word"
>     word <- getLine
>     putStrLn ("Its length is " ++ show(length word))
>     if word == "" then return () else lengthloop'

Now I will try to quickly find a secret number you are thinking of, by
asking you some yes-no questions:

> lowerBound = 1
> upperBound = 10000

> guessSecretNumber = dialogue lowerBound upperBound
>   where 
>     dialogue l u | l == u    = putStrLn ("If you answered truthfully, your secret number is " ++ show l)
>                  | otherwise = do
>                                 putStrLn ("You are thinking of a number beween " ++ show l ++ " and " ++ show u)
>                                 let m = ((l + u) `div` 2) + 1
>                                 putStrLn ("Is your number smaller than " ++ show m ++ "? Please answer y or n.") 
>                                 answer <- getLine
>                                 case answer of
>                                  "y" -> dialogue l (m-1)
>                                  "n" -> dialogue m u
>                                  _   -> do
>                                           putStrLn "Wrong answer. Try again."
>                                           dialogue l u

Read and then run the above.

(The yes-no questions you answered correspond to the binary notation
of your secret number, right? So this program is implicitly computing
your number from its binary notation. And you are telling me what its
binary notation is, by answering yes or no.)

Now I am going to print the numbers from 1 to 10. There are a number
of ways of doing this. This is not the cleverest way, but it works:

> print1to10 :: IO ()
> print1to10 = print1to10helper 1 10
>
> print1to10helper :: Int -> Int -> IO ()
> print1to10helper x y | x <= y = do
>                             putStrLn (show x)
>                             print1to10helper (x+1) y
>                 | x >  y = return ()

This is cleverer:

> print1to10again :: IO [()]
> print1to10again = sequence [putStrLn (show n) | n <- [1..10]]

"sequence" "does" all things in the list. This is really like a "do",
but instead of writing each thing to do, we put all things to do in a
list, and we ask for all of them to be done, in sequence, one after
the other, from the first to the last.

Try this, both with ghci, and with runhaskell (and with "ghc --make" if you wish).

> fib :: (Ord a, Eq a, Num a) => a -> a
> fib = f 0 1
>  where
>    f :: (Ord a, Eq a, Num a) => a -> a -> a -> a
>    f current next n | n == 0    = current
>                     | n >= 1    = f next (current+next) (n-1)
>                     | n <  0    = undefined
 
Exercise. Fill the following definition to print the first 20
fibonacci numbers, using fib defined above.

> twentyfibs = sequence [putStrLn (show (fib n)) | n <- [1..10]]

Exercise. Look at the function maybefib in the lecture handout. Write
a function maybefactorial that maybe computes the factorial, using the
maybe monad:

> maybefactorial :: (Eq a, Num a, Ord a) => a -> Maybe a
> maybefactorial n | n == 0    = return 1
>                  | n >= 1    =  do
>                                  y <- maybefactorial(n-1) 
>                                  return(n*y)
>                  | otherwise = Nothing

Exercise. Now write a function sfactorial corresponding to sfib in the
lecture handout, which counts recursive calls, and run it to test it:

> sfactorial :: (Eq a, Num a, Ord a) => a -> State Int a
> sfactorial n | n == 0    = return 1
>              | n >= 1    =  do
>                                y <- sfactorial(n-1) 
>                                s <- get
>                                put(s+1)
>                                return(n*y)

Now I will go beyond the lectures, after I recall something we've
already seen in the lectures.

Here is a version of deterministic quick sort, choosing the first
element as the pivot:

> qsort :: Ord a => [a] -> [a]
> qsort [] = []
> qsort (x:xs) =  qsort [y | y <- xs, y < x] 
>              ++ [x] 
>              ++ qsort [y | y <- xs, y >= x] 

It is fast on average (namely n*log(n)), but slow in the worst case
(namely n*n). We will make its expected run time fast by randomly
choosing the pivot. Trouble: choosing a random number is not a
function. So we use a monad, as discussed in the lectures (but not yet
explained in the lectures).

Given a list xs and an integer n, we return the nth element of xs
together with all elements of xs except the nth element:

> getPivot :: [a] -> Int -> (a, [a])
> getPivot (x:xs) 0 = (x,xs)
> getPivot (x:xs) n = let (p,ys) = getPivot xs (n-1) in (p, x:ys)

Now, probably ignore the following "data" and "instance" definitions,
which I will eventually explain in lectures:

> data Rand a = Rand(StdGen -> (a , StdGen))

> instance Monad Rand where
>  return x = Rand(\g -> (x,g))
>  Rand h >>= f = Rand(\g -> let (x, g') = h g 
>                                (Rand h') = f x
>                            in h' g')

The point is that I am defining a new monad. If you are brave, you can
try to understand this at this point (by reading the book).

Again, I am not explaining this, other than saying that it gives a
random Int in our monad for random things:

> randInt :: Rand Int
> randInt = Rand random

And this picks a random number in a given range:

> randIntR :: (Int, Int) -> Rand Int
> randIntR (lower, upper) = Rand(randomR(lower, upper))

This is how we "can get out" of Rand a: 

> runRand :: Int -> Rand a -> a
> runRand seed (Rand h) = fst(h(mkStdGen seed))

This is "magic" again for the moment. I am not asking you to
understand it yet. I will explain it in due course (but again you may
wish to be ahead of the lectures).

Using this, I can pick a random element of a list with "uniform
distribution", meaning that there is no bias in my choice, so that all
elements are equally like:

> uniform :: [a] -> Rand a
> uniform xs = do
>    n <- randIntR (0, length xs - 1)
>    return(xs !! n)

Here is randomized quicksort using our random monad. What is random is
the choice of the pivot:

> rqsort :: Ord a => [a] -> Rand [a]
> rqsort [] = return []
> rqsort xs = do 
>                 n <- randIntR (0, length xs - 1)
>                 let (p, ys) = getPivot xs n
>                 as <- rqsort [y | y <- ys, y < p] 
>                 bs <- rqsort [y | y <- ys, y >= p] 
>                 return(as ++ [p] ++ bs)

> rqsort' :: Ord a => [a] -> [a]
> rqsort' xs = runRand seed (rqsort xs)
>  where seed = 3 -- say

Test this with inputs such as [1..10000] or even larger to convince
yourself in practice that rqsort' is way faster than qsort. (The
theorem is that its expected run time is always n*log(n), whereas
qsort is n*log(n) on average but n*n in the worst case.)

Exercise. Try to produce a random list of Int's of a given length, by
successively picking random Int's with randInt.

> randomList :: Int -> Rand [Int]
> randomList 0 = return []
> randomList n = do  
>   i <- randInt
>   xs <- randomList(n-1)
>   return(i : xs)
