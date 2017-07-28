{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
module Exercise where



-- Exercise set 2.
--
-- 30% of the exercises are intended to be rather challenging, and
-- will allow you to get a mark above 69%, in conjunction with the
-- other available exercises, so as to get a 1st class mark. To get
-- II.2, you will need to do enough hard exercises, in addition to the
-- medium and easy ones. 
--
--      >= 70% 1st
--      >= 60% II.1
--      >= 50% II.2
--      >= 40% III
--      <= 39% fail
--
-- 
-- Do as many unassessed exercises as you can, as they should make the
-- assessed exercises easier.
--
-- You are allowed to use the functions available in the standard
-- prelude (loaded by default by ghc and ghci). You should not need to
-- use other Haskell libraries from the Haskell platform available in
-- the lab, but you are allowed to use them if you wish. However, in
-- your final submission, you should not use any IO facilities, as
-- this won't compile with the marking script.

-- This exercise set revolves around a directory tree on a computer,
-- and some Unix-like functions to manipulate them.
--
-- This exercise doesn't involve reading or writing actual files from
-- disk. Instead, we represent them internally in Haskell using a
-- "data" definition, explained below.
--
-- We have these data types:
--
--     - Entry: Can be either a file or a directory (with sub-Entries)
--     - EntryName: exactly the same as a string; 
--                  represents a directory or file name
--     - Path: exactly the same as a list of strings
--     - FileProp: file properties 


data Entry = File EntryName FileProp
           | Dir EntryName [Entry]
  deriving (Show, Eq, Read)


-- The name of a file
type EntryName = String

-- A sequence of file name components.
--
-- A path is a list of strings used to navigate down to a subdirectory
-- of a given directory. We start from an Entry, and we end up with a
-- sub-Entry, provided the path is valid. See the example in the "cd"
-- exercise below.
type Path = [String]

-- FileProp describes some attributes of a file.
--
-- The components mean size, content, and creation time,
-- in that order.
data FileProp = FP Int String Int
  deriving (Show, Eq, Read)


-- Exercise, easy. Create a FileProp that describes a file with size 3,
-- content "abc", and time 4.

exampleFP :: FileProp
exampleFP = FP 3 "abc" 4

{-

Entries describe directories and files in the file system. For instance, the
following entry describes an empty directory:

    Dir "somedirname" []

The following entry describes a file in the filesystem:

    File "somefilename" (FP 4 "xyz" 3)

The following entry describes a more complicated directory.

    Dir "uni" [File "marks.txt" (FP 1036 "..." 2014),
               Directory "examSheets" [],
               File "address.txt" (FP 65 "..." 2010)
              ]

-}

-- Exercise, easy. Create three entries that correspond to the following trees:
--
-- 1.   todo.txt, size 723, time 2015, content "do fp exercises"
--
-- 2.   empty-directory
--      |
--
-- 3.   hard drive
--      |
--      |-- WINDOWS
--      |   |
--      |   |-- cmd.exe, size 1024, time 1995, content ""
--      |   |
--      |   |-- explorer.exe, size 2048, time 1995, content ""
--      |
--      |-- Documents
--      |   |
--      |   |-- User1
--      |   |   |
--      |   |   |-- recipe.doc, size 723, time 2000
--      |   |
--      |   |-- User2
--      |   |   |
--
-- You must pay attention to the order of entries in a directory.
--
-- There is a dash in the directory name of exampleEntry2.


exampleEntry1 :: Entry
exampleEntry1 = File "todo.txt" (FP 723 "do fp exercises" 2015)
exampleEntry2 :: Entry
exampleEntry2 = Dir "empty-directory" []
exampleEntry3 :: Entry
exampleEntry3 = Dir "hard-drive" [Dir "WINDOWS" [File "cmd.exe" (FP 1024 "" 1995),
						 File "explorer.exe" (FP 2048 "" 1995)
						], 
				  Dir "Documents" [Dir "User1" [File "recipe.doc" (FP 723 "" 2000)], 
						   Dir "User2" []
						  ] 
				 ]

-- Exercise, unassessed. You're given a directory as a value of type
-- Entry. In this directory there is a subdirectory with name n. Find
-- (and return) this subdirectory.

cd1 :: Entry -> String -> Maybe Entry
cd1 (File _ _) n = Nothing
cd1 (Dir _ []) n = Nothing
cd1 (Dir x (entries)) n = (cd2 entries n)

cd2 :: [Entry] -> String -> Maybe Entry
cd2 [] n = Nothing	
cd2 ((File x _) : xs) n = cd2 xs n
cd2 (d@(Dir x _) : xs) n = if x == n then Just d else cd2 xs n


-- Exercise, easy. As before, but you need to navigate not one but
-- possibly many steps down; consecutive directory names are given in
-- the list of strings.
-- 
-- Example: Given the entry in the following drawing
-- 
--     root
--     |
--     |-- dir1
--     |   |
--     |   |-- dir1a
--     |   |   |
--     |   |   |-- dir1a1
--     |   |   |
--     |   |   |-- dir1a2
--     |   |
--     |   |-- dir1b
--     |
--     |-- dir2
--     |   |
--     |   |-- dir1a
--     |   |
--     |   |-- dir1b
--     |
--     |-- file3
-- 
-- and the path ["dir1", "dir1a"], you need to return the Entry that
-- contains dir1a1 and dir1a2.
-- 
-- If there is no such entry, return Nothing. If there is such an entry,
-- return Just that entry.
-- 
-- You can assume that there will be at most one entry with the given path.

cd :: Entry -> Path -> Maybe Entry 
cd (File _ _) path = Nothing
cd (Dir _ []) path = Nothing
cd d@(Dir _ (entries)) [x] = cd1 d x
cd d@(Dir _ (entries)) (x : xs) = cd (convertMaybeToEntry (cd1 d x)) xs

convertMaybeToEntry (Just (File x y)) = File x y
convertMaybeToEntry (Just (Dir x y)) = Dir x y



-- Exercise, medium. Split a string representing a path into its
-- components. The components are separated by (forward) slashes.
-- Hint: the prelude functions for lists will be helpful here, but you
-- are not required to use them.
--
-- Examples:
--
--     explode "abc/de/fghi" = ["abc", "de", "fghi"]
--     explode "abc//fghi" = ["abc", "", "fghi"]
--
-- It is a matter of convention whether we prefer
--     explode "" = [] 
-- or
--     explode "" = [""]
-- Choose your own convention. Both will be considered to be correct.


explode :: String -> Path
explode "" = []
explode ls = removeSpace (wordsWhen (=='/') (addSpace ls))

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

addSpace :: String -> String
addSpace [x] = [x]
addSpace (x : y : xs) = if x == '/' && y == '/'
		  then (x : " ") ++ addSpace (y : xs)
		  else x : addSpace (y : xs)

removeSpace :: Path -> Path
removeSpace [] = []
removeSpace (x : xs)| x == " " = "" : (removeSpace xs)
		     | otherwise = x : (removeSpace xs) 
  
-- Exercise, easy. The "inverse" of explode: combine components with
-- slashes to a single String.
--
-- For every string s, you must have
--
--    implode (explode s) = s
--
-- You may want to use the functions "concat", "intersperse", and/or
-- "intercalate"; the latter two are from package Data.List.

implode :: Path -> String
implode [] = ""
implode [x] = x
implode (x : xs) = x ++ '/' : implode xs

-- Exercise, easy. Given an Entry representing a directory, print out
-- a directory listing in a format similar to "ls -l" on Unix.
--
-- The required format is as in the following example:
--
--     size: 420 time: 5 filename1
--     size: 5040 time: 200 other.txt
--     size: 30 time: 36 filename2
--
-- You need to separate every line with a newline ('\n') character,
-- and also put a newline at the end.
--
-- Keep the files in their order in the given Entry.
--
-- You do not need to convert units, just print the numbers.

lsL :: Entry -> String
lsL (File n (FP s c t)) = ("size: " ++ (show s) ++ " time: " ++ (show t) ++ " " ++ c ++ "\n")
lsL (Dir _ entries) = (lsL' entries)

lsL' [] = ""
lsL' ((Dir n _) : xs) = n ++ "\n" ++ lsL' xs
lsL' ((File n (FP s c t)) : xs) = "size: " ++ (show s) ++ " time: " ++ (show t) ++ " " ++ c ++ "\n" ++ lsL' xs

-- Exercise, medium. List all the files in a directory tree. Sample
-- output:
--
--    root
--    |
--    |-- dir1
--    |   |
--    |   |-- dir1a
--    |   |   |
--    |   |   |-- dir1a1
--    |   |   |
--    |   |   |-- somefile
--    |   |   |
--    |   |   |-- dir1a2
--    |   |
--    |   |-- dir1b
--    |
--    |-- file2
--    |
--    |-- dir3
--    |   |
--    |   |-- dir3a
--    |   |
--    |   |-- dir3b
--
--
-- You can assume that the entry represents a directory.
--
-- Use the newline convention as given above.

lsTree :: Entry -> String
lsTree (File name _) = undefined
lsTree (Dir name entries) = name ++ 
			    (lsTree' entries 1)

lsTree' [] _ = ""

lsTree' ((Dir name entries) : xs) n = '\n' : (format n) ++ '\n' : (format (n-1)) ++ 
				      "|-- " ++ name ++ 
				      '\n' : (format (n+1)) ++ 
				      (lsTree' entries (n+1)) ++ 
				      (lsTree' xs n)

lsTree' ((File name _) : xs) n ='\n' : (format (n-1)) ++ "|-- " ++ name ++ (lsTree' xs n)

format n = (foldr (++) "" (replicate n "|   "))

-- Exercise, challenge. Make a list of all the files and directories
-- recursively in a tree (similar to "find ." in linux). If the
-- argument fullPath is False, every entry in the returned list will
-- have only the bare directory or file name. If fullPath is True,
-- every entry is the path towards that entry,
-- e.g. "root/subdir1/subdir1a/file".
--
-- The root must be the first list item. The output will be in the
-- same order as for lsTree.
--
-- For example, if d is this directory from an earlier exercise:
--
--      hard drive
--      |
--      |-- WINDOWS
--      |   |
--      |   |-- cmd.exe, size 1024, time 1995, content ""
--      |   |
--      |   |-- explorer.exe, size 2048, time 1995, content ""
--      |
--      |-- Documents
--      |   |
--      |   |-- User1
--      |   |   |
--      |   |   |-- recipe.doc, size 723, time 2000
--      |   |
--      |   |-- User2
--      |   |   |
--
-- then we have

--       listAll False d =
--                ["hard drive", "WINDOWS", "cmd.exe", "explorer.exe"
--                ,"Documents", "User1", "recipe.doc", "User2"]
-- and
--      listAll True d = 
--                ["hard drive"
--                ,"hard drive/WINDOWS"
--                ,"hard drive/WINDOWS/cmd.exe"
--                ,"hard drive/WINDOWS/explorer.exe"
--                ,"hard drive/Documents"
--                ,"hard drive/Documents/User1"
--                ,"hard drive/Documents/User1/recipe.doc",
--                ,"hard drive/Documents/User2"]

listAll :: Bool -> Entry -> [String]
listAll fullPath root = undefined

-- Exercise, hard. 
--
-- Given a tree, insert a given subtree in a certain position.
--
-- It does not matter how the inserted subtree is ordered with respect
-- to the other items in the directory where it is inserted. That is,
--
--     cp (Dir "root" [Dir "subdir1" [Dir "subdir1a" []]]) (["subdir1"], Dir "subdir1b" [])
--
-- may return either
--
--     Dir "root" [Dir "subdir1" [Dir "subdir1a" [], Dir "subdir1b" []]]
--
-- or
--
--     Dir "root" [Dir "subdir1" [Dir "subdir1b" [], Dir "subdir1a" []]] .
--
-- (This function is similar-ish to the Unix 'cp' utility.)


cp :: Entry -> (Path, Entry) -> Entry
cp root (destPath, subtree) = undefined

-- Exercise, medium. Given a tree and a path, remove the file or
-- directory at that path.
--
-- You can assume that there is a file or directory at that path. If
-- there are multiple files or directories with that path, you need to
-- remove all of them.
--
-- (In that the case, the tree would not be "valid" according to isValid.)

rm :: Entry -> Path -> Entry
rm root path = undefined

-- Exercise, harder. Return a tree with all the same entries, but so
-- that the entries of each (sub)directory are in sorted order.
--
-- You may use the function `sort` from the Prelude.
--
-- If there are multiple entries with the same name in a directory,
-- you may choose any order.

sortTree :: Entry -> Entry
sortTree root = undefined

-- Exercise, unassessed. Change all letters to upper case in a string.
--
-- For instance,
--
--     upcaseStr "!someString123" = "!SOMESTRING123"
--
-- Hint: look at the definition of the String type in the Prelude, and think
-- about functions related to that type.
--
-- You may use the function upcaseChar, defined below.

upcaseStr :: String -> String
upcaseStr s = undefined

upcaseChar :: Char -> Char
upcaseChar c =
    if ('a' <= c && c <= 'z')
    then toEnum (fromEnum c + fromEnum 'A' - fromEnum 'a')
    else c

-- Exercise, harder. Change all the file names (as above) and their
-- properties, similar to the above exercise.
--
-- From the type of modifyEntries, you can see what the input of
-- fileMap must be.

modifyEntries :: Entry -> ((EntryName, FileProp) -> (EntryName, FileProp)) -> Entry
modifyEntries root fileMap = undefined

-- Exercise, unassessed. Create a "Fibonacci tree".
--
-- The Fibonacci tree for n=3 looks like this:
--
--     dir3
--     |   
--     |-- dir2
--     |   |
--     |   |-- dir1
--     |   |   |
--     |   |   |-- file, size 0, time 0, content 0
--     |   |   
--     |   |
--     |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir1
--     |   |
--     |   |-- file, size 0, time 0, content 0
--
--
--
-- The Fibonacci tree (fibCreate 0) is a file with name "file", size and time
-- 0, and content "". For n >= 1, fibCreate n is a directory named "dir{n}",
-- containing precisely fibCreate (n-1) and fibCreate (n-2). Exception:
-- fibCreate 1 contains only fibCreate 0, and not fibCreate (-1).
--
-- (We just made up the concept of Fibonacci trees, it is not a real thing.)

fibCreate :: Int -> Entry
fibCreate maxLevel = undefined

-- Exercise, unassessed. Make the following infinite tree:
--
--     all
--     |
--     |-- file, size 0, time 0, content 0
--     |
--     |-- dir1
--     |   |
--     |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir2
--     |   |
--     |   |-- dir1
--     |   |   |
--     |   |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir3
--     |   |
--     |   |-- dir2
--     |   |   |
--     |   |   |-- dir1
--     |   |   |   |
--     |   |   |   |-- file, size 0, time 0, content 0
--     |   |
--     |   |-- dir1
--     |   |   |
--     |   |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir4
--     |   |
--     |   (and so on)
--     |
--     | ...
--
--
-- It is to be expected that computations such as (size fibEntry) will
-- not return a result and loop for ever (or until we run out of
-- memory). But you can still e.g. "cd" into such a tree.

fibEntry :: Entry
fibEntry = undefined

-- Exercise, unassessed. Remove from a tree all files that are larger
-- than a certain size. You should not remove any directories.
--
-- Files that are exactly that size should be kept in.

findSmallerThan :: Entry -> Int -> Entry
findSmallerThan root maxSize = undefined

-- Exercise, challenge. Remove from a tree all files that do not
-- satisfy a given predicate. You should not remove any directories.

find :: Entry -> ((EntryName, FileProp) -> Bool) -> Entry
find root predicate = undefined


-- Exercise, unassessed. Given a maximum file size, a file name and its file
-- properties, return whether the file is at most that size.
--
-- (This function gets a lot of information that it doesn't need; the
-- extra arguments are so that you can easily use `findSmallerThanPred
-- maxSize` as the predicate argument to `find`, in the next
-- exercise.)

findSmallerThanPred :: Int -> ((EntryName, FileProp) -> Bool)
findSmallerThanPred maxSize (filename, props) = undefined

-- Exercise, unassessed. Same as findSmallerThan, but implement it again
-- using `find` and `findSmallerThanPred`.

findSmallerThan2 :: Entry -> Int -> Entry
findSmallerThan2 root maxSize = undefined

-- Exercise, challenge, assessed.
--
-- List all directory and file names in the current directory in a
-- table. The table can be at most termWidth cells wide. You need to
-- use as few rows as possible, while separating columns with 2
-- spaces.
--
-- (This is similar to the Unix utility 'ls'.)
-- 
-- For instance, for terminal width 80, you might have the following
-- output:
--
--     a  d  g  j                zbcdefghijklmnc  zbcdefghijklmnf  zbcdefghijklmni
--     b  e  h  zbcdefghijklmna  zbcdefghijklmnd  zbcdefghijklmng
--     c  f  i  zbcdefghijklmnb  zbcdefghijklmne  zbcdefghijklmnh
--
--
-- The ordering is alphabetical by column, and the columns should be
-- indented as above.  You can assume that the longest directory/file
-- name is at most as long as the terminal is wide.
--
-- The first argument is the terminal width:

ls :: Int -> Entry -> String
ls termWidth root = undefined

-- End of exercise set 2.

