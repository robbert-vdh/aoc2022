{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-7.txt"

  putStrLn "Part 1:"
  print $! sumLargeDirs input

  putStrLn "\nPart 2:"
  print $! findRemovalCandidate input

-- * Part 1

type DirectoryListing = HashMap String FileSystemNode
data FileSystemNode
  = -- | A file with a certain size
    File Int
  | -- | A directory possibly containing other named files and directories.
    Dir DirectoryListing
  deriving (Show)

-- | Return the total size of all directories with a total size of at most
-- 100000 (the same file may be counted multiple times).
sumLargeDirs :: DirectoryListing -> Int
sumLargeDirs = sum . filter (<= 100000) . dirSizes

-- | Recursively compute the total size of this directory and all of its
-- descendent directories.
dirSizes :: DirectoryListing -> [Int]
dirSizes dir =
  -- This top-down approach is super inefficient but it's fast enough so may as
  -- well
  let !size = dirSize dir
   in size : concatMap go (toList dir)
  where
    -- Files shouldn't contribute on their own
    go (File _) = []
    go (Dir subdir) = dirSizes subdir

-- | Get the total size of a directory. This is super inefficient, but since
-- it's fast enough may as well.
dirSize :: DirectoryListing -> Int
dirSize = sum . map fileSystemNodeSize . toList

fileSystemNodeSize :: FileSystemNode -> Int
fileSystemNodeSize (File size) = size
fileSystemNodeSize (Dir dir) = dirSize dir

-- ** Parsing

-- | Parse the command line output for the root file system. This implementation
-- is pretty inefficient because I didn't feel like implementing a zipper and
-- parsing this recursively without one proved to be too much work.
parse :: String -> DirectoryListing
parse =
  foldl' mergeDirs M.empty
    . fromRight'
    . Parsec.runParser (many pCommandBlock <* eof) [] "input"
  where
    fromRight' (Right a) = a
    fromRight' e = error $ "That's not quite...right ;))))) " <> show e

type Parser = Parsec String DirStack

-- | Paths are a stack of directories. In other words, @/@ becomes @[]@ and
-- @/foo/bar/baz@ becomes @["baz", "bar", "foo"]@.
type DirStack = [String]

-- | Contains the output of one or more @cd@ commands followed by an @ls@. This
-- is incredibly inefficient and creates a directory listing with zero or more
-- leading directories leading up to the directory listing from the @ls@
-- command.
pCommandBlock :: Parser DirectoryListing
pCommandBlock = do
  -- We're parsing the @$ @ command prefix first and then after every command.
  -- This allows us to avoid backtracking since the cd commands and ls start
  -- with different letters
  _ <- string "$ " *> sepEndBy pCd (string "$ ")
  ancestorDirs <- getState

  -- The listing needs to have the leading directories from @currentDir@
  -- prepended to it
  relativeListing <- pLs

  return $! foldl' addParentDir relativeListing ancestorDirs
  where
    -- This modifies the parser's state to set the current path.
    pCd :: Parser ()
    pCd = string "cd " *> (pCdParent <|> pCdRoot <|> pCdSubdir)

    pCdParent = (string ".." *> endOfLine) >> modifyState tail
    pCdRoot = (char '/' *> endOfLine) >> putState []
    pCdSubdir = manyTill anyChar endOfLine >>= \subdir -> modifyState (subdir :)

-- | We can cheat a bit here because of the way the commands are structured. At
-- the end of a "block" we either go up to the parent directory and continue
-- parsing @$ cd ...@ commands from there, or the block ends with a @$ cd /@ in
-- which case we should ascend all the way to the root file system and continue
-- from there.
data CdResult = EnterDir String | GoToParent | GoToRoot deriving (Show)

-- | Parse the output of an @$ ls@ command. Directories will be created as empty
-- directories.
pLs :: Parser DirectoryListing
pLs = string "ls" *> endOfLine *> pLsOutput
  where
    -- The @manyTill ... endOfLine@ already consumes the end of line character,
    -- so this is a regular many instead of 'endSepBy'
    pLsOutput = foldl' insertNode' M.empty <$> many (pDir <|> pFile)
    pDir = (,Dir M.empty) <$> (string "dir " *> manyTill anyChar endOfLine)
    pFile = (\size name -> (name, File $ read size)) <$> many1 digit <*> (char ' ' *> manyTill anyChar endOfLine)

-- | Insert a node into the file system. If the node is a directory that already
-- exists, then the directory's contents are merged recursively. If the node is
-- a file that already exists this function diverges. This directory merging
-- should not be needed for this puzzle, but better be safe than sorry.
insertNode :: DirectoryListing -> String -> FileSystemNode -> DirectoryListing
insertNode m name node = M.alter go name m
  where
    go :: Maybe FileSystemNode -> Maybe FileSystemNode
    go (Just (Dir existingNodes))
      -- Directories should be recursively merged if this ever comes up
      | Dir newNodes <- node =
          Just . Dir $ mergeDirs existingNodes newNodes
    go (Just _) = error "Duplicate files or directories in tree"
    go Nothing = Just node

-- | 'insertNode', but in a format that can be easily used with 'foldl''.
insertNode' :: HashMap String FileSystemNode -> (String, FileSystemNode) -> HashMap String FileSystemNode
insertNode' m (name, node) = insertNode m name node

-- | Recursively merge two directory listings. Diverges when a regular file
-- exists in both file systems or a path is a regular file in one file system
-- and a directory in the other.
mergeDirs :: DirectoryListing -> DirectoryListing -> DirectoryListing
mergeDirs = M.foldlWithKey' insertNode

-- | Wrap a directory listing in a parent directory.
addParentDir :: DirectoryListing -> String -> DirectoryListing
addParentDir m name = M.singleton name (Dir m)

-- * Part 2

totalDiskSpace, requiredDiskSpace :: Int
totalDiskSpace = 70000000
requiredDiskSpace = 30000000

-- | Find the smallest directory which when removed, brings the available disk
-- space up to 30000000, and returns its size.
findRemovalCandidate :: DirectoryListing -> Maybe Int
findRemovalCandidate dir =
  let usedDiskSpace = dirSize dir
      freeDiskSpace = totalDiskSpace - usedDiskSpace
      minDirSize = max 0 (requiredDiskSpace - freeDiskSpace)
      candidates = filter (>= minDirSize) (dirSizes dir)
   in if null candidates
        then Nothing
        else Just (minimum candidates)
