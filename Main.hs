{-
    Chronorder - Rename your files in chronological order
    Copyright (C) 2013  Pieter Brandwijk

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-
  Supported options:
  -v Print version
  -d [STRING] specify directory to order, default is pwd (maybe without flag -d?)
  -e [STRING] Only process files with specified extension (use ".foo .bar" for more than one)
  --start-index [INT] Start at given index
  --prefix [STRING] Prepend prefix to each filename
  --no-safety Don't ask safety question
  --no-log Don't print a log with all changed filenames to the console

  Possible options:
  -h, -? Print help message
  --pattern [REGEX] Only order files that comply to the pattern
  --restore [STRING] Restore file names from given log file
  --reverse-order sort in ascending order, so newest files first
  --delete-original Delete the original name

  Possible improvements:
  - Check first that renaming does in fact change alphabetical order. If not: abort.
  - Add zeros before smaller numbers eg. 001, 002 if there are between 100 and 1000 files
  - Make prefix prepend text BEFORE index number
  - Display example of conversion with safety question (oldest file, for instance)
  - Replace errors with putStrLn .. >> exitWith ExitSuccess

  Test cases: 
  - equal names (different extension)
  - equal modification times 
  - double specification of command line arguments
-}
module Main ( main ) where

import IO ( putStrLn )
import System ( getArgs, exitWith, ExitCode (ExitSuccess) )
import System.Console.GetOpt
import System.Directory ( getDirectoryContents, getCurrentDirectory, doesDirectoryExist )
import System.FilePath.Posix ( takeBaseName, replaceBaseName )
import System.Posix.Files ( FileStatus, isRegularFile, modificationTime, getFileStatus, rename )
import System.Posix.Types ( EpochTime )
import Data.List( isSuffixOf, (\\) )

-- The options type constructor holds all the option types and functions used in the program.
data Options = Options {
    optVersion :: Bool,
    optExtension :: String -> Bool,
    optPath :: IO FilePath,
    optStartIndex :: Int,
    optPrefix :: String,
    optSafety :: Bool,
    optLog :: Bool
  }

-- The options that are loaded if no option is specified on the command line
defaultOptions :: Options
defaultOptions = Options {
    optVersion  = False, -- Without a version option specified, display version is False
    optExtension = (\x -> True), -- Without an extension specified any file can be processed
    optPath = getCurrentDirectory, -- Without a directory specified use the current directory
    optStartIndex = 1, -- Start at index 1 by default
    optPrefix = "", -- Default prefix is empty string
    optSafety = True, -- By default ask safety question
    optLog = True -- By default do show a log of the affected files
  }

-- The specification of all the options used in the program
options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (NoArg specVersion) "show version number",
    Option ['d'] ["directory"] (ReqArg specPath "DIRECTORY") "directory to process",
    Option ['e'] ["extension"] (ReqArg specExtension "EXTENSION") "only process files with specified extension",
    Option [] ["start-index"] (ReqArg specStartIndex "INT") "start numbering at given index",
    Option [] ["prefix"] (ReqArg specPrefix "STRING") "prepend prefix to each file name",
    Option [] ["no-safety"] (NoArg specSafety) "don't ask safety confirmation before renaming",
    Option [] ["no-log"] (NoArg specLog) "don't print a log of all the affected files to the console"
  ]

-- Set showVersion option to True
specVersion opts = return opts { 
    optVersion = True
  }

-- Check if the specified argument is a directory. If so, return that. If not, error.
specPath arg opts = return opts { 
    optPath = do
      exists <- doesDirectoryExist arg
      if exists 
        then return arg 
        else error $ "Directory does not exist: " ++ arg
  }

-- Return a filter function for file names. Expects the dot to be given e.g. ".jpg"
specExtension arg opts = return opts {
    optExtension = checkSuffix (words arg)
  }

-- Take a list of possible suffixes and return a function to check a file name against each
checkSuffix :: [String] -> String -> Bool
checkSuffix [] _ = False
checkSuffix (e:es) fileName | isSuffixOf e fileName = True
                            | otherwise = checkSuffix es fileName

-- Return the given argument as an integer.
-- TODO: catch "no parse" exception
specStartIndex arg opts = return opts {
    optStartIndex = (read arg :: Int)
  }

-- Return the given argument as string.
specPrefix arg opts = return opts {
    optPrefix = arg
  }

-- Return False so the safety question won't be asked.
specSafety opts = return opts {
    optSafety = False
  }

-- Return False so no log will be printed after the renaming process
specLog opts = return opts {
    optLog = False
  }

type Handle = (EpochTime, FilePath)

printCopyright = do
  putStrLn "Chronorder  Copyright (C) 2013  Pieter Brandwijk"
  putStrLn "This program comes with ABSOLUTELY NO WARRANTY\n"

main = do
  args <- getArgs
  let (actions,nonOpts,msgs) = getOpt RequireOrder options args
  -- Terminate program if unrecognized options are found
  attempt (nonOpts /= []) (error $ "unrecognized arguments: " ++ unwords nonOpts)
  -- Terminate program if any errors in the options are found
  attempt (msgs /= []) (error $ concat msgs ++ usageInfo "Usage: chronorder [OPTION...]" options)
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optVersion = showVersion,
                optExtension = extensionFilter,
                optPath = path,
                optStartIndex = index,
                optPrefix = prefix,
                optSafety = safety, 
                optLog = log } = opts
  -- Show version and exit if -v option is specified
  attempt showVersion (putStrLn "Chronorder version \"1.0\"" >> exitWith ExitSuccess)
  printCopyright
  directory <- path
  let fileFilter = extensionFilter -- other filters can be added with (.)
  contents <- getDirectoryContents directory
  statuses <- mapM getFileStatus contents
  let join = zip statuses contents
  let files = filter (isRegularFile . fst) join
  let filteredFiles = filter (fileFilter . snd) files
  let fileMap :: [Handle]
      fileMap = [ (modificationTime status, name) | (status, name) <- filteredFiles ]
  let sortedFileMap = qsort fileMap
  let newNameMap = genName (map snd sortedFileMap) index prefix
  -- Don't ask for safety confirmation if --no-safety option is specified
  attempt safety (safetyQuestion directory newNameMap)
  -- Do the actual renaming
  mapM_ renameFile newNameMap
  -- Don't print log if --no-log is specified
  attempt log (mapM_ printLog newNameMap)

-- Helper function to reduce boiler plate code. Only execute action if condition is true.
attempt :: Bool -> IO () -> IO ()
attempt True action = action
attempt False action = return ()

-- Prompt the user to confirm the files in the directory should indeed be renamed.
safetyQuestion :: FilePath -> [(FilePath, FilePath)] -> IO ()
safetyQuestion directory newNameMap = do
  putStrLn ("Caution, this will rename " ++ (show $ length newNameMap) ++ " files in directory " ++ directory)
  putStrLn "Are you sure you want to continue? (enter 's' to preview the effects) [y/n/s]: "
  char <- getChar
  case char of
    'y' -> return ()
    's' -> mapM_ printLog newNameMap >> safetyQuestion directory newNameMap
    _   -> putStrLn "Nothing changed" >> exitWith ExitSuccess

-- The actual action that renames the file on disk
renameFile :: (FilePath, FilePath) -> IO ()
renameFile (old, new) = rename old new

-- Print a record of the old and new name of a file
printLog :: (FilePath, FilePath) -> IO ()
printLog (a,b) = putStrLn $ a ++ " -> " ++ b

-- Generate a list of tuples with an old and new file path.
-- The new file path is based on the old file path, the index number and optional prefix
-- TODO: Check for double extensions (takeBaseName only trims last extension)
genName :: [FilePath] -> Int -> String -> [(FilePath, FilePath)]
genName [] _ _ = []
genName (filePath:xs) i prefix = (filePath, newFilePath) : genName xs (i+1) prefix
  where
    baseName = takeBaseName filePath
    newBaseName = show i ++ " " ++ prefix ++ baseName
    newFilePath = replaceBaseName filePath newBaseName

-- Quick sort algorithm applied to Handle type. Files are sorted by ascending modification date.
qsort :: [Handle] -> [Handle]
qsort [] = []
qsort (x:xs) = (qsort older) ++ [x] ++ (qsort rest)
  where
    older = filter (olderThan x) xs
    rest = (xs \\ older) -- files younger and of equal date
    olderThan :: Handle -> Handle -> Bool
    olderThan (atime,_) (btime,_) = atime > btime
