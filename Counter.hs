-- | I have no idea 
module Counter where

import Data.List.Split
import System.IO
import System.Process
import Data.List
import Data.Time
import System.Locale
import System.Directory
import Control.Monad

-- Data Types

data Session = Session {date :: String
                      , time :: String
                      , nwords :: Int} deriving (Show, Eq)

{- ^ Session is Date, Time, Nwords

interp. A completed session that records the date, time and number of words

@
s1 = Session \"06\/24\/13\" \"14:57:46\" 4944
s2 = Session \"06\/24\/13\" \"15:22:31\" 5207
s3 = Session \"07\/02\/13\" \"17:10:03\" 7546
@

@
fnForSession s = date s ++ time s ++ (show (nwords s))
@
-}

type LOS = [Session]
{- ^ ListOfSession is one of 

- []

- Session:ListOfSession

@
los1 = []
los2 = [s1]
los3 = [s1, s2]
los4 = [s1, s2, s3]
@

@
fnForLos [] = []
fnForLos (x:xs) = (fnForSession x):(fnForLos xs)
@
-}


type CSVSession  = String
-- ^ singles instance of CSVSessions

type CSVSessions = String
{- ^ interp. is a string comma seperated version of Sessions

@
text = \"06\/24\/13, 14:57:46, 4944\\n06\/24\/13, 15:22:31, 5207\\n07\/02\/13, 17:10:03, 7546\\n\":: CSVSessions
@
-}

type TexcountString = String
{- ^ is one of 

   - Succesful word count

   - Error: File not found

   - Error: No files specified

   - Error: Syntax help
 
interp. the string that is returned by running texcount

@
fnForTexcountString ts 
  | isInfixOf \"File not found\" ts = error ts
  | isInfixOf \"Syntax\" ts         = error ts
  | isInfixOf \"No file\" ts        = error ts
  | otherwise                     = \"yup\"
@
-}

type WordCount = Int
{-^ the number of words written that is counted by the system process texcount
-}

-- time
-- let time = (formatTime defaultTimeLocale "%d/%m/%y" x) ++ (formatTime defaultTimeLocale "%X" x)
-- parseTime defaultTimeLocale "%d/%m/%y %X" "06/24/13 14:57:46":: Maybe UTCTime
--     -> Just 2013-12-06 14:57:46 UTC


-- Functions

daily :: LOS -> LOS
-- ^takes a list of sessions and returns the latest entry for the day
daily []     = []
daily (x:[]) = [x]
daily (x:xs)
  | not (compareDates x (head xs)) = x:daily xs 
  | otherwise                      = daily xs
  where
    compareDates :: Session -> Session -> Bool
    -- ^takes two sessions and compares their dates and returns a boolean
    compareDates s1 s2 = date s1 == date s2

multisToLOS :: CSVSessions -> LOS
-- ^takes a string of multiple CSV lines and converts them to a list of Sessions
multisToLOS str = map stringToLOS (lines str)
  where 
    stringToLOS :: CSVSession -> Session
    -- ^takes a CSV string and returns a list of Sessions
    stringToLOS str = Session (head s) (s!!1) (read (s!!2) :: Int)
        where s = splitOn ", " str 

losToString :: LOS -> CSVSessions 
-- ^takes a list of sessions and compiles them to a string
losToString = foldr ((++) . sessionToString) ""
  
sessionToString :: Session -> CSVSession 
-- ^ takes a session and converts it to strig
sessionToString s = date s ++ ", " ++ time s ++ ", " ++ (show (nwords s)++ "\n")

makeIndex :: FilePath -> IO()
-- ^ reads a file of CSV Sessions and writes a file of CSV Sessions 
-- with only the last entry for the day
makeIndex file = do
  string <- readFile file
  let str = losToString $ daily $ multisToLOS string
  writeFile "index.txt" str

texcount :: FilePath -> IO WordCount
-- ^ calls texcount and returns the total number of words
texcount file = do
  str <- readProcess "texcount" [file] []
  return (getWordCount str)

getWordCount :: TexcountString -> WordCount
-- ^ takes the string output of texcount and returns the number of words
-- as int
--
-- I need to refactor this with maybe the Maybe monad.
getWordCount ts 
  | "File not found" `isInfixOf` ts = error ts
  | "Syntax"         `isInfixOf` ts = error ts
  | "No file"        `isInfixOf` ts = error ts
  | otherwise                       = read (dropSuffix $ dropPrefix ts)::WordCount
  where
    dropPrefix :: String -> String
    -- because it's more fun than regex!
    dropPrefix ('W':'o':'r':'d':'s':' ':'i':'n':' ':'t':'e':'x':'t':':':' ':xs) = xs
    dropPrefix (_:xs) = dropPrefix xs

    dropSuffix :: String -> String
    dropSuffix ('\n':xs) = []
    dropSuffix (x:xs) = x:dropSuffix xs

createSession :: FilePath -> IO Session
createSession file = do
    nword <- texcountDirectory file 
    utc  <- getCurrentTime
    let date = formatTime defaultTimeLocale "%d/%m/%y" utc
        time = formatTime defaultTimeLocale "%X" utc
    return (Session date time nword)

logSession :: FilePath -> IO ()
-- ^ logs the current session to session-history.txt
logSession file = do
  session <- createSession file
  appendFile "session-history.txt" (sessionToString session)
  putStrLn ("You have written " ++ (show (nwords session)) ++ " words")

startSession :: FilePath -> IO () 
startSession file = do
  session <- texcountDirectory file
  writeFile "track-session.txt" (show session)
  putStrLn "Session Started"

trackSession :: FilePath -> IO ()
-- ^ shows how much you have written in that current session
trackSession file = do
  stringsession <- readFile "track-session.txt"
  let session = (read stringsession :: Int)
  nwords <- (texcountDirectory file) 
  let output = show (nwords - session)
  putStrLn output

texcountDirectory :: FilePath -> IO Int
texcountDirectory dir = do
  files <- getDirectoryContents dir
  let texfiles = getTexOnly files
  output <- (texcountList dir texfiles)
  return output

getTexOnly :: [String] -> [String]
-- ^ gets the files that end in .tex in list 
getTexOnly [] = []
getTexOnly xs = filter (".tex" `isInfixOf`) xs

texcountList :: FilePath -> [String] -> IO WordCount
-- ^ aplies texcount to a list of files in a directory
texcountList dir files = do
  let dirfiles = map (dir ++) files
  let iowordcount = map texcount dirfiles
  fmap sum (sequence iowordcount)

showProgress :: FilePath -> IO ()
showProgress file = do
  output <- readFile file
  putStrLn output
