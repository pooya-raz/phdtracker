module Main where

import System.Environment
import System.Directory  
import System.IO  
import Data.List  

import Counter

phdpath = "/Users/pooya/Documents/PHD-Thesis/mythesis/Chapters/"
startPhd = startSession phdpath
trackPhd = trackSession phdpath
logPhd = logSession phdpath
showPhd = showProgress "index.txt"
makeIndexPhd = makeIndex "session-history.txt"

dispatch :: [(String, IO ())]  
dispatch =  [ ("start", startPhd)
            , ("track", trackPhd)
            , ("log", logPhd)
            , ("show", showPhd)
            , ("make-index", makeIndexPhd)
            ]
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action
