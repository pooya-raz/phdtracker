module PoosterSpec where

import Test.Hspec
import Counter
import Main

-- | Data Types
-- | Examples
s1 = Session "06/24/13" "14:57:46" 4944
s2 = Session "06/24/13" "15:22:31" 5207
s3 = Session "07/02/13" "17:10:03" 7546
--funForSession s = date s

--CSVSession
-- interp. is a string comma seperated version of Session
text = "06/24/13, 14:57:46, 4944\n06/24/13, 15:22:31, 5207\n07/02/13, 17:10:03, 7546\n"

-- ListOfSession is one of 
-- []
-- Session:ListOfSession
los1 = []
los2 = [s1]
los3 = [s1, s2]
los4 = [s1, s2, s3]

{-
funForLos [] = []
funForLos (x:xs) = x:funForLos xs
-}

tests:: IO ()
tests = hspec $ do
  describe "makeIndex" $ do
    it "reads a file of CSVSessions and writes a file of CSVSessions with only the last entry for the day"$ do
      makeIndex "./samples/sample-history.txt"
      sampleindex <- readFile "./samples/sample-index.txt"
      index <- readFile "index.txt"
      sampleindex `shouldBe` index

  describe "daily" $ do
    it "takes a list of Sessions and returns a list of Sessions with only the last entry for a day" $
      daily los4 `shouldBe` [s2,s3]
    context "edge cases" $ do
      it "returns an empty list when given an empty list" $
        daily los1 `shouldBe` []
      it "returns the same list of Session if given only one in list" $
        daily los2 `shouldBe` [s1]

  describe "losToString" $
    it "takes a list of sessions and compiles them to a CSVSession" $ 
      losToString los4 `shouldBe` text

  describe "multisToLOS" $
    it "takes a string of multiple CSVSession lines and converts them to a list of Sessions" $ do
      multisToLOS text `shouldBe` los4
      multisToLOS [] `shouldBe` los1

  describe "texcount" $ do
    it "calls texcount and returns the total number of words" $ do 
      output <- texcount "/Users/pooya/Programming/haskell/pooster-tools/samples/3word.tex" 
      output `shouldBe` 3
  --  it "should return error" $ do
  --    texcount "nothing" `shouldThrow` anyException

  describe "getWordCount" $
    it "takes the string output of texcount and returns the number of words as int" $ do
      --needs the tests on texcountString
      let tcoutput = "File: /Users/pooya/Programming/haskell/pooster-tools/samples/3word.tex\nEncoding: ascii\nWords in text: 3\nWords in headers: 1\nWords in float captions: 0\nNumber of headers: 1\nNumber of floats: 0\nNumber of math inlines: 0\nNumber of math displayed: 0\n\n"
      getWordCount tcoutput `shouldBe` 3
  
  describe "createSession" $
    it "makes a new Session by running texcount on the directory and adding the date and time." $ do
      output <- createSession "/Users/pooya/Programming/haskell/pooster-tools/samples/sample-directory/" 
      nwords output `shouldBe` 6

  describe "startSession" $
    it "starts a writing session" $ do
      startSession "/Users/pooya/Programming/haskell/pooster-tools/samples/3word.tex" 
      output <- readFile  "/Users/pooya/Programming/haskell/pooster-tools/track-session.txt"
      output `shouldBe` "3"
 
  describe "trackSession" $
    it "shows how much you have written in that current session" $ do
      output <- trackSession  "/Users/pooya/Programming/haskell/pooster-tools/samples/3word.tex" 
      output `shouldBe` 0

  describe "texcountDirectory" $
    it "runs texcount on all files in a directory" $ do
      let dir = "/Users/pooya/Programming/haskell/pooster-tools/samples/sample-directory/"
      output <- texcountDirectory dir 
      output `shouldBe` 6

  describe "getTexOnly" $
    it "gets the files that end in .tex" $
      getTexOnly ["file.txt", "file.tex", "files.tex"] `shouldBe` ["file.tex", "files.tex"]


