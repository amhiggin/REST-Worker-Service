-- Intended to be a file to do all the monkey work with the libraries e.g. Git

module CyclomaticComplexity (
   getCommits,
   cloneRepo,
   changeCommit,
   calcFileComplexity,
   calcBatchComplexity
) where

import Control.Monad
import Git
import Github.commitSha
import Github.Commit
import Argon
import System.FilePath
import System.Process
import System.Directory		-- doesDirectoryExist
import System.IO


const repo_url = "http://github.com/rubik/argon"


main :: IO ()
main = do
    cloneRepo
    changeCommit

-- Function to clone a git repository given a url
-- Returns the dir where the repo has been cloned to
cloneRepo :: String -> String -> IO ()
cloneRepo url dir = do
  exists <- doesDirectoryExist (dir ++ url)		    -- if it exists locally, don't need to clone
  unless exists $ callProcess "git" ["clone", url]  -- if it doesn't, clone it
  liftIO $ putStrLn "Finished cloning url"
  return dir


-- TODO implement (Function to change commit version)
-- changeCommit :: String ()
-- changeCommit


getAllCommits :: String -> Process([String])
getAllCommits url = do
   dir <- cloneRepo "argon" "/tmp/"
   -- commitsCmd = " cd "++ folder ++ "&&" ++"/usr/bin/git rev-list HEAD"
   liftIO $ putStrLn $ "getting commits"  ++  show dir
   (_, Just hout, _, _) <- liftIO $ createProcess (shell $ commitsCmd ){ std_out = CreatePipe }
   commits <- liftIO $ hGetContents hout
   return $ filter (""/=) (splitOn "\n" commits)


-- TODO: This function should calculate the complexity of an individual file
-- calcFileComplexity :: String -> IO ()
--    do some stuff here
-- return Double :: value



-- This function should initiate the calculation of the complexity of a set of files, and return the result
-- Should be used by the client, since the server will pass a block of files to calculate complexity on
--calcBatchComplexity
-- for file : files:
--  calcFileComplexity


