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


-- Not quite sure where this will be passed in the program flow yet
const argon_repo_url = "http://github.com/rubik/argon"


-- This is the method that is called by Lib.hs
-- Should execute a cmd command
calculateCyclomaticComplexity :: String -> IO (Float)
calculateCyclomaticComplexity file_path = do
  callProcess "stack" ["exec --argon --json ", file_path]


-- GIT
type GitRepository = (String,String,String)

-- Function to clone a git repository given a url
-- Returns the dir where the repo has been cloned to
cloneRepository :: String -> String -> IO ()
cloneRepository url directory = do
  exists <- doesDirectoryExist (directory)		    -- if it exists locally, don't need to clone
  unless exists $ callProcess "git" ["clone", url]  -- if it doesn't, clone it
  liftIO $ putStrLn "Finished cloning url"
  return ()


-- TODO implement (Function to change commit version)
-- changeCommit :: String ()
-- changeCommit


fetchSpecificCommitVersion :: GitRepository -> IO ()
fetchSpecificCommitVersion (url, directory, commit) = do
  cloneRepository argon_repo_url directory -- clone repo
  readCreateProcess ((proc "git" ["reset", "--hard", commit]) {cwd = Just directory}) "" -- get a particular commit

-- TODO: This function should calculate the complexity of an individual file
-- calcFileComplexity :: String -> IO ()
--    do some stuff here
-- return Float :: value

-- This function should initiate the calculation of the complexity of a set of files, and return the result
-- Should be used by the client, since the server will pass a block of files to calculate complexity on
--calcBatchComplexity
-- for file : files:
--  calcFileComplexity


