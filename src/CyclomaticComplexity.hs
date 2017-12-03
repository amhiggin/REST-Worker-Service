-- Intended to be a file to do all the monkey work with the libraries e.g. Git

module CyclomaticComplexity (
getCommitList, cloneRepo
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

main :: IO ()
main = do
    getCommits
    cloneRepo
    changeCommit

-- Function to get a list of commits from github repo
getCommitList :: IO
getCommitList = do
    tmp <- commitsFor "argon" "haskell" --FetchAll
    case tmp of
        (Left error)    -> putStrLn $ "Error: " ++ (show error)
        (Right commits) -> do
            let tmp = intercalate "\n" $ map formatCommit (toList commits)
            putStrLn tmp

formatCommit :: GitHub.Commit -> String
formatCommit commit =
   (show $ GitHub.commitSha commit)



-- Function to clone a git repository given a url
cloneRepo :: String -> String -> IO ()
cloneRepo url dir = do
  exists <- doesDirectoryExist dir		    -- if it exists locally, don't need to clone
  unless exists $ callProcess "git" ["clone", url]  -- if it doesn't, clone it
  liftIO $ putStrLn "Finished cloning url"
  return ()


-- Function to change commit version
changeCommit :: String ()
changeCommit