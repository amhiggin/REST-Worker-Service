-- Intended to be a file to do all the monkey work with the libraries e.g. Git

-- This part is the declaration of the "class" with its methods inside it
module Utils (
cloneRepo, changeCommit
) where


-- module imports
import System.IO
import Control.Monad.State
import System.FilePath
import Control.Distributed.Process hiding (try)
import Control.Exception
import Prelude hiding (log)
import System.Process
import System.Directory

import System.IO.Temp


-- Function to clone a git repository
cloneRepo :: String -> String -> IO ()
cloneRepo url dir = do
  exists <- doesDirectoryExist dir					-- if it exists locally, don't clone
  unless exists $ callProcess "git" ["clone", url]  -- if it doesn't, clone it
  return ()