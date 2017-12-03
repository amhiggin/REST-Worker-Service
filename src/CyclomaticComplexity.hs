-- | Prime factorization
--
-- Written by Dan Weston
-- <http://westondan.blogspot.ie/2007/07/simple-prime-factorization-code.html>
module CyclomaticComplexity where

import Control.Monad
import Git
import Github.commitSha
import Github.Commit
import Argon

main :: IO ()
main = do
    getCommits


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
