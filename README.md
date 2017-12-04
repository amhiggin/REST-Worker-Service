# RESTServiceSystem
A REST system. A management node delegates work to varying numbers of worker nodes, in the form of files from commits to a Github repository.
TODO: figure out if this will be a <b>work-stealing</b> rather than <b>work-delegation</b> pattern.

## Dependencies
* Radon, used to calculate cyclomatic complexity: http://radon.readthedocs.io/en/latest/api.html#module-radon.complexity

## Extra Notes
* The original implementation of this project was first attempted in Haskell, with heavy influence from Prof. Stephen Barrett's use-cloudhaskell project (available at http://www.bitbucket.org/esjmb/use-cloudhaskell).
However, given the time remaining to complete the project versus the learning curve involved, it was realised that there was too little time available to dedicate to implementing the project in Haskell.