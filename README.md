# RESTServiceSystem
A REST service system to compute the cyclomatic complexity of a github codebase on a distributed set of nodes.
The system implements the <i>work-stealing pattern</i>, where the worker nodes request work from the management node rather than waiting for work to be delegated to them.

Implementation heavily based off the implementation by Prof. Stephen Barrett, SCSS TCD, available at: http://www.bitbucket.org/esjmb/use-cloudhaskell

## Packages
TODO add all packages
* Argon <b>version (TODO)</b>
* Git
* Github
* Etc

## Worker
* Calculates the cyclomatic complexity of a given segment of a repository
* Will probably have its own copy of the repository for each commit, that it can download and update (with each commit) at runtime

## Manager
<b>TODO add some notes here</b>

## Extra Notes
Work on this project started later than expected, due to the amount of time that was put into the Distributed File System and Chat Server tasks for the same module.
