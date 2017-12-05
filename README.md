# RESTServiceSystem
A REST system. A management node gives work to worker nodes, which request more work once they are finished - the <i>work-stealing pattern</i>. Work is the calculation of the average cyclomatic complexity for each commit in a Github repository.

### Manager
* Accessible for requests for work at <i>http://127.0.0.1:5000</i>. Waits for requests from workers.
* Startup param includes num_workers as sys.argv[1].

### Worker
* Calculates the average Cyclomatic Complexity for <u>each commit</u> it is given by the Master, using the Radon Python CC Library. This is the 'work' that it carries out.
* First registers itself with the Manager at url <i>http://127.0.0.1:5000/register_worker</i>.
* Thereafter, the worker requests work from the Manager until there is no work left to do. In this case, the Manager will send {'running': False} as JSON to instruct the Worker to finish executing.

## Dependencies
* Radon, used to calculate cyclomatic complexity: http://radon.readthedocs.io/en/latest/api.html#module-radon.complexity

## Extra Notes
* The original implementation of this project was first attempted in Haskell, with heavy influence from Prof. Stephen Barrett's use-cloudhaskell project (available at http://www.bitbucket.org/esjmb/use-cloudhaskell).
However, given the time remaining to complete the project versus the learning curve involved, it was realised that there was too little time available to dedicate to implementing the project in Haskell.
