# RESTServiceSystem
<b>Amber Higgins, M.A.I. Computer Engineering, 13327954. For CS7NS1 Scalable Computing, TCD.</b>
<b>Task</b>: <i>	To construct a REST service system, focussed on the efficient computation of code complexity for a given repository, utilising a set of nodes as appropriate to minimise execution time from submission to result return.</i>

## Distributed RESTful System
A REST system. A management node gives work to worker nodes, which request more work once they are finished - the <i>work-stealing pattern</i>. Work is the calculation of the average cyclomatic complexity (CC) for each commit in a Github repository.

### Manager
Listens for requests from worker nodes, and gives each node a commit to calculate the average CC for.
* The Manager clones the required repository to its own directory, <i>/ManagerDir</i>.
* Accessible for requests for work at <i>http://127.0.0.1:5000</i>. Waits for requests from workers.
* Startup param includes <i>required_num_workers</i> as <b>sys.argv[1]</b>. This allows the manager to stall the provision of files to calculate the average CC for, until the required number of worker nodes have been registered.

### Worker
* Calculates the average Cyclomatic Complexity for <u>each commit</u> it is given by the Master, using the Radon Python CC Library. This is the 'work' that it carries out.
* First registers itself with the Manager at url <i>http://127.0.0.1:5000/register_worker</i>. Thereafter, the worker requests work from the Manager.
* The lifecycle of the worker is to:
    * Initialise, including cloning the required repository to a directory for itself;
    * Register itself with the manager;
    * Poll the manager until work is given to it. A delay here may occur where the manager is waiting for the required number of workers to register with it;
    * Once there are sufficient workers, each worker requests work until there is no work left to do.  In this case, the Manager will send {'running': False} as JSON to instruct the Worker to finish executing.

## Dependencies
Dependencies are listed in <b>requirements.txt</b>, and are installed as part of the launch script. <b>TODO insert the updated name of the launch script</b>.
* Python 2.7.9
* Radon, used to calculate cyclomatic complexity: http://radon.readthedocs.io/en/latest/api.html#module-radon.complexity
* Flask 0.12.2
* Flask-RESTful 0.3.6
* requests 2.18.4

## Extra Notes
* The real work began on this project on Sunday 3rd December, once a significant dent had been made in the work required for the <i>DistributedFileSystem</i> project for this module. This explains the spike in activity over the first week of December, where time was dedicated solely to this project.
* The original implementation of this project was first attempted in Haskell, with heavy influence from Prof. Stephen Barrett's use-cloudhaskell project (available at http://www.bitbucket.org/esjmb/use-cloudhaskell).
However, given the time remaining to complete the project versus the learning curve involved, it was realised that there was too little time available to dedicate to implementing the project in Haskell.
