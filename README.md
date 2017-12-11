# RESTServiceSystem
<b>Amber Higgins, M.A.I. Computer Engineering, 13327954. For CS7NS1 Scalable Computing, TCD.</b>

<b>Task</b>: <i>"To construct a REST service system, focused on the efficient computation of code complexity for a given repository, utilising a set of nodes as appropriate to minimise execution time from submission to result return".</i>

## Distributed RESTful System
A RESTful service to calculate the Cyclomatic Complexity (CC) of a Github repository. 

A Manager node delegates work to Worker nodes, which request more work once they have completed a piece of work - the <i>work-stealing pattern</i>. 'Work' is the calculation of the average cyclomatic complexity (CC) for each commit in a Github repository.

### Manager Node
Manages connected workers, listens and responds to requests for work, and gives each node a commit to calculate the average CC for.

Upon start-up, the Manager:
* Clones the required repository to its own root directory, <i>/ManagerDir</i>.
* Cleans up after any previous worker nodes, i.e. deletes any existing '<i>WorkerDir</i>' directories. 
* Becomes accessible for registration of workers at <i>http://127.0.0.1:5000/register_worker</i>, and at <i>http://127.0.0.1:5000</i> for incoming requests for work.
* Startup param includes <i>required_num_workers</i> as <b>sys.argv[1]</b>. This allows the manager to stall the provision of files to calculate the average CC for, until the required number of worker nodes have been registered.

A screenshot of the server starting up for <b>required_num_workers = 5</b> is shown below.

![server_init](https://github.com/amhiggin/RESTServiceSystem/blob/master/Results%20and%20Screenshots/Initialising%20manager%20from%20launch_manager.sh%20script.PNG)

Once it has received all of the results for all of the delegated work, it:
* Terminates the workers;
* Averages the results returned by all of the workers;
* Writes the result to the file <b>complexity_results.txt</b>. 

![server_exit](https://github.com/amhiggin/RESTServiceSystem/blob/master/Results%20and%20Screenshots/Outputting%20of%20calculation%20results%20to%20file%20from%20Manager.PNG)

### Worker
Requests work from the Manager, and operates on the delegated work item it is assigned before sending the result back to the Manager. Calculates the CC for <u>each file</u> in a commit specified by the Manager, using the Radon Python CC Library. It then calculates and returns the average complexity over the whole commit, to the Manager. This is the 'work' that it carries out.

* The lifecycle of the worker is to:
    * Register itself with the Manager at url <i>http://127.0.0.1:5000/register_worker</i>, obtaining a unique worker ID. 
    * Initialise its individual properties, including cloning the required repository to its own root directory.
    * Poll the manager until work is given to it. A delay here may occur where the manager is waiting for the required number of workers to register with it.
    * Thereafter, once there are sufficient workers registered, each worker completes and requests delegated work items until there is no work left to do.  
    * When all work has been completed, the Manager will send {'running': False} as JSON to instruct the Worker to finish executing. It is at this point that the Worker terminates.
    
![worker_operation](https://github.com/amhiggin/RESTServiceSystem/blob/master/Results%20and%20Screenshots/Initialisation%20and%20operation%20of%205%20worker%20nodes.PNG)
    
#### Some subtleties:
* Each worker clones its own copy of the repository in order to simplify the issue of conflicts. This does result in more time to launch the workers, but doesn't directly impact on the time taken to execute the cyclomatic complexity of the repository since this is only measured from when all workers are ready.
* No worker will be given work until all of the expected number of workers have registered with the Manager.
* As a resiliency measure, if a worker is launched before there is a Manager to register with, it will wait until the Manager is running rather than throwing a ConnectionException.


## Performance Results
As mentioned, this system was implemented using the <b>work-stealing pattern</b>. The performance results for between 1 and 5 workers are shown in the graph below.

![performance_graph](https://github.com/amhiggin/RESTServiceSystem/blob/master/Results%20and%20Screenshots/Graph%20-%20Performance%20for%205%20workers.PNG)

It was found that there was an inversely proportional relationship between the number of worker nodes completing the calculation, and the time taken to complete it. This is evident from the marked decrease in the amount of time required to complete the calculation, for an increasing number of nodes.

At the point where 5 worker nodes are used to complete the work, it is clear that the graph is levelling off. This is because at some point, the time required to set up and manage the work-delegation for additional nodes becomes costly, and the performance cannot be improved any further.


## Launch Instructions
### Launch Manager
The <i>launch_manager.sh</i> script is used to launch the Manager node. This should be run before the workers are launched.
* The script takes <b>num_required_workers</b> as <b>$1</b> (command line argument).
* All dependencies are installed by this script, before launching the Manager node. The dependencies are specified in <i>requirements.txt</i>.
* Specifically:

<b>Note:</b> There is an issue with access permissions for the cleanup() method used by the Manager node on start-up. This means that when using Windows, between subsequent runnings of the scripts the user should manually delete the dirs <i>ManagerDir/</i> and all <i>WorkerDirX</i>s to ensure that this cleanup has occurred.

### Launch Workers
The <i>launch_workers.sh</i> script is used to launch a number of Worker nodes. This should be run after the Manager has been launched.
* The script takes the number of workers as as <b>$1</b>, similarly to the <i>launch_manager.sh</i> script.
* Each of the workers is then launched in succession.

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
