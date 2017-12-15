# RESTServiceSystem
<b>Amber Higgins, M.A.I. Computer Engineering, 13327954. For CS7NS1 Scalable Computing, TCD.</b>

<b>Task</b>: <i>"To construct a REST service system, focused on the efficient computation of code complexity for a given repository, utilising a set of nodes as appropriate to minimise execution time from submission to result return".</i>

## Distributed RESTful System
A RESTful service to calculate the average Cyclomatic Complexity (CC) of the commits in a Github repository.

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

A screenshot below shows the outputting of results.
![server_exit](https://github.com/amhiggin/RESTServiceSystem/blob/master/Results%20and%20Screenshots/Outputting%20of%20calculation%20results%20to%20file%20from%20Manager.PNG)

### Worker
Requests work from the Manager, and operates on the delegated work item it is assigned before sending the result back to the Manager. Calculates the CC for <u>each file</u> in a commit specified by the Manager, using the Radon Python CC Library. It then calculates and returns the average complexity over the whole commit, to the Manager. This is the 'work' that it carries out.

* The lifecycle of the worker is to:
    * Register itself with the Manager at url <i>http://127.0.0.1:5000/register_worker</i>, obtaining a unique worker ID. 
    * Initialise its individual properties, including cloning the required repository to its own root directory.
    * Poll the manager until work is given to it. A delay here may occur where the manager is waiting for the required number of workers to register with it.
    * Thereafter, once there are sufficient workers registered, each worker completes and requests delegated work items until there is no work left to do.  
    * When all work has been completed, the Manager will send {'running': False} as JSON to instruct the Worker to finish executing. It is at this point that the Worker terminates.

Below is shown a screenshot of multiple workers performing the calculation on the repository, for different commits.
![worker_operation](https://github.com/amhiggin/RESTServiceSystem/blob/master/Results%20and%20Screenshots/Initialisation%20and%20operation%20of%205%20worker%20nodes.PNG)
    
#### Some subtleties:
* Each worker clones its own copy of the repository in order to simplify the issue of conflicts. This does result in more time to launch the workers, but doesn't directly impact on the time taken to execute the cyclomatic complexity of the repository since this is only measured from when all workers are ready.
* No worker will be given work until all of the expected number of workers have registered with the Manager.
* As a resiliency measure, if a worker is launched before there is a Manager to register with, it will wait until the Manager is running rather than throwing a ConnectionException.

### Full Explanation of a Single Run of the Calculation
The system operates as follows (assuming the <i>launch_manager.sh</i> and <i>launch_workers.sh</i> scripts are being used to run the application):
1. The Manager node starts initialising the system. It has been provided with the number of workers it should expect to register.
    * First, it removes existing <i>/ManagerDir</i> and <i>WorkerDir</i> directories from any previous runs.
    * Next, it clones a new copy of the specified repository to <i>ManagerDir</i>, it's own root directory.
    * Then, it makes itself available to the workers.
2. At the same time that the Manager is starting up, Workers may also be starting up.
    * Each worker immediately begins to request to register with the Manager. If the manager is not yet available, the worker waits.
    * Once the Manager node is available, the Worker successfully sends the registration request. The Manager will receive the request, and respond with a unique ID for the worker.
    * The worker will create its own root directory (including its new ID), e.g. <i>/WorkerDir25</i> for a worker with ID = 25. It will then proceed to clone the specified repository to this directory.
    * As soon as this initialisation is done, the Worker starts to poll the Manager for work.
3. The Manager will wait until all of the workers it expects to register, have registered. This means that it will continue to register workers until, for example, it has registered 25 workers (if this is the number that was specified in the <i>launch_manager.sh</i> script).
    * Remember that all of the Workers start polling the Manager for work as soon as they are initialised. These Workers will be given the response <b>{"commit": -2, "running": True}</b>, telling them that there is no work available yet.
    * Once all of the required workers have registered, the Manager starts to delegate work. This is a response to the request for work in the form <b>{"commit": 0bc76a0210035540b53a8588f1b2585ee62fd691 , "running": True}</b> for example, where the 'commit' field is the hash of a commit in the respository. Since the worker has cloned the repository locally, it is able to check out it's own copy of the repository at this commit.
    * <b>NB:</b> Once the Manager knows that all of the required workers are registered, the next request for work that it receives from a worker will start the timer.
4. Thereafter, the Workers complete work:
   * Each worker checks out the repository at the specified commit, and fetches the files contained.
   * It iterates over each <b>python file</b> (<i>.py</i> or <i>.pyc</i>) in the commit, calculating the cyclomatic complexity using the Radon library. This complexity is then added to a running total for the commit.
   * Once the complexity of all of the files in the commit has been calculated, the complexity is averaged by dividing by the number of files. It is this average value that is sent back to the Worker.
   * The cycle begins again with the Worker requesting more work.
5. Each time the Manager receives a result from one of the Workers, it is stored in a data structure along with the results received from all other workers.
    * The Manager continues to iterate through the commits, providing each subsequent commit hash as a response to a request for work from the Workers.
    * After the Manager has delegated all of the commits, any new requests for work will be given the response <b>{"commit": -1, "running": "False"}</b>. When received by the Worker that sent the request, the Worker will terminate.
    * <b>NB:</b> As soon as the Manager has received all of the results from the Workers, it stops the timer.
    * Once the manager knows that all of the Workers have been given a response telling them to terminate, it will terminate its service.
6. Finally, the Manager must calculate and output the results of the operation.
    * The Manager calculates the average cyclomatic complexity for all of the commits, by taking the sum of all the results returned by the Workers and dividing it by the number of results.
    * The Manager calculates the total time taken to run the calculation on the repository, by subtracting the <i>start_time</i> from the <i>end_time</i>.
    * Finally, the results are written out to i) the console window, and ii) a file called <b>complexity_results.txt</b>, which is created if it doesn't already exist. These results are the number of workers, the average commit complexity, and the time taken.

## Performance Results
As mentioned, this system was implemented using the <b>work-stealing pattern</b>. The performance results for between 1 and 28 workers are shown in the graph below.

![performance_graph](https://github.com/amhiggin/RESTServiceSystem/blob/master/Results%20and%20Screenshots/Graph%20-%20Performance%20for%2028%20workers.PNG)

It was found that there was an inversely proportional relationship between the number of worker nodes completing the calculation, and the time taken to complete it. This is evident from the marked decrease in the amount of time required to complete the calculation, for an increasing number of nodes.

At the point where 13 worker nodes are used to complete the work, it is clear that the graph is levelling off. This is because at some point, the time required to set up and manage the work-delegation for additional nodes becomes costly, and the performance cannot be improved any further. There is slight fluctuation in the time taken for >14 workers, hovering somewhere between 15-17 seconds. This is a sign that adding more workers simply doesn't increase the performance of the calculation time any further.

The Excel spreadsheet used to produce this graph is included in the repository [here](https://github.com/amhiggin/RESTServiceSystem/blob/master/Results%20and%20Screenshots/Performance%20Graph.xlsx)


## Launch Instructions
### Launch Manager
The <i>launch_manager.sh</i> script is used to launch the Manager node. This should be run before the workers are launched.
* The script takes <b>num_required_workers</b> as <b>$1</b> (command line argument).
* All dependencies are installed by this script, before launching the Manager node. The dependencies are specified in <i>requirements.txt</i>.

<b>Notes:</b>
* When launching the script after previously running the application for a large number of workers, the clean-up of the existing directories may take some time. However, this doesn't impact on the execution time of the calculation.
* There is an issue with access permissions for the cleanup() method used by the Manager node on start-up. This means that when using Windows, between subsequent runnings of the scripts the user should manually delete the dirs <i>ManagerDir/</i> and all <i>WorkerDirX</i>s to ensure that this cleanup has occurred.
* The number of workers specified as <b>$1</b> should be the same for the <i>launch_workers.sh</i> script.

### Launch Workers
The <i>launch_workers.sh</i> script is used to launch a number of Worker nodes. This should be run after the Manager has been launched.
* The script takes the number of workers as as <b>$1</b>, similarly to the <i>launch_manager.sh</i> script.
* Each of the workers is then launched in succession.

<b>Notes:</b>
* When launching the script for a large number of workers, the cloning of the repository for each worker and the handling of all of these workers polling the server, may slow down the start-up of the service. However, this doesn't impact on the execution time of the calculation.
* The number of workers specified as <b>$1</b> should be the same for the <i>launch_manager.sh</i> script.

## Dependencies
Dependencies are listed in <b>requirements.txt</b>, and are installed as part of the launch script <i>launch_manager.sh</i>. 
* Python 2.7.9
* Radon, used to calculate cyclomatic complexity: http://radon.readthedocs.io/en/latest/api.html#module-radon.complexity
* Flask 0.12.2
* Flask-RESTful 0.3.6
* requests 2.18.4

## Extra Notes
* The real work began on this project on Sunday 3rd December, once a significant dent had been made in the work required for the <i>DistributedFileSystem</i> project for this module. This explains the spike in activity over the first week of December, where time was dedicated solely to this project.
* The original implementation of this project was first attempted in Haskell, with heavy influence from Prof. Stephen Barrett's use-cloudhaskell project (available at http://www.bitbucket.org/esjmb/use-cloudhaskell).
However, given the time remaining to complete the project versus the learning curve involved, it was realised that there was too little time available to dedicate to implementing the project in Haskell.
* Prior to 14th December 2017, this repository was public. However, I made the repository private on this date because of the volume of clones of my repository over the course of the previous week, raising concerns about plagiarism. A screenshot of the traffic insights on 15th December 2017 can be found [here](https://github.com/amhiggin/RESTServiceSystem/blob/master/Results%20and%20Screenshots/Traffic%20to%20repository%20-%2015th%20December%202017.PNG).  A copy of the email response from Prof. Stephen Barret can be found [here](https://github.com/amhiggin/RESTServiceSystem/blob/master/Results%20and%20Screenshots/Email%20response%20regarding%20plagiarism%20concerns.PNG).

