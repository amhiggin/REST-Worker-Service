from flask_restful import Resource, Api, reqparse
from flask import Flask, request, jsonify
import Utilities as utils
import sys

NUM_WORKERS = 0
ROOT_DIR = "ManagerDir"
repository = None

# working variables
commits_list = {}
current_commit_index = 0
finished = False
workers_terminated = 0

# results variables
complexity_results = []
start_time = 0.0
end_time = 0.0
total_time = 0.0

app = Flask(__name__)
api = Api(app)

class Manager(Resource):

    def get(self):
        global current_commit_index, commits_list, finished, start_time

        # start timing if this is the first request
        if current_commit_index == 0 and int(required_num_workers) == NUM_WORKERS:
            start_time = utils.get_time()
            print 'Started timing at {0}'.format(str(start_time))

        running = utils.get_outstanding_commits(commits_list, current_commit_index)
        if not running and finished is False:
            finished = True
            check_if_workers_terminated_and_shutdown()
            return {"commit": -1, "running": "False"}
        elif not running and finished is True:
            check_if_workers_terminated_and_shutdown()
            return {"commit": -1, "running": "False"}
        else:
            if NUM_WORKERS != int(required_num_workers):
                commit = -2
            else:
                commit = utils.get_next_piece_of_work(commits_list, current_commit_index)
                current_commit_index += 1
                # still waiting for all workers to join: send back commit == -2
            return {"commit": commit, "running": running}


    def post(self):
        global complexity_results, end_time

        response = request.get_json()
        average_complexity = response['average_complexity']
        # record this in our array of results for each commit
        complexity_results.append(average_complexity)

        # check if this is the last post, i.e. stop timing
        if len(complexity_results) == len(commits_list):
            end_time = utils.get_time()
            print 'Finished timing at '.format(str(end_time))

class RegisterWorker(Resource):

    def get(self):
        global NUM_WORKERS
        registration_request = request.get_json()['registration_request']
        if registration_request is True:
            response = {"worker_id": NUM_WORKERS}
            NUM_WORKERS += 1
            utils.print_to_console('RegisterWorker:', 'Registered new worker {0}'.format(NUM_WORKERS))
        else:
            response = {"worker_id": None}

        return response


def check_if_workers_terminated_and_shutdown():
    global workers_terminated, NUM_WORKERS
    workers_terminated += 1

    if workers_terminated == NUM_WORKERS:
        shutdown()

def shutdown():
    # http://flask.pocoo.org/snippets/67/
    func = request.environ.get('werkzeug.server.shutdown')
    if func is None:
        raise RuntimeError('Not running with the Werkzeug Server')
    func()

# Add url handles for registration of worker, and general worker requests
api.add_resource(Manager, '/')
api.add_resource(RegisterWorker, '/register_worker')


if __name__ == '__main__':

    # Some cleanup of any previous files
    required_num_workers = sys.argv[1]
    utils.clean_up_before_init(required_num_workers)
    # get repo set up first
    repository = utils.get_git_repository(ROOT_DIR)
    commits_list = utils.get_commits_as_list(ROOT_DIR)

    app.run(host='127.0.0.1', port=5000, debug=False)


    total_time = end_time - start_time

    utils.print_to_console("Manager", "Manager finished")
    utils.output_results(NUM_WORKERS, total_time, complexity_results)

