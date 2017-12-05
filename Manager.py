import threading, os, sys
from flask_restful import Resource, Api, reqparse
from flask import Flask, request, jsonify
import Utilities as utils
import json

NUM_WORKERS = 0
ROOT_DIR = "ManagerDir"
repository = None

# working variables
commits_list = {}
current_commit_index = 0

# results variables
complexity_results = []
total_time = 0.0

app = Flask(__name__)
api = Api(app)

class Manager(Resource):

    def get(self):
        global current_commit_index, commits_list

        utils.print_to_console("Manager", "In get method")
        running = utils.get_outstanding_commits(commits_list, current_commit_index)
        if not running:
            utils.print_to_console("Manager", "There are no more commits. Running == False now.")
            utils.output_results(NUM_WORKERS, total_time, complexity_results)
            return {"commit": -1, "running": "False"}
        else:
            commit = utils.get_next_piece_of_work(commits_list, current_commit_index)
            current_commit_index += 1

        return {"commit": commit, "running": running}


    def post(self):
        global total_time

        utils.print_to_console("Manager", 'In post method')
        time_taken = request.json()['time_taken']
        average_complexity = request.json()['average_complexity']
        # record this in our array of results for each commit
        complexity_results.append(average_complexity)
        total_time += time_taken


class RegisterWorker(Resource):

    def get(self):
        global NUM_WORKERS
        utils.print_to_console("RegisterWorker", 'In get method of worker registration')
        registration_request = request.get_json()['registration_request']
        if registration_request is True:
            response = {"worker_id": NUM_WORKERS}
            NUM_WORKERS += 1
        else:
            response = {"worker_id": None}
        return response


# Add url handles for registration of worker, and general worker requests
api.add_resource(Manager, '/')
api.add_resource(RegisterWorker, '/register_worker')


# Requires the number of workers as arg[1]
if __name__ == '__main__':
    global commits_list, NUM_WORKERS, repository, time_taken

    # get repo set up first
    repository = utils.get_git_repository(ROOT_DIR)
    commits_list = utils.get_commits_as_list(ROOT_DIR)
    utils.print_to_console("Manager", "Loaded commits on manager side")

    # wait for required number of workers to join before doing anything else
    app.run(host='127.0.0.1', port=5000)
    while NUM_WORKERS < sys.argv[1]:
        pass
