import threading, os, sys
from flask_restful import Resource, Api, reqparse
from flask import Flask, request
import Utilities as utils

NUM_WORKERS = 0
commits_list = {}
current_commit_index = 0
work_remaining = {}

app = Flask(__name__)
api = Api(app)

class Manager(Resource):

    def get(self):
        global current_commit_index
        utils.print_to_console("Manager", "In get method")
        commit = utils.get_next_piece_of_work(commits_list, current_commit_index)
        running = utils.get_are_files_remaining(commits_list, current_commit_index)
        current_commit_index += 1
        return {"commit": commit, "running": running}


    def post(self):
        utils.print_to_console("Manager", 'In post method')
        work_response = request.get_json()
        file_names = work_response.json()['file_names']
        average_complexity = work_response.json()['average_complexity']
        # update data structure storing this data


class RegisterWorker(Resource):

    def get(self):
        global NUM_WORKERS
        utils.print_to_console("RegisterWorker", 'In get method of worker registration')
        registration_request = request.get_json()['registration_request']
        if registration_request is True:
            NUM_WORKERS += 1
            return {"worker_id": NUM_WORKERS}
        return {"worker_id": None}


# Add url handles for registration of worker, and general worker requests
api.add_resource(Manager, '/')
api.add_resource(RegisterWorker, '/register_worker')


# Requires the number of workers as arg[1]
if __name__ == '__main__':
    global commits_list, NUM_WORKERS
    # get repo set up first
    utils.get_git_repository()
    commits_list = utils.get_commits_as_list()

    app.run(Debug=True, host='127.0.0.1', port=5000)
    while NUM_WORKERS < sys.argv[1]:
        pass # wait for required number of workers to join

    # Now start timing
    start_time = utils.get_time()
    # insert work here
    end_time = utils.get_time()
    utils.output_results(NUM_WORKERS, start_time, end_time)