'''
Worker node, which calculates the cyclomatic complexity of a set of files given to it.
Intention: that the worker will follow the work-stealing pattern, i..e requesting work from the manager.
'''

import os, sys, requests
from flask_restful import Resource, Api, reqparse
from flask import Flask, request
import Utilities as utils
from radon.cli.harvest import CCHarvester

FETCH_WORK_FROM_MANAGER_URL = "http://127.0.0.1:5000"
WORKER_REGISTRATION_URL = "http://127.0.0.1:5000/register_worker"
WORKER_ID = ""

class Worker(object):

    def __init__(self):
        # do any required initialisation
        self.running = True

    # fetches work from the manager
    def fetch_work(self):
        utils.print_to_console('Worker' + WORKER_ID, 'In fetch_work method')
        while(self.running):
            work = requests.get(FETCH_WORK_FROM_MANAGER_URL, json={"worker_id": worker})
            if work.json()['running'] is False:
                self.running = False
            if work is not None:
                file_names = work.json()["file_names"]
                result = self.do_work(file_names)
                response = requests.post(FETCH_WORK_FROM_MANAGER_URL, result)
            utils.print_to_console("Worker" + WORKER_ID, 'The manager instructed us to terminate')

    def do_work(self, file_names):
        utils.print_to_console('Worker' + WORKER_ID, 'In do_work method')
        total_complexity = 0
        num_files_assessed = 0
        for file_name in file_names:
            total_complexity += self.calculate_complexity(file_name)
            num_files_assessed += 1
        average_complexity = utils.calculate_average(total_complexity, num_files_assessed)

        return {'file_names': file_names, "average_complexity": average_complexity}


    def calculate_complexity(self, file_name):
        utils.print_to_console('Worker' + WORKER_ID, 'Calculating complexity for file {0}'.format(file_name))
        file_complexity = 0
        file = open(file_name, 'r')
        results = CCHarvester(file_name).gobble(file)

        for result in results:
            print (result.complexity)
            file_complexity += int(result.complexity)

        average_complexity = utils.calculate_average(file_complexity, len(results))
        utils.print_to_console('Worker' + WORKER_ID, "Total complexity of {0}: {1}".format(file_name, str(file_complexity)))
        utils.print_to_console('Worker' + WORKER_ID, "Average complexity of {0}: {1}".format(file_name, str(average_complexity)))
        return average_complexity


def register_worker():
    response = requests.get(WORKER_REGISTRATION_URL, json={'registration_request': True})
    worker_id = response.json()['worker_id']
    utils.print_to_console('Worker' + WORKER_ID, 'Response to request for new worker registration: {0}'.format(worker_id))
    return worker_id


if __name__ == '__main__':
    global WORKER_ID
    worker = Worker()
    WORKER_ID = register_worker(worker)
    worker.fetch_work()