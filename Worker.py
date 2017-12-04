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
        print 'In fetch_work method of worker'
        while(self.running):
            work = requests.get(FETCH_WORK_FROM_MANAGER_URL, json={"worker_id": worker})
            if work.json()['running'] is False:
                break
            if work is not None:
                file_names = work.json()["file_names"]
                self.do_work(file_names)
        print 'The manager instructed us to terminate'

    def do_work(self, file_names):
        print 'In do_work method of worker'
        total_complexity = 0
        num_files_assessed = 0
        for file_name in file_names:
            total_complexity += self.calculate_complexity(file_name)
            num_files_assessed += 1
        average_complexity = utils.calculate_average(total_complexity, num_files_assessed)

        return {'file_names': file_names, "average_complexity": average_complexity}


    def calculate_complexity(self, file_name):
        complexity = 0
        file = open(file_name, 'r')
        results = CCHarvester(file_name).gobble(file)

        for result in results:
            print (result.complexity)
            complexity += int(result.complexity)

        average_complexity = utils.calculate_average(complexity, len(results))
        print "Total complexity of {0}: {1}".format(file_name, str(complexity))
        print "Average complexity of {0}: {1}".format(file_name, str(average_complexity))
        return average_complexity

def register_worker(Worker):
    response = requests.get(WORKER_REGISTRATION_URL, json={'registration_request': True})
    worker_id = response.json()['worker_id']
    print 'Response to request for new worker registration: {0}'.format(worker_id)
    return worker_id


if __name__ == '__main__':
    global WORKER_ID
    worker = Worker()
    WORKER_ID = register_worker(worker)
    utils.clone_repository()
    worker.fetch_work()