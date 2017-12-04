import threading, os, sys
from flask_restful import Resource, Api, reqparse
from flask import Flask, request
import Utilities as utils

NUM_WORKERS = 0


app = Flask(__name__)
api = Api(app)



class Manager(Resource):

    def get(self):
        print 'In get method of manager'
        file_names = utils.get_next_segment_of_repository()
        running = utils.get_are_files_remaining()
        return {"file_names": file_names, "running": running}


    def post(self):
        print 'In post method of manager'




class RegisterWorker(Resource):

    def get(self):
        global NUM_WORKERS
        print 'In get method of worker registration'
        registration_request = request.get_json()['registration_request']
        if registration_request is True:
            NUM_WORKERS += 1
            return {"worker_id": NUM_WORKERS}


# Add url handles for registration of worker, and general worker requests
api.add_resource(Manager, '/')
api.add_resource(RegisterWorker, '/register_worker')

if __name__ == '__main__':
    app.run(Debug=True, host='127.0.0.1', port=5000)
    start_time = utils.get_time()
    # do work
    end_time = utils.get_time()
    utils.get_results(NUM_WORKERS, start_time, end_time)