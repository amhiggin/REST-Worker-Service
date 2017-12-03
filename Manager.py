import threading, os, sys
import flask
import flask_restful


app = Flask(__name__)
api = Api(app)


class Manager(Resource):

    def get(self):
        print 'In get method of manager'


    def post(self):
        print 'In post method of manager'




class RegisterWorker(Resource):

    def get(self):
        print 'In get method of worker registration'

# Add url handles for registration of worker, and general worker requests
api.add_resource(Manager, '/')
api.add_resource(RegisterWorker, '/register_worker')

if __name__ == '__main__':
    app.run(Debug=True, host='127.0.0.1', port=5000)