# This class will be used for generic Git methods
# These methods will probably need to be called by both Workers and Manager
# TODO implement
import time, os, sys
from pygit2 import clone_repository, Repository, GitError

# to be used later for cloning etc
GITHUB_REPO_COMMITS_URL = "http://api.github.com/repos/rubik/argon/commits"
GITHUB_REPO_URL = "http:/www.github.com/rubik/argon"
REPO_PATH = "/argon_clone/"
GIT_REPO_PATH = REPO_PATH + ".git"


def print_to_console(node_name, message):
    print '{0}: {1}'.format(node_name, message)

def clone_repository():
    print 'In clone repository method: required to clone {0}'.format(GITHUB_REPO_URL)
    print("Checking if the git repo exists...")
    try:
        repo = Repository(GIT_REPO_PATH)
    except GitError as e:
        repo = clone_repository(GITHUB_REPO_URL, REPO_PATH)
    print("Finished!")

def get_commits_as_list():
    repo = Repository(GIT_REPO_PATH)
    commit_list = {}
    for commit in repo.walk(repo.head.target):
        commit_list.append(str(commit.id))
    return commit_list

def calculate_average(total_complexity, num_results_assessed):
    return (total_complexity / num_results_assessed)

def change_commit(url, commit_version):
    print 'In change_commit method: required to change to commit version {0}'.format(commit_version)
    # TODO implement

def get_next_segment_of_repository():
    print 'Fetching next segment of repository'
    # TODO implement

def get_are_files_remaining():
    print 'Checking whether there are any files left to process'
    # TODO implement

# used to get the start and end time of the execution
def get_time():
    return time.time()


def output_results(num_workers, start_time, end_time):
    time_taken = end_time - start_time
    print 'Workers: {0} \t Time: {1}'.format(num_workers, time_taken)
