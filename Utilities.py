# This class will be used for generic Git methods
# These methods will probably need to be called by both Workers and Manager
# TODO implement
import time, os, sys

from git import Repo
from os import walk

GITHUB_REPO_COMMITS_URL = "http://api.github.com/repos/amhiggin/DistributedFileSystem/commits"
GITHUB_REPO_URL = "https://github.com/amhiggin/DistributedFileSystem/"


def print_to_console(node_name, message):
    print '{0}: {1}'.format(node_name, message)

def get_git_repository(repo_path):
    print 'In clone repository method: required to clone {0}'.format(GITHUB_REPO_URL)
    if not os.path.exists(repo_path):
        os.makedirs(repo_path)
    if not os.listdir(repo_path):
        repo = Repo.clone_from(GITHUB_REPO_URL, repo_path)
    else:
        repo = Repo(repo_path)
    return repo

def get_commits_as_list(repo_path):
    repo = Repo(repo_path)
    commit_list = []
    for commit in repo.iter_commits():
        commit_list.append(commit)
        print 'Adding commit {0}'.format(str(commit))
    return commit_list

def calculate_average(total_complexity, num_results_assessed):
    return (total_complexity / num_results_assessed)

def get_next_piece_of_work(commits_list, current_commit_index):
    print 'Fetching next segment of repository'
    return commits_list[current_commit_index]

def get_are_files_remaining(commits_list, current_commit_index):
    print 'Checking whether there are any files left to process'
    if current_commit_index >= len(commits_list):
        return False
    else:
        return True

def get_files_at_commit(commit, repo_path):
    print 'In get_files_at_commit method. Getting commit number {0}'.format(commit)

    # get the commit first
    repo = Repo(repo_path)
    repo.checkout(commit)

    # then get the files for that commit
    return extract_python_files_for_commit(repo_path)


def extract_python_files_for_commit(repo_path):
    repo = Repo(repo_path)
    files = []
    for (dirpath, dirnames, filenames) in walk(repo_path):
        for filename in filenames:
            # filter on python files (these are the only ones we are interested in)
            if '.py' in filename:
                files.append(dirpath + '/' + filename)
    return files


# used to get the start and end time of the execution
def get_time():
    return time.time()


def output_results(num_workers, start_time, end_time):
    time_taken = end_time - start_time
    result = 'Workers: {0} \t Time: {1}'.format(num_workers, time_taken)
    print result
    # output as appendage to file
    with open('complexity_results.txt', 'a') as output_file:
        output_file.write(result)
