# This class will be used for generic Git methods
# These methods will probably need to be called by both Workers and Manager
# TODO implement
import time, os, sys

from git import Repo
from os import walk
from radon.cli import Config
from radon.complexity import cc_rank, SCORE

GITHUB_REPO_COMMITS_URL = "http://api.github.com/repos/amhiggin/DistributedFileSystem/commits"
GITHUB_REPO_URL = "https://bitbucket.org/Breandan96/cs4400distributedfileserver"


def print_to_console(node_name, message):
    print '{0}: {1}'.format(node_name, message)


def get_CCHarvester_config():
    # Obtained from https://github.com/rubik/radon/blob/master/radon/cli/__init__.py#L16
    return Config(
        min='A', max='F', show_complexity=True, average=False,
        exclude=None, ignore=None, order=SCORE, json=False, no_assert=False,
        show_closures=False, total_average=False, xml=False, codeclimate=False
    )


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
        commit_list.append(str(commit))
        print 'Adding commit {0}'.format(str(commit))
    return commit_list


def calculate_average(total_complexity, num_results_assessed):
    return (total_complexity / num_results_assessed)


def get_next_piece_of_work(commits_list, current_commit_index):
    print 'Fetching next segment of repository'
    print 'Commits list has {0} commits in it, we are going to get number {1}'.format(len(commits_list), str(current_commit_index))

    return commits_list[current_commit_index]


def get_outstanding_commits(commits_list, current_commit_index):
    print 'Checking whether there are any files left to process'
    if current_commit_index >= len(commits_list):
        print 'There are no commits remaining'
        return False
    else:
        print 'There are still commits remaining'
        return True


def get_files_at_commit(commit, repo_path):
    print 'In get_files_at_commit method. Getting commit {0}'.format(commit)

    # get the commit first
    repo = Repo(repo_path)
    git = repo.git
    git.checkout(commit)

    # then get the files for that commit
    return extract_python_files_for_commit(repo_path)


def extract_python_files_for_commit(repo_path):
    repo = Repo(repo_path)
    files = []
    for (dirpath, dirnames, filenames) in walk(repo_path):
        for filename in filenames:
            # filter on python files (these are the only ones we are interested in)
            if '.py' in filename and not '.pyc' in filename:
                files.append(dirpath + '/' + filename)
    return files


# used to get the start and end time of the execution
def get_time():
    return time.time()


def output_results(num_workers, total_time, complexity_results):
    print_to_console("Manager", "Outputting final results for complexity calculation")
    total_complexity = 0
    for result in complexity_results:
        total_complexity += result[0]
    average_complexity = calculate_average(total_complexity, len(complexity_results))

    result = 'Workers: {0} \tAverage Complexity: {1} \tTime: {2}'.format(num_workers, average_complexity, total_time)
    print result
    # output as appendage to file
    with open('complexity_results.txt', 'a') as output_file:
        output_file.write(result)
