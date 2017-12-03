# This class will be used for generic Git methods
# These methods will probably need to be called by both Workers and Manager
# TODO implement

# to be used later for cloning etc
GITHUB_REPO_COMMITS_URL = "https://api.github.com/repos/amhiggin/ChatroomServer/commits"


def clone_repository(url):
    print 'In clone repository method: required to clone {0}'.format(url)


def change_commit(url, commit_version):
    print 'In change_commit method: required to change to commit version {0}'.format(commit_version)