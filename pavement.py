from paver.easy import *
import paver.doctools
from paver.setuputils import setup
import os, os.path

def read(fname):
    return open(os.path.join(os.path.dirname(__file__), fname)).read()


# "master --squash",
subtrees = [["lib/websocket", "https://github.com/ahyatt/emacs-websocket.git"],
            ["lib/auto-complete", "https://github.com/auto-complete/auto-complete.git"],
            ["lib/fuzzy", "https://github.com/auto-complete/fuzzy-el.git"],
            ["lib/popup", "https://github.com/auto-complete/popup-el.git"],
            ["lib/pos-tip", "https://github.com/emacsmirror/pos-tip.git"],
            ["lib/smartrep", "https://github.com/myuhe/smartrep.el.git"],
            ["lib/markdown-mode", "https://github.com/defunkt/markdown-mode.git"],
            ["lib/ert", "https://github.com/ohler/ert.git"],
            ["lib/request", "https://github.com/tkf/emacs-request"],
            ["lib/ein-mumamo", "https://github.com/millejoh/ein-mumamo"]]

@task
def update_libraries():
    for subdir, pkg in subtrees:
        if os.path.exists(os.path.join(os.getcwd(), subdir)):
            os.system('git subtree pull --prefix {dir} {pkg} master --squash'.format(dir=subdir, pkg=pkg))
        else:
            os.system('git subtree add --prefix {dir} {pkg} master --squash'.format(dir=subdir, pkg=pkg))

@task
def run_testein(self):
    os.system('python tools/testein.py --clean-elc --e {EMACS} --ipython {IPYTHON}'.format(emacs=self.emacs,
                                                                                               ipython=self.ipython))

setup(
    name='emacs-ipython-notebook',
    version='0.10.0',
    packages=['tests.notebook.nbformat3.nbpackage', 'tests.notebook.nbformat3.nbpackage.nbs'],
    url='https://github.com/millejoh/emacs-ipython-notebook',
    license='GPL v3',
    author='John M. Miller',
    author_email='millejoh@mac.com',
    description='Jupyter Notebook Client written in Emacs.',
    long_description=read('README.rst'),
)







