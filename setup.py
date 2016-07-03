from distutils.core import setup
from distutils.cmd import Command

setup(
    name='emacs-ipython-notebook',
    version='0.8.2',
    packages=[],
    url='https://github.com/millejoh/emacs-ipython-notebook',
    license='',
    author='millejoh',
    author_email='millejoh@mac.com',
    description='Emacs IPython Notebook'
)


class TestIPython(Command):
    user_options =

    def initialize_options(self):
        pass

    def finalize_options(self):
        pass

    def run(self):
        pass

class TestJupyter(Command):
    pass


