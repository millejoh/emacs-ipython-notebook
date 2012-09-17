#!/usr/bin/env python

"""
Run EIN test suite
"""

import os
import glob
from subprocess import Popen, PIPE, STDOUT

EIN_ROOT = os.path.normpath(
    os.path.join(os.path.dirname(__file__), os.path.pardir))


def has_library(emacs, library):
    """
    Return True when `emacs` has build-in `library`.
    """
    with open(os.devnull, 'w') as devnull:
        proc = Popen(
            [emacs, '-Q', '-batch', '-l', 'cl',
             '--eval', '(assert (locate-library "{0}"))'.format(library)],
            stdout=devnull, stderr=devnull)
        return proc.wait() == 0


def eindir(*path):
    return os.path.join(EIN_ROOT, *path)


def einlispdir(*path):
    return eindir('lisp', *path)


def eintestdir(*path):
    return eindir('tests', *path)


def einlibdir(*path):
    return eindir('lib', *path)


class TestRunner(object):

    def __init__(self, **kwds):
        self.__dict__.update(kwds)
        self.batch = self.batch and not self.debug_on_error

        fmtdata = self.__dict__.copy()
        fmtdata.update(
            emacsname=os.path.basename(self.emacs),
            testname=os.path.splitext(self.testfile)[0],
            modename='batch' if self.batch else 'interactive',
        )
        logpath = lambda x: '"{0}"'.format(os.path.join(self.log_dir, x))
        logtemp = logpath("{testname}_log_{modename}_{emacsname}.log")
        msgtemp = logpath("{testname}_messages_{modename}_{emacsname}.log")
        self.lispvars = {
            'ein:testing-dump-file-log': logtemp.format(**fmtdata),
            'ein:testing-dump-file-messages': msgtemp.format(**fmtdata),
            'ein:log-level': self.ein_log_level,
            'ein:log-message-level': self.ein_message_level,
        }
        if self.ein_debug:
            self.lispvars['ein:debug'] = "'t"

    def bind_lispvars(self):
        command = []
        for (k, v) in self.lispvars.iteritems():
            if v is not None:
                command.extend([
                    '--eval', '(setq {0} {1})'.format(k, v)])
        return command

    @property
    def base_command(self):
        command = [self.emacs, '-Q'] + self.bind_lispvars()

        if self.batch:
            command.append('-batch')
        if self.debug_on_error:
            command.extend(['-f', 'toggle-debug-on-error'])

        # load modules
        if self.need_ert():
            ertdir = einlibdir('ert', 'lisp', 'emacs-lisp')
            command.extend([
                '-L', ertdir,
                # Load `ert-run-tests-batch-and-exit`:
                '-l', os.path.join(ertdir, 'ert-batch.el'),
                # Load `ert-run-tests-interactively`:
                '-l', os.path.join(ertdir, 'ert-ui.el'),
             ])
        for path in self.load_path:
            command.extend(['-L', path])
        for path in self.load:
            command.extend(['-l', path])
        command.extend(['-L', einlispdir(),
                        '-L', einlibdir('websocket'),
                        '-L', einlibdir('auto-complete'),
                        '-L', einlibdir('popup'),
                        '-L', eintestdir(),
                        '-l', eintestdir(self.testfile)])
        return command

    @property
    def command(self):
        command = self.base_command[:]
        if self.batch:
            command.extend(['-f', 'ert-run-tests-batch-and-exit'])
        else:
            command.extend(['--eval', "(ert 't)"])
        return command

    def show_sys_info(self, base_command):
        print "*" * 50
        command = base_command + [
            '-batch', '-l', 'ein-dev', '-f', 'ein:dev-print-sys-info']
        proc = Popen(command, stderr=PIPE)
        err = proc.stderr.read()
        proc.wait()
        if proc.returncode != 0:
            print "Error with return code {0} while running {1}".format(
                proc.returncode, command)
            print err
            pass
        print "*" * 50

    def need_ert(self):
        if self.load_ert:
            return True
        if self.auto_ert:
            if has_library(self.emacs, 'ert'):
                print "{0} has ERT module.".format(self.emacs)
                return False
            else:
                print "{0} has no ERT module.".format(self.emacs),
                print "ERT is going to be loaded from git submodule."
                return True
        return False

    def make_process(self):
        print "Start test {0}".format(self.testfile)
        self.proc = Popen(self.command, stdout=PIPE, stderr=STDOUT)
        return self.proc

    def report(self):
        if self.proc.wait() != 0:
            print "{0} failed".format(self.testfile)
            print self.proc.stdout.read()
            self.failed = True
        else:
            print "{0} OK".format(self.testfile)
            self.failed = False
        return int(self.failed)

    def run(self):
        mkdirp(self.log_dir)
        self.show_sys_info(self.base_command)
        self.make_process()
        return self.report()


def mkdirp(path):
    """Do ``mkdir -p {path}``"""
    if not os.path.isdir(path):
        os.makedirs(path)


def remove_elc():
    files = glob.glob(einlispdir("*.elc")) + glob.glob(eintestdir("*.elc"))
    map(os.remove, files)
    print "Removed {0} elc files".format(len(files))


def construct_command(args):
    """
    Construct command as a string given a list of arguments.
    """
    command = []
    escapes = set(' ()')
    for a in args:
        if set(a) & escapes:
            command.append(repr(str(a)))  # hackish way to escape
        else:
            command.append(a)
    return " ".join(command)


def run_ein_test(unit_test, func_test, clean_elc, dry_run, **kwds):
    if clean_elc and not dry_run:
        remove_elc()
    if unit_test:
        unit_test_runner = TestRunner(testfile='test-load.el', **kwds)
        if dry_run:
            print construct_command(unit_test_runner.command())
        elif unit_test_runner.run() != 0:
            return 1
    if func_test:
        func_test_runner = TestRunner(testfile='func-test.el', **kwds)
        if dry_run:
            print construct_command(func_test_runner.command())
        elif func_test_runner.run() != 0:
            return 1
    return 0


def main():
    import sys
    from argparse import ArgumentParser
    parser = ArgumentParser(description=__doc__.splitlines()[1])
    parser.add_argument('--emacs', '-e', default='emacs',
                        help='Emacs executable.')
    parser.add_argument('--load-path', '-L', default=[], action='append',
                        help="add a directory to load-path. "
                        "can be specified multiple times.")
    parser.add_argument('--load', '-l', default=[], action='append',
                        help="load lisp file before tests. "
                        "can be specified multiple times.")
    parser.add_argument('--load-ert', default=False, action='store_true',
                        help="load ERT from git submodule. "
                        "you need to update git submodule manually "
                        "if ert/ directory does not exist yet.")
    parser.add_argument('--no-auto-ert', default=True,
                        dest='auto_ert', action='store_false',
                        help="load ERT from git submodule. "
                        "if this Emacs has no build-in ERT module.")
    parser.add_argument('--no-batch', '-B', default=True,
                        dest='batch', action='store_false',
                        help="start interactive session.")
    parser.add_argument('--debug-on-error', '-d', default=False,
                        action='store_true',
                        help="set debug-on-error to t and start "
                        "interactive session.")
    parser.add_argument('--no-func-test', '-F', default=True,
                        dest='func_test', action='store_false',
                        help="do not run functional test.")
    parser.add_argument('--no-unit-test', '-U', default=True,
                        dest='unit_test', action='store_false',
                        help="do not run unit test.")
    parser.add_argument('--clean-elc', '-c', default=False,
                        action='store_true',
                        help="remove *.elc files in ein/lisp and "
                        "ein/tests directories.")
    parser.add_argument('--dry-run', default=False,
                        action='store_true',
                        help="Print commands to be executed.")
    parser.add_argument('--ein-log-level', default=40)
    parser.add_argument('--ein-message-level', default=30)
    parser.add_argument('--ein-debug', default=False, action='store_true',
                        help="(setq ein:debug t) when given.")
    parser.add_argument('--log-dir', default="log",
                        help="Directory to store log (default: %(default)s)")
    args = parser.parse_args()
    sys.exit(run_ein_test(**vars(args)))


if __name__ == '__main__':
    main()
