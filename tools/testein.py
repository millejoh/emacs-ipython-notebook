#!/usr/bin/env python

"""
Run EIN test suite
"""

import sys
import os
import glob
from subprocess import Popen, PIPE, STDOUT
import itertools

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


def show_nonprinting(string, stream=sys.stdout):
    """Emulate ``cat -v`` (``--show-nonprinting``)."""
    stream.writelines(itertools.imap(chr, convert_nonprinting(string)))


def convert_nonprinting(string):
    """
    Convert non-printing characters in `string`.

    Output is iterable of int.  So for Python 2, you need to
    convert it into string using `chr`.

    Adapted from: http://stackoverflow.com/a/437542/727827

    """

    for b in itertools.imap(ord, string):
        assert 0 <= b < 0x100

        if b in (0x09, 0x0a):   # '\t\n'
            yield b
            continue

        if b > 0x7f:            # not ascii
            yield 0x4d          # 'M'
            yield 0x2d          # '-'
            b &= 0x7f

        if b < 0x20:            # control char
            yield 0x5e          # '^'
            b |= 0x40
        elif b == 0x7f:
            yield 0x5e          # '^'
            yield 0x3f          # '?'
            continue

        yield b


class BaseRunner(object):

    def __init__(self, **kwds):
        self.__dict__.update(kwds)
        self.batch = self.batch and not self.debug_on_error

    def logpath(self, name, ext='log'):
        return os.path.join(
            self.log_dir,
            "{testname}_{logname}_{modename}_{emacsname}.{ext}".format(
                ext=ext,
                logname=name,
                emacsname=os.path.basename(self.emacs),
                testname=os.path.splitext(self.testfile)[0],
                modename='batch' if self.batch else 'interactive',
            ))

    @property
    def command(self):
        raise NotImplementedError

    def do_run(self):
        raise NotImplementedError

    def run(self):
        if self.dry_run:
            command = self.command
            if isinstance(command, basestring):
                print command
            else:
                print construct_command(command)
            return 0
        else:
            mkdirp(self.log_dir)
            return self.do_run()


class TestRunner(BaseRunner):

    def __init__(self, **kwds):
        super(TestRunner, self).__init__(**kwds)

        fmtdata = self.__dict__.copy()
        fmtdata.update(
            emacsname=os.path.basename(self.emacs),
            testname=os.path.splitext(self.testfile)[0],
            modename='batch' if self.batch else 'interactive',
        )
        quote = '"{0}"'.format
        self.logpath_log = self.logpath('log')
        self.logpath_messages = self.logpath('messages')
        self.lispvars = {
            'ein:testing-dump-file-log': quote(self.logpath_log),
            'ein:testing-dump-file-messages': quote(self.logpath_messages),
            'ein:log-level': self.ein_log_level,
            'ein:log-message-level': self.ein_message_level,
        }
        if self.ein_debug:
            self.lispvars['ein:debug'] = "'t"

    def setq(self, sym, val):
        self.lispvars[sym] = val

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
                        '-L', einlibdir('request'),
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

    def show_sys_info(self):
        print "*" * 50
        command = self.base_command + [
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
        (stdout, _) = self.proc.communicate()
        self.stdout = stdout
        self.failed = self.proc.returncode != 0
        if self.failed:
            print "*" * 50
            print "Showing {0}:".format(self.logpath_log)
            print open(self.logpath_log).read()
            print
            print "*" * 50
            print "Showing STDOUT/STDERR:"
            show_nonprinting(stdout)
            print
            print "{0} failed".format(self.testfile)
        else:
            print "{0} OK".format(self.testfile)
            for line in reversed(stdout.splitlines()):
                if line.startswith('Ran'):
                    print line
                    break
        return int(self.failed)

    def do_run(self):
        self.show_sys_info()
        self.make_process()
        return self.report()

    def is_known_failure(self):
        """
        Check if failures are known, based on STDOUT from ERT.
        """
        import re
        lines = iter(self.stdout.splitlines())
        for l in lines:
            if re.match("[0-9]+ unexpected results:.*", l):
                break
        else:
            return True  # no failure

        # Check "FAILED <test-name>" lines
        for l in lines:
            if not l:
                break  # end with an empty line
            for f in self.known_failures:
                if re.search(f, l):
                    break
            else:
                return False
        return True

    known_failures = [
        "ein:notebook-execute-current-cell-pyout-image$",
    ]
    """
    A list of regexp which matches to test that is known to fail (sometimes).
    This is a workaround for ##74.
    """


def mkdirp(path):
    """Do ``mkdir -p {path}``"""
    if not os.path.isdir(path):
        os.makedirs(path)


def remove_elc():
    files = glob.glob(einlispdir("*.elc")) + glob.glob(eintestdir("*.elc"))
    map(os.remove, files)
    print "Removed {0} elc files".format(len(files))


class ServerRunner(BaseRunner):

    port = None
    notebook_dir = os.path.join(EIN_ROOT, "tests", "notebook")

    def __enter__(self):
        self.run()
        return self.port

    def __exit__(self, type, value, traceback):
        self.stop()

    def do_run(self):
        self.clear_notebook_dir()
        self.start()
        self.get_port()
        print "Server running at", self.port

    def clear_notebook_dir(self):
        files = glob.glob(os.path.join(self.notebook_dir, '*.ipynb'))
        map(os.remove, files)
        print "Removed {0} ipynb files".format(len(files))

    @staticmethod
    def _parse_port_line(line):
        return line.strip().rsplit(':', 1)[-1].strip('/')

    def get_port(self):
        if self.port is None:
            self.port = self._parse_port_line(self.proc.stdout.readline())
        return self.port

    def start(self):
        from subprocess import Popen, PIPE, STDOUT
        self.proc = Popen(
            self.command, stdout=PIPE, stderr=STDOUT, stdin=PIPE,
            shell=True)
        # Answer "y" to the prompt: Shutdown Notebook Server (y/[n])?
        self.proc.stdin.write('y\n')

    def stop(self):
        print "Stopping server", self.port
        returncode = self.proc.poll()
        if returncode is not None:
            logpath = self.logpath('server')
            print "Server process was already dead by exit code", returncode
            print "*" * 50
            print "Showing {0}:".format(logpath)
            print open(logpath).read()
            print
            return
        if not self.dry_run:
            try:
                kill_subprocesses(self.proc.pid, lambda x: 'ipython' in x)
            finally:
                self.proc.terminate()

    @property
    def command(self):
        fmtdata = dict(
            notebook_dir=self.notebook_dir,
            ipython=self.ipython,
            server_log=self.logpath('server'),
        )
        return self.command_template.format(**fmtdata)

    command_template = r"""
{ipython} notebook \
    --notebook-dir {notebook_dir} \
    --debug \
    --no-browser 2>&1 \
    | tee {server_log} \
    | grep --line-buffered 'The IPython Notebook is running at' \
    | head -n1
"""


def kill_subprocesses(pid, include=lambda x: True):
    from subprocess import Popen, PIPE
    import signal

    command = ['ps', '-e', '-o', 'ppid,pid,command']
    proc = Popen(command, stdout=PIPE, stderr=PIPE)
    (stdout, stderr) = proc.communicate()
    if proc.returncode != 0:
        raise RuntimeError(
            'Command {0} failed with code {1} and following error message:\n'
            '{2}'.format(command, proc.returncode, stderr))

    for line in map(str.strip, stdout.splitlines()):
        (cmd_ppid, cmd_pid, cmd) = line.split(None, 2)
        if cmd_ppid == str(pid) and include(cmd):
            print "Killing PID={0} COMMAND={1}".format(cmd_pid, cmd)
            os.kill(int(cmd_pid), signal.SIGINT)


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


def run_ein_test(unit_test, func_test, func_test_max_retries,
                 no_skip, clean_elc, **kwds):
    if clean_elc and not kwds['dry_run']:
        remove_elc()
    if unit_test:
        unit_test_runner = TestRunner(testfile='test-load.el', **kwds)
        if unit_test_runner.run() != 0:
            return 1
    if func_test:
        for i in range(func_test_max_retries + 1):
            func_test_runner = TestRunner(testfile='func-test.el', **kwds)
            with ServerRunner(testfile='func-test.el', **kwds) as port:
                func_test_runner.setq('ein:testing-port', port)
                if func_test_runner.run() == 0:
                    print "Functional test succeeded after {0} retries." \
                        .format(i)
                    return 0
                if not no_skip and func_test_runner.is_known_failure():
                    print "All failures are known.  Ending functional test."
                    return 0
        print "Functional test failed after {0} retries.".format(i)
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
    parser.add_argument('--func-test-max-retries', default=4, type=int,
                        help="""
                        Specify number of retries for functional test
                        before failing with error.  This is workaround
                        for the issue #74.
                        """)
    parser.add_argument('--no-skip', default=False, action='store_true',
                        help="""
                        Do no skip known failures.  Known failures
                        are implemented as another workaround for the
                        issue #74.
                        """)
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
    parser.add_argument('--ipython', default='ipython',
                        help="""
                        ipython executable to use to run notebook server.
                        """)
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
