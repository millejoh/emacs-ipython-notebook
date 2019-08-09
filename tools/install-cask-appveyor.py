#!/usr/bin/env python
# -*- coding: utf-8; -*-

# Copyright (C) 2012, 2013, 2014 Johan Andersson
# Copyright (C) 2013, 2014 Sebastian Wiesner

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

"""
Install Cask
"""

from __future__ import print_function, unicode_literals

import errno
import os
import sys
from subprocess import CalledProcessError, check_call

HOME = os.path.expanduser('~')
TARGET_DIRECTORY = os.path.join(HOME, '.cask')
REPOSITORY = 'https://github.com/millejoh/cask.git'
ISSUE_TRACKER = 'https://github.com/cask/cask/issues'


class CaskGoError(Exception):
    pass


OKGREEN = '\033[32m'
FAIL = '\033[31m'
ENDC = '\033[0m'


def success(s):
    print(OKGREEN + s + ENDC)
    sys.exit(0)


def fail(s):
    print(FAIL + s + ENDC, file=sys.stderr)
    sys.exit(1)


def bootstrap_cask(target_directory):
    cask = os.path.join(target_directory, 'bin', 'cask')
    try:
        check_call([sys.executable, cask, 'eval', '(progn (setq package-check-signature nil) (cask-cli/upgrade-cask))'])
    except CalledProcessError:
        raise CaskGoError('Cask could not be bootstrapped. Try again later, '
                          'or report an issue at {0}'.format(ISSUE_TRACKER))


def install_cask(target_directory):
    if os.path.isdir(target_directory):
        raise CaskGoError(
            'Directory {0} exists. Is Cask already installed?'.format(
                target_directory))
    else:
        try:
            check_call(['git', 'clone', '--branch', 'appveyor-build', REPOSITORY, target_directory])
        except CalledProcessError:
            raise CaskGoError('Cask could not be installed. Try again '
                              'later, or report an issue at {0}'.format(
                                  ISSUE_TRACKER))
        except OSError as error:
            if error.errno == errno.ENOENT:
                raise CaskGoError('git executable not found.  Please install Git')
            else:
                raise


def main():
    try:
        install_cask(TARGET_DIRECTORY)
        bootstrap_cask(TARGET_DIRECTORY)
        success("""\
Successfully installed Cask!  Now, add the cask binary to your $PATH:
  export PATH="{0}/bin:$PATH\"""".format(TARGET_DIRECTORY))
    except CaskGoError as error:
        fail('{0!s}'.format(error))



if __name__ == '__main__':
    sys.exit(main())
