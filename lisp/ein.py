"""
Python utilities to use it from ein.el

Copyright (C) 2012- Takafumi Arakaki

Author: Takafumi Arakaki <aka.tkf at gmail.com>

ein.py is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

ein.py is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ein.py.  If not, see <http://www.gnu.org/licenses/>.

"""


def _find_edit_target_012(*args, **kwds):
    from IPython.core.interactiveshell import InteractiveShell
    inst = InteractiveShell.instance()
    return inst._find_edit_target(*args, **kwds)


def _find_edit_target_013(*args, **kwds):
    from IPython.core.interactiveshell import InteractiveShell
    inst = InteractiveShell.instance()
    return CodeMagics._find_edit_target(inst, *args, **kwds)

try:
    from IPython.core.magics import CodeMagics
    _find_edit_target = _find_edit_target_013
except ImportError:
    _find_edit_target = _find_edit_target_012


def find_source(name):
    """Given an object as string, `name`, print its place in source code."""
    # FIXME: use JSON display object instead of stdout
    ret = _find_edit_target(name, {}, [])
    if ret:
        (filename, lineno, use_temp) = ret
        if not use_temp:
            print filename
            print lineno
            return
    raise RuntimeError("Source code for {0} cannot be found".format(name))


def run_docstring_examples(obj, verbose=True):
    from IPython.core.interactiveshell import InteractiveShell
    import doctest
    inst = InteractiveShell.instance()
    globs = inst.user_ns
    return doctest.run_docstring_examples(obj, globs, verbose=verbose)
