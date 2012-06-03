"""
Python utilities to use it from ein.el

Copyright (C) 2012- Takafumi Arakaki

Author: Takafumi Arakaki

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


def find_source(name):
    """Given an object as string, `name`, print its place in source code."""
    from IPython.core.interactiveshell import InteractiveShell
    inst = InteractiveShell.instance()
    (filename, lineno, use_temp) = inst._find_edit_target(name, {}, [])
    print filename
    print lineno
