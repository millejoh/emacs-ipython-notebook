"""
Python utilities for the ein inspector.

Copyright (C) 2017- John M. Miller

Author: John Miller <millejoh at gmail.com>

ein_inspector.py is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

ein_inspector.py is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ein_inspector.py.  If not, see <http://www.gnu.org/licenses/>.

"""

import json
import inspect


def generate_inspector_data(obj_str, globals, locals):
    odata = {'name': obj_str}
    try:
        obj = eval(obj_str, globals, locals)
    except NameError:
        odata['error'] = 'Object {} not found.'.format(obj_str)
    else:
        odata['doc'] = inspect.getdoc(obj)
        odata['type'] = str(type(obj))
        odata['repr'] = str(obj)
        try:
            odata['source_file'] = inspect.getsourcefile(obj)
            odata['source_lines'] = inspect.getsourcelines(obj)
        except:
            odata['source_file'] = None
            odata['source_lines'] = None
    print(json.dumps(odata))
    return odata


