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
        odata['type'], odata['repr'] = determine_object_type(obj)
        odata['source_file'] = inspect.getsourcefile(obj)
        odata['source_lines'] = inspect.getsourcelines(obj)

    print(json.dumps(odata))
    return odata

def determine_object_type(obj):
    if inspect.ismodule(obj):
        return 'Module', obj.__str__()
    elif inspect.isclass(obj):
        return 'Class', obj.__str__()
    elif inspect.ismethod(obj):
        return 'Method', obj.__str__()
    elif inspect.isfunction(obj):
        return 'Function', obj.__str__()
    elif inspect.isgeneratorfunction(obj):
        return 'Generator Function', obj.__str__()
    elif inspect.isgenerator(obj):
        return 'Generator', obj.__str__()
    elif inspect.iscoroutinefunction(obj):
        return 'Coroutine Function', obj.__str__()
    elif inspect.iscoroutine(obj):
        return 'Coroutine', obj.__str__()
    elif inspect.isawaitable(obj):
        return 'Awaitable', obj.__str__()
    elif inspect.istraceback(obj):
        return 'Traceback', obj.__str__()
    elif inspect.isframe(obj):
        return 'Frame', obj.__str__()
    elif inspect.iscode(obj):
        return 'Code', obj.__str__()
    elif inspect.isbuiltin(obj):
        return 'Builtin', obj.__str__()
    elif inspect.isroutine(obj):
        return 'Routine', obj.__str__()
    elif inspect.isabstract(obj):
        return 'Abstract Base Class', obj.__str__()
    elif inspect.ismethoddescriptor(obj):
        return 'Method Descriptor', obj.__str__()
    elif inspect.isdatadescriptor(obj):
        return 'Data Descriptor', obj.__str__()
    elif inspect.isgetsetdescriptor(obj):
        return 'Getset Descriptor', obj.__str__()
    elif inspect.ismemberdescriptor(obj):
        return 'Member Descriptor', obj.__str__()
    elif inspect.isbuiltin(obj):
        return type(obj), obj.__str__()
    else:
        return type(obj), obj.__str__()
