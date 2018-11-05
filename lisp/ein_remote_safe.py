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


def __ein_export_nb(nb_json, format):
    import IPython.nbconvert as nbconvert
    import IPython.nbformat as nbformat
    nb = nbformat.reads(nb_json, nbformat.NO_CONVERT)
    output = nbconvert.export_by_name(format, nb)
    print(output[0])


def __ein_find_edit_target_012(*args, **kwds):
    from IPython.core.interactiveshell import InteractiveShell
    inst = InteractiveShell.instance()
    return inst._find_edit_target(*args, **kwds)


def __ein_find_edit_target_013(*args, **kwds):
    from IPython.core.interactiveshell import InteractiveShell
    inst = InteractiveShell.instance()
    return CodeMagics._find_edit_target(inst, *args, **kwds)


def __ein_find_edit_target_python(name):
    from inspect import getsourcefile, getsourcelines
    try:
        obj = eval(name)
    except NameError:
        return False
    else:
        sfile = getsourcefile(obj)
        sline = getsourcelines(obj)[-1]
        if sfile and sline:
            return(sfile, sline, False)
        else:
            return False

try:
    from IPython.core.magics import CodeMagics
    __ein_find_edit_target = __ein_find_edit_target_013
except ImportError:
    __ein_find_edit_target = __ein_find_edit_target_012

def __ein_set_figure_size(*dim):
    try:
        from matplotlib.pyplot import rcParams
        rcParams['figure.figsize'] = dim
    except:
        raise RuntimeError("Matplotlib not installed in this instance of python!")

def __ein_find_source(name):
    """Given an object as string, `name`, print its place in source code."""
    # FIXME: use JSON display object instead of stdout
    ret =  __ein_find_edit_target_python(name) or __ein_find_edit_target(name, {}, [])
    if ret:
        (filename, lineno, use_temp) = ret
        if not use_temp:
            print(filename)
            print(lineno)
            return
    raise RuntimeError("Source code for {0} cannot be found".format(name))


def __ein_run_docstring_examples(obj, verbose=True):
    from IPython.core.interactiveshell import InteractiveShell
    import doctest
    inst = InteractiveShell.instance()
    globs = inst.user_ns
    return doctest.run_docstring_examples(obj, globs, verbose=verbose)


def __ein_maybe_undefined_object(obj, locals=None):
    try:
        return eval(obj, None, locals)
    except Exception:
        return None
    except SyntaxError:
        return None

def __ein_print_object_info_for(obj):
    import IPython.core.oinspect
    import json

    inspector = IPython.core.oinspect.Inspector()

    try:
        print(json.dumps(inspector.info(obj)))
    except Exception:
        print(json.dumps(inspector.info(None)))

def __ein_eval_hy_string(obj):
    try:
        import hy
    except ImportError:
        print("Hy not supported in this kernel. Execute `pip install hy` if you want this support.")

    expr = hy.read_str(obj)
    ret = hy.eval(expr)

    return ret
