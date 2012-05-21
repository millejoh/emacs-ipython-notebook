"""
Emacs Lisp Display object/Formatter for IPython.

Usage::

   In [1]:
   %run path/to/emacslisp.py

   In [2]:
   EmacsLisp('(+ 1 2 3)')
   Out [2]:
   6

"""

from IPython.core.formatters import BaseFormatter, Unicode, ObjectName
from IPython.core.display import DisplayObject


def add_display_formatter(new_formatter):
    from IPython.core.formatters import FormatterABC
    FormatterABC.register(new_formatter)
    from IPython.core.interactiveshell import InteractiveShell
    inst = InteractiveShell.instance()
    f = new_formatter(config=inst.display_formatter.config)
    inst.display_formatter.formatters[f.format_type] = f


class EmacsLispFormatter(BaseFormatter):
    format_type = Unicode('application/emacs-lisp')
    print_method = ObjectName('_repr_emacs_lisp_')


class EmacsLisp(DisplayObject):

    def _repr_emacs_lisp_(self):
        return self.data

if __name__ == '__main__':
    add_display_formatter(EmacsLispFormatter)
