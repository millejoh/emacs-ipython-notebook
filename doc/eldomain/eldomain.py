
# eldomain is a Emacs Lisp domain for the Sphinx documentation tool.
# Copyright (C) 2012 Takafumi Arakaki

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""
The Emacs Lisp domain
~~~~~~~~~~~~~~~~~~~~~

This Sphinx extension provides directives and roles under the domain
``el`` for documenting Emacs Lisp program.  It also provides autodoc
functionality -- docstrings in the Emacs Lisp is automatically
inserted in the documentation.

.. note::
   The source code is heavily borrowed from the `Common Lisp domain`_.

.. _`Common Lisp domain`:
   https://github.com/russell/sphinxcontrib-cldomain

"""

import os
from os import path
import re
import itertools
import subprocess
import json

from docutils import nodes
from docutils.statemachine import string2lines, StringList

from sphinx import addnodes
from sphinx.locale import l_, _
from sphinx.roles import XRefRole
from sphinx.domains import Domain, ObjType
from sphinx.directives import ObjectDescription
from sphinx.util.nodes import make_refnode
from sphinx.util.compat import Directive
from sphinx.util.docfields import Field, GroupedField

DATA = {}
DATA_DOC_STRINGS = {}
DATA_ARGS = {}


def bool_option(arg):
    """Used to convert flag options to auto directives.  (Instead of
    directives.flag(), which returns None).
    """
    return True


def string_list(delimiter):
    """
    Return a parser for option_spec to parse strings separated by `delimiter`.

    >>> parser = string_list(',')
    >>> parser('a, b, c')
    ['a', 'b', 'c']

    """
    return lambda argument: [v.strip() for v in argument.split(delimiter)]


class ELSExp(ObjectDescription):

    doc_field_types = [
        GroupedField('parameter', label=l_('Parameters'),
                     names=('param', 'parameter', 'arg', 'argument',
                            'keyword', 'kwparam')),
        Field('returnvalue', label=l_('Returns'), has_arg=False,
              names=('returns', 'return')),
    ]

    option_spec = {
        'nodoc': bool_option, 'noindex': bool_option,
    }

    def handle_signature(self, sig, signode):
        symbol_name = []

        def render_sexp(sexp, signode=None, prepend_node=None):
            desc_sexplist = addnodes.desc_parameterlist()
            desc_sexplist.child_text_separator = ' '
            if prepend_node:
                desc_sexplist.append(prepend_node)
            if signode:
                signode.append(desc_sexplist)
            for atom in sexp:
                if isinstance(atom, list):
                    render_sexp(atom, desc_sexplist)
                else:
                    render_atom(atom, desc_sexplist)
            return desc_sexplist

        def render_atom(token, signode, noemph=True):
            "add syntax hi-lighting to interesting atoms"

            if token.startswith("&") or token.startswith(":"):
                signode.append(addnodes.desc_parameter(token, token))
            else:
                signode.append(addnodes.desc_parameter(token, token))

        package = self.env.temp_data.get('el:package')

        objtype = self.get_signature_prefix(sig)
        signode.append(addnodes.desc_annotation(objtype, objtype))
        lisp_args = DATA_ARGS.get(package, {}).get(sig, [])

        if lisp_args:
            function_name = addnodes.desc_name(sig, sig + " ")
        else:
            function_name = addnodes.desc_name(sig, sig)

        if lisp_args:
            arg_list = render_sexp(lisp_args, prepend_node=function_name)
            signode.append(arg_list)
        else:
            signode.append(function_name)

        symbol_name = sig
        if not symbol_name:
            raise Exception("Unknown symbol type for signature %s" % sig)
        return objtype.strip(), symbol_name

    def get_index_text(self, name, type):
        return _('%s (Lisp %s)') % (name, type)

    def get_signature_prefix(self, sig):
        return self.objtype + ' '

    def add_target_and_index(self, name, sig, signode):
        # note target
        type, name = name

        if name not in self.state.document.ids:
            signode['names'].append(name)
            signode['ids'].append(name)
            signode['first'] = (not self.names)
            self.state.document.note_explicit_target(signode)
            inv = self.env.domaindata['el']['symbols']
            if name in inv:
                self.state_machine.reporter.warning(
                    'duplicate symbol description of %s, ' % name +
                    'other instance in ' + self.env.doc2path(inv[name][0]),
                    line=self.lineno)
            inv[name] = (self.env.docname, self.objtype)

        indextext = self.get_index_text(name, type)
        if indextext:
            self.indexnode['entries'].append(('single', indextext, name, ''))

    def run(self):
        result = super(ELSExp, self).run()
        if "nodoc" not in self.options:
            package = self.env.temp_data.get('el:package')
            node = addnodes.desc_content()
            string = DATA_DOC_STRINGS.get(package, {}) \
                                     .get(self.names[0][1], "")
            lines = string2lines(string)
            self.state.nested_parse(StringList(lines), 0, node)
            if (result[1][1].children and
                isinstance(result[1][1][0], nodes.field_list)):
                cresult = result[1][1].deepcopy()
                target = result[1][1]
                target.clear()
                target.append(cresult[0])
                target.extend(node)
                target.extend(cresult[1:])
            else:
                cresult = result[1][1].deepcopy()
                target = result[1][1]
                target.clear()
                target.extend(node)
                target.extend(cresult)
        return result


class ELCurrentPackage(Directive):
    """
    This directive is just to tell Sphinx that we're documenting stuff in
    namespace foo.
    """

    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {}

    def run(self):
        env = self.state.document.settings.env
        env.temp_data['el:package'] = self.arguments[0]
        return []


def parse_text_list(argument, delimiter=','):
    """
    Converts a space- or comma-separated list of values into a list
    """
    if delimiter in argument:
        entries = argument.split(delimiter)
    else:
        entries = argument.split()
    return [v.strip() for v in entries]


def compose(f, g):
    def h(*args, **kwds):
        return f(g(*args, **kwds))
    return h


def filter_by_exclude_regexp_list(candidates, regexp_list, getter=lambda x: x):
    """
    Exclude elements in `candidates` that matches to one of the regexp.

    >>> filter_by_exclude_regexp_list(['a', 'aa', 'bb'], ['a+'])
    ['bb']
    >>> filter_by_exclude_regexp_list(['a', 'ab', 'bb'], ['a+', 'a?b$'])
    ['bb']
    >>> filter_by_exclude_regexp_list(
    ...     [{'key': 'a'}, {'key': 'ab'}, {'key': 'bb'}],
    ...     ['a+', 'a?b$'],
    ...     lambda x: x['key'])
    [{'key': 'bb'}]

    """
    for compiled in map(re.compile, regexp_list):
        test = compose(compiled.match, getter)
        candidates = itertools.filterfalse(test, candidates)
    return list(candidates)


def simple_sed(scripts, string):
    r"""
    A simple sed-like function

    >>> simple_sed(['s/before/after/g'], 'and then before that ...')
    'and then after that ...'
    >>> simple_sed([r's/([0-9])/\1\1/g'], 'there are 2 apples')
    'there are 22 apples'

    """
    for scr in scripts:
        scr = scr.lstrip('s')
        scr_split = scr[1:].split(scr[0])
        (regexp, replace) = scr_split[:2]
        string = re.sub(regexp, replace, string)
    return string


class ELKeyMap(Directive):

    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {
        'exclude': parse_text_list,
        'replace': string_list('\n'),
    }

    def run(self):
        env = self.state.document.settings.env
        package = env.temp_data.get('el:package')
        keymap_list = DATA.get(package, {}).get('keymap', [])
        keymap_name = self.arguments[0]
        for keymap in keymap_list:
            if keymap['name'] == keymap_name:
                break
        else:
            return [self.state.reporter.warning(
                "Keymap {0} not found".format(keymap_name))]
        nodelist = []

        mapdoc = keymap['doc']
        if mapdoc:
            nd = nodes.paragraph()
            lines = string2lines(doc_to_rst(mapdoc))
            self.state.nested_parse(StringList(lines), 0, nd)
            nodelist.append(nd)

        exclude = self.options.get('exclude', [])
        replace = self.options.get('replace', [])
        for keybind in filter_by_exclude_regexp_list(
                keymap['data'], exclude, lambda x: x['func']):
            desc = addnodes.desc()
            desc['domain'] = 'el'
            desc['objtype'] = 'keybind'
            desc['noindex'] = False
            signode = addnodes.desc_signature()
            # signode += addnodes.desc_annotation("", 'keybind ')
            key = simple_sed(replace, keybind['key'])
            signode += addnodes.desc_name("", key)
            signode += addnodes.desc_addname("", " " + keybind['func'])
            desc += signode
            if keybind['doc']:
                nd = addnodes.desc_content()
                lines = string2lines(doc_to_rst(keybind['doc']))
                self.state.nested_parse(StringList(lines), 0, nd)
                desc += nodes.definition("", nd)
            nodelist.append(desc)

        return nodelist


class ELXRefRole(XRefRole):
    def process_link(self, env, refnode, has_explicit_title, title, target):
        if not has_explicit_title:
            target = target.lstrip('~')  # only has a meaning for the title
            # if the first character is a tilde, don't display the package
            if title[0:1] == '~':
                title = title[1:]
                dot = title.rfind(':')
                if dot != -1:
                    title = title[dot + 1:]
        return title, target


class ELDomain(Domain):
    """EL language domain."""
    name = 'el'
    label = 'Common Lisp'

    object_types = {
        'package': ObjType(l_('package'), 'package'),
        'function': ObjType(l_('function'), 'function'),
        'macro': ObjType(l_('macro'), 'macro'),
        'variable': ObjType(l_('variable'), 'variable'),
    }

    directives = {
        'package': ELCurrentPackage,
        'function': ELSExp,
        'macro': ELSExp,
        'variable': ELSExp,
        'keymap': ELKeyMap,
    }

    roles = {
        'symbol': ELXRefRole(),
    }
    initial_data = {
        'symbols': {},
    }

    def clear_doc(self, docname):
        for fullname, (fn, _) in self.data['symbols'].items():
            if fn == docname:
                del self.data['symbols'][fullname]

    def find_obj(self, env, name):
        """Find a Lisp symbol for "name", perhaps using the given package
        Returns a list of (name, object entry) tuples.
        """
        symbols = self.data['symbols']
        if ":" in name:
            if name in symbols:
                return [(name, symbols[name])]
        else:
            def filter_symbols(symbol):
                symbol = symbol[0]
                if name == symbol:
                    return True
                if ":" in symbol:
                    symbol = symbol.split(":")[1]
                    if name == symbol:
                        return True
                return False
            return [f for f in filter(filter_symbols, symbols.items())]

    def resolve_xref(self, env, fromdocname, builder,
                     typ, target, node, contnode):
        matches = self.find_obj(env, target)
        if not matches:
            return None
        elif len(matches) > 1:
            env.warn_node(
                'more than one target found for cross-reference '
                '%r: %s' % (target, ', '.join(match[0] for match in matches)),
                node)
        name, obj = matches[0]
        return make_refnode(builder, fromdocname, obj[0], name,
                            contnode, name)

    def get_symbols(self):
        for refname, (docname, type) in self.data['symbols'].items():
            yield (refname, refname, type, docname, refname, 1)


def doc_to_rst(docstring):
    docstring = _eldoc_quote_re.sub(r":el:symbol:`\1`", docstring)
    return docstring
_eldoc_quote_re = re.compile(r"`(\S+)'")


def index_package(emacs, package, prefix, pre_load, extra_args=[]):
    """Call an external lisp program that will return a dictionary of
    doc strings for all public symbols."""
    lisp_script = path.join(path.dirname(path.realpath(__file__)),
                            "eldomain.el")
    command = [emacs, "-Q", "-batch", "-l", pre_load,
               "--eval", '(setq eldomain-prefix "{0}")'.format(prefix),
               "-l", lisp_script] + extra_args
    proc = subprocess.Popen(
        command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (stdout, stderr) = proc.communicate()
    if proc.poll() != 0:
        raise RuntimeError(
            "Error while executing '{0}'.\n\n"
            "STDOUT:\n{1}\n\nSTDERR:\n{2}\n".format(
                ' '.join(command), stdout, stderr))
    DATA[package] = lisp_data = json.loads(stdout.decode())
    DATA_DOC_STRINGS[package] = {}

    # FIXME: support using same name for function/variable/face
    for key in ['face', 'variable', 'function']:
        for data in lisp_data[key]:
            doc = data['doc']
            if doc:
                DATA_DOC_STRINGS[package][data['name']] = doc_to_rst(doc)

    DATA_ARGS[package] = {}

    for data in lisp_data['function']:
        DATA_ARGS[package][data['name']] = data['arg']


def load_packages(app):
    emacs = app.config.emacs_executable
    # `app.confdir` will be ignored if `elisp_pre_load` is an absolute path
    pre_load = path.join(app.confdir, app.config.elisp_pre_load)
    for (name, prefix) in app.config.elisp_packages.items():
        index_package(emacs, name, prefix, pre_load)


def setup(app):
    app.add_domain(ELDomain)
    app.add_config_value('emacs_executable',
                         os.getenv("EMACS") or 'emacs',
                         'env')
    app.add_config_value('elisp_pre_load', 'conf.el', 'env')
    app.add_config_value('elisp_packages', {}, 'env')
    app.connect('builder-inited', load_packages)
