# -*- coding: utf-8 -*-
"""
    babel.messages.extract
    ~~~~~~~~~~~~~~~~~~~~~~

    Basic infrastructure for extracting localizable messages from source files.

    This module defines an extensible system for collecting localizable message
    strings from a variety of sources. A native extractor for Python source
    files is builtin, extractors for other sources can be added using very
    simple plugins.

    The main entry points into the extraction functionality are the functions
    `extract_from_dir` and `extract_from_file`.

    :copyright: (c) 2013-2022 by the Babel Team.
    :license: BSD, see LICENSE for more details.
"""

import os
from os.path import relpath
import sys
from tokenize import generate_tokens, COMMENT, NAME, OP, STRING

from babel.util import parse_encoding, parse_future_flags, pathmatch, has_python_format, has_python_brace_format
from textwrap import dedent


GROUP_NAME = 'babel.extractors'

DEFAULT_KEYWORDS = {
    '_': None,
    'gettext': None,
    'ngettext': (1, 2),
    'ugettext': None,
    'ungettext': (1, 2),
    'dgettext': (2,),
    'dngettext': (2, 3),
    'N_': None,
    'pgettext': ((1, 'c'), 2),
    'npgettext': ((1, 'c'), 2, 3)
}

DEFAULT_MAPPING = [('**.py', 'python')]

empty_msgid_warning = (
    '%s: warning: Empty msgid.  It is reserved by GNU gettext: gettext("") '
    'returns the header entry with meta information, not the empty string.')


def _strip_comment_tags(comments, tags):
    """Helper function for `extract` that strips comment tags from strings
    in a list of comment lines.  This functions operates in-place.
    """
    def _strip(line):
        for tag in tags:
            if line.startswith(tag):
                return line[len(tag):].strip()
        return line
    comments[:] = map(_strip, comments)


def default_directory_filter(dirpath):
    subdir = os.path.basename(dirpath)
    # Legacy default behavior: ignore dot and underscore directories
    return not (subdir.startswith('.') or subdir.startswith('_'))


def extract_from_dir(
    dirname=None,
    method_map=DEFAULT_MAPPING,
    options_map=None,
    keywords=DEFAULT_KEYWORDS,
    comment_tags=(),
    callback=None,
    strip_comment_tags=False,
    directory_filter=None,
):
    """Extract messages from any source files found in the given directory.

    This function generates tuples of the form ``(filename, lineno, message,
    comments, context, flags)``.

    Which extraction method is used per file is determined by the `method_map`
    parameter, which maps extended glob patterns to extraction method names.
    For example, the following is the default mapping:

    >>> method_map = [
    ...     ('**.py', 'python')
    ... ]

    This basically says that files with the filename extension ".py" at any
    level inside the directory should be processed by the "python" extraction
    method. Files that don't match any of the mapping patterns are ignored. See
    the documentation of the `pathmatch` function for details on the pattern
    syntax.

    The following extended mapping would also use the "genshi" extraction
    method on any file in "templates" subdirectory:

    >>> method_map = [
    ...     ('**/templates/**.*', 'genshi'),
    ...     ('**.py', 'python')
    ... ]

    The dictionary provided by the optional `options_map` parameter augments
    these mappings. It uses extended glob patterns as keys, and the values are
    dictionaries mapping options names to option values (both strings).

    The glob patterns of the `options_map` do not necessarily need to be the
    same as those used in the method mapping. For example, while all files in
    the ``templates`` folders in an application may be Genshi applications, the
    options for those files may differ based on extension:

    >>> options_map = {
    ...     '**/templates/**.txt': {
    ...         'template_class': 'genshi.template:TextTemplate',
    ...         'encoding': 'latin-1'
    ...     },
    ...     '**/templates/**.html': {
    ...         'include_attrs': ''
    ...     }
    ... }

    :param dirname: the path to the directory to extract messages from.  If
                    not given the current working directory is used.
    :param method_map: a list of ``(pattern, method)`` tuples that maps of
                       extraction method names to extended glob patterns
    :param options_map: a dictionary of additional options (optional)
    :param keywords: a dictionary mapping keywords (i.e. names of functions
                     that should be recognized as translation functions) to
                     tuples that specify which of their arguments contain
                     localizable strings
    :param comment_tags: a list of tags of translator comments to search for
                         and include in the results
    :param callback: a function that is called for every file that message are
                     extracted from, just before the extraction itself is
                     performed; the function is passed the filename, the name
                     of the extraction method and and the options dictionary as
                     positional arguments, in that order
    :param strip_comment_tags: a flag that if set to `True` causes all comment
                               tags to be removed from the collected comments.
    :param directory_filter: a callback to determine whether a directory should
                             be recursed into. Receives the full directory path;
                             should return True if the directory is valid.
    :see: `pathmatch`
    """
    if dirname is None:
        dirname = os.getcwd()
    if options_map is None:
        options_map = {}
    if directory_filter is None:
        directory_filter = default_directory_filter

    absname = os.path.abspath(dirname)
    for root, dirnames, filenames in os.walk(absname):
        dirnames[:] = [
            subdir for subdir in dirnames
            if directory_filter(os.path.join(root, subdir))
        ]
        dirnames.sort()
        filenames.sort()
        for filename in filenames:
            filepath = os.path.join(root, filename).replace(os.sep, '/')

            for message_tuple in check_and_call_extract_file(
                filepath,
                method_map,
                options_map,
                callback,
                keywords,
                comment_tags,
                strip_comment_tags,
                dirpath=absname,
            ):
                yield message_tuple


def check_and_call_extract_file(filepath, method_map, options_map,
                                callback, keywords, comment_tags,
                                strip_comment_tags, dirpath=None):
    """Checks if the given file matches an extraction method mapping, and if so, calls extract_from_file.

    Note that the extraction method mappings are based relative to dirpath.
    So, given an absolute path to a file `filepath`, we want to check using
    just the relative path from `dirpath` to `filepath`.

    Yields 6-tuples (filename, lineno, messages, comments, context, flags).

    :param filepath: An absolute path to a file that exists.
    :param method_map: a list of ``(pattern, method)`` tuples that maps of
                       extraction method names to extended glob patterns
    :param options_map: a dictionary of additional options (optional)
    :param callback: a function that is called for every file that message are
                     extracted from, just before the extraction itself is
                     performed; the function is passed the filename, the name
                     of the extraction method and and the options dictionary as
                     positional arguments, in that order
    :param keywords: a dictionary mapping keywords (i.e. names of functions
                     that should be recognized as translation functions) to
                     tuples that specify which of their arguments contain
                     localizable strings
    :param comment_tags: a list of tags of translator comments to search for
                         and include in the results
    :param strip_comment_tags: a flag that if set to `True` causes all comment
                               tags to be removed from the collected comments.
    :param dirpath: the path to the directory to extract messages from.
    :return: iterable of 6-tuples (filename, lineno, messages, comments, context)
    :rtype: Iterable[tuple[str, int, str|tuple[str], list[str], str|None, set[str]]
    """
    # filename is the relative path from dirpath to the actual file
    filename = relpath(filepath, dirpath)

    for pattern, method in method_map:
        if not pathmatch(pattern, filename):
            continue

        options = {}
        for opattern, odict in options_map.items():
            if pathmatch(opattern, filename):
                options = odict
        if callback:
            callback(filename, method, options)
        for message_tuple in extract_from_file(
            method, filepath,
            keywords=keywords,
            comment_tags=comment_tags,
            options=options,
            strip_comment_tags=strip_comment_tags
        ):
            if len(message_tuple) == 4:
                yield (filename, *message_tuple, set())
            else:
                yield (filename, *message_tuple)

        break


def extract_from_file(method, filename, keywords=DEFAULT_KEYWORDS,
                      comment_tags=(), options=None, strip_comment_tags=False):
    """Extract messages from a specific file.

    This function returns a list of tuples of the form ``(lineno, message, comments, context, flags)``.

    :param filename: the path to the file to extract messages from
    :param method: a string specifying the extraction method (.e.g. "python")
    :param keywords: a dictionary mapping keywords (i.e. names of functions
                     that should be recognized as translation functions) to
                     tuples that specify which of their arguments contain
                     localizable strings
    :param comment_tags: a list of translator tags to search for and include
                         in the results
    :param strip_comment_tags: a flag that if set to `True` causes all comment
                               tags to be removed from the collected comments.
    :param options: a dictionary of additional options (optional)
    :returns: list of tuples of the form ``(lineno, message, comments, context, flags)``
    :rtype: list[tuple[int, str|tuple[str], list[str], str|None, set[str]]
    """
    if method == 'ignore':
        return []

    with open(filename, 'rb') as fileobj:
        return list(extract(method, fileobj, keywords, comment_tags,
                            options, strip_comment_tags))


def extract(method, fileobj, keywords=DEFAULT_KEYWORDS, comment_tags=(),
            options=None, strip_comment_tags=False):
    """Extract messages from the given file-like object using the specified
    extraction method.

    This function returns tuples of the form ``(lineno, message, comments, context)``.

    The implementation dispatches the actual extraction to plugins, based on the
    value of the ``method`` parameter.

    >>> source = b'''# foo module
    ... def run(argv):
    ...    print(_('Hello, world!'))
    ... '''

    >>> from io import BytesIO
    >>> for message in extract('python', BytesIO(source)):
    ...     print(message)
    (3, u'Hello, world!', [], None, set())

    :param method: an extraction method (a callable), or
                   a string specifying the extraction method (.e.g. "python");
                   if this is a simple name, the extraction function will be
                   looked up by entry point; if it is an explicit reference
                   to a function (of the form ``package.module:funcname`` or
                   ``package.module.funcname``), the corresponding function
                   will be imported and used
    :param fileobj: the file-like object the messages should be extracted from
    :param keywords: a dictionary mapping keywords (i.e. names of functions
                     that should be recognized as translation functions) to
                     tuples that specify which of their arguments contain
                     localizable strings
    :param comment_tags: a list of translator tags to search for and include
                         in the results
    :param options: a dictionary of additional options (optional)
    :param strip_comment_tags: a flag that if set to `True` causes all comment
                               tags to be removed from the collected comments.
    :raise ValueError: if the extraction method is not registered
    :returns: iterable of tuples of the form ``(lineno, message, comments, context, flags)``
    :rtype: Iterable[tuple[int, str|tuple[str], list[str], str|None, set[str]]
    """
    func = None
    if callable(method):
        func = method
    elif ':' in method or '.' in method:
        if ':' not in method:
            lastdot = method.rfind('.')
            module, attrname = method[:lastdot], method[lastdot + 1:]
        else:
            module, attrname = method.split(':', 1)
        func = getattr(__import__(module, {}, {}, [attrname]), attrname)
    else:
        try:
            from pkg_resources import working_set
        except ImportError:
            pass
        else:
            for entry_point in working_set.iter_entry_points(GROUP_NAME,
                                                             method):
                func = entry_point.load(require=True)
                break
        if func is None:
            # if pkg_resources is not available or no usable egg-info was found
            # (see #230), we resort to looking up the builtin extractors
            # directly
            builtin = {
                'ignore': extract_nothing,
                'python': extract_python,
                'javascript': extract_javascript,
                'django': extract_django,
            }
            func = builtin.get(method)

    if func is None:
        raise ValueError('Unknown extraction method %r' % method)

    results = func(fileobj, keywords.keys(), comment_tags,
                   options=options or {})

    for lineno, funcname, messages, comments, *rest in results:
        if funcname:
            spec = keywords[funcname] or (1,)
        else:
            spec = (1,)
        if not isinstance(messages, (list, tuple)):
            messages = [messages]
        if not messages:
            continue

        if rest:
            flags = rest[0]
        else:
            flags = set()

        # Validate the messages against the keyword's specification
        context = None
        msgs = []
        invalid = False
        # last_index is 1 based like the keyword spec
        last_index = len(messages)
        for index in spec:
            if isinstance(index, tuple):
                context = messages[index[0] - 1]
                continue
            if last_index < index:
                # Not enough arguments
                invalid = True
                break
            message = messages[index - 1]
            if message is None:
                invalid = True
                break
            msgs.append(message)
        if invalid:
            continue

        # keyword spec indexes are 1 based, therefore '-1'
        if isinstance(spec[0], tuple):
            # context-aware *gettext method
            first_msg_index = spec[1] - 1
        else:
            first_msg_index = spec[0] - 1
        if not messages[first_msg_index]:
            # An empty string msgid isn't valid, emit a warning
            where = '%s:%i' % (hasattr(fileobj, 'name') and
                               fileobj.name or '(unknown)', lineno)
            sys.stderr.write((empty_msgid_warning % where) + '\n')
            continue

        messages = tuple(msgs)
        if len(messages) == 1:
            messages = messages[0]

        if strip_comment_tags:
            _strip_comment_tags(comments, comment_tags)
        yield lineno, messages, comments, context, flags


def extract_nothing(fileobj, keywords, comment_tags, options):
    """Pseudo extractor that does not actually extract anything, but simply
    returns an empty list.
    """
    return []


def extract_python(fileobj, keywords, comment_tags, options):
    """Extract messages from Python source code.

    It returns an iterator yielding tuples in the following form ``(lineno,
    funcname, message, comments, flags)``.

    :param fileobj: the seekable, file-like object the messages should be
                    extracted from
    :param keywords: a list of keywords (i.e. function names) that should be
                     recognized as translation functions
    :param comment_tags: a list of translator tags to search for and include
                         in the results
    :param options: a dictionary of additional options (optional)
    :rtype: ``iterator``
    """
    funcname = lineno = message_lineno = None
    call_stack = -1
    buf = []
    messages = []
    translator_comments = []
    in_def = in_translator_comments = False
    comment_tag = None
    flags = set()

    encoding = parse_encoding(fileobj) or options.get('encoding', 'UTF-8')
    future_flags = parse_future_flags(fileobj, encoding)
    next_line = lambda: fileobj.readline().decode(encoding)

    tokens = generate_tokens(next_line)
    for tok, value, (lineno, _), _, _ in tokens:
        if call_stack == -1 and tok == NAME and value in ('def', 'class'):
            in_def = True
        elif tok == OP and value == '(':
            if in_def:
                # Avoid false positives for declarations such as:
                # def gettext(arg='message'):
                in_def = False
                continue
            if funcname:
                message_lineno = lineno
                call_stack += 1
        elif in_def and tok == OP and value == ':':
            # End of a class definition without parens
            in_def = False
            continue
        elif call_stack == -1 and tok == COMMENT:
            # Strip the comment token from the line
            value = value[1:].strip()
            if in_translator_comments and \
                    translator_comments[-1][0] == lineno - 1:
                # We're already inside a translator comment, continue appending
                translator_comments.append((lineno, value))
                continue
            # If execution reaches this point, let's see if comment line
            # starts with one of the comment tags
            for comment_tag in comment_tags:
                if value.startswith(comment_tag):
                    in_translator_comments = True
                    translator_comments.append((lineno, value))
                    break
        elif funcname and call_stack == 0:
            nested = (tok == NAME and value in keywords)
            if (tok == OP and value == ')') or nested:
                if buf:
                    messages.append(''.join(buf))
                    del buf[:]
                else:
                    messages.append(None)

                if has_python_format(message for message in messages if message):
                    flags.add("python-format")

                if has_python_brace_format(message for message in messages if message):
                    flags.add('python-brace-format')

                if len(messages) > 1:
                    messages = tuple(messages)
                else:
                    messages = messages[0]
                # Comments don't apply unless they immediately preceed the
                # message
                if translator_comments and \
                        translator_comments[-1][0] < message_lineno - 1:
                    translator_comments = []

                yield (message_lineno, funcname, messages,
                       [comment[1] for comment in translator_comments], flags)

                funcname = lineno = message_lineno = None
                call_stack = -1
                messages = []
                translator_comments = []
                flags = set()
                in_translator_comments = False
                if nested:
                    funcname = value
            elif tok == STRING:
                # Unwrap quotes in a safe manner, maintaining the string's
                # encoding
                # https://sourceforge.net/tracker/?func=detail&atid=355470&
                # aid=617979&group_id=5470
                code = compile('# coding=%s\n%s' % (str(encoding), value),
                               '<string>', 'eval', future_flags)
                value = eval(code, {'__builtins__': {}}, {})
                buf.append(value)
            elif tok == OP and value == ',':
                if buf:
                    messages.append(''.join(buf))
                    del buf[:]
                else:
                    messages.append(None)
                if translator_comments:
                    # We have translator comments, and since we're on a
                    # comma(,) user is allowed to break into a new line
                    # Let's increase the last comment's lineno in order
                    # for the comment to still be a valid one
                    old_lineno, old_comment = translator_comments.pop()
                    translator_comments.append((old_lineno + 1, old_comment))
        elif call_stack > 0 and tok == OP and value == ')':
            call_stack -= 1
        elif funcname and call_stack == -1:
            funcname = None
        elif tok == NAME and value in keywords:
            funcname = value


def extract_javascript(fileobj, keywords, comment_tags, options):
    """Extract messages from JavaScript source code.

    :param fileobj: the seekable, file-like object the messages should be
                    extracted from
    :param keywords: a list of keywords (i.e. function names) that should be
                     recognized as translation functions
    :param comment_tags: a list of translator tags to search for and include
                         in the results
    :param options: a dictionary of additional options (optional)
                    Supported options are:
                    * `jsx` -- set to false to disable JSX/E4X support.
                    * `template_string` -- set to false to disable ES6 template string support.
    """
    from babel.messages.jslexer import Token, tokenize, unquote_string
    funcname = message_lineno = None
    messages = []
    last_argument = None
    translator_comments = []
    concatenate_next = False
    encoding = options.get('encoding', 'utf-8')
    last_token = None
    call_stack = -1
    dotted = any('.' in kw for kw in keywords)

    for token in tokenize(
        fileobj.read().decode(encoding),
        jsx=options.get("jsx", True),
        template_string=options.get("template_string", True),
        dotted=dotted
    ):
        if (  # Turn keyword`foo` expressions into keyword("foo") calls:
            funcname and  # have a keyword...
            (last_token and last_token.type == 'name') and  # we've seen nothing after the keyword...
            token.type == 'template_string'  # this is a template string
        ):
            message_lineno = token.lineno
            messages = [unquote_string(token.value)]
            call_stack = 0
            token = Token('operator', ')', token.lineno)

        if token.type == 'operator' and token.value == '(':
            if funcname:
                message_lineno = token.lineno
                call_stack += 1

        elif call_stack == -1 and token.type == 'linecomment':
            value = token.value[2:].strip()
            if translator_comments and \
               translator_comments[-1][0] == token.lineno - 1:
                translator_comments.append((token.lineno, value))
                continue

            for comment_tag in comment_tags:
                if value.startswith(comment_tag):
                    translator_comments.append((token.lineno, value.strip()))
                    break

        elif token.type == 'multilinecomment':
            # only one multi-line comment may preceed a translation
            translator_comments = []
            value = token.value[2:-2].strip()
            for comment_tag in comment_tags:
                if value.startswith(comment_tag):
                    lines = value.splitlines()
                    if lines:
                        lines[0] = lines[0].strip()
                        lines[1:] = dedent('\n'.join(lines[1:])).splitlines()
                        for offset, line in enumerate(lines):
                            translator_comments.append((token.lineno + offset,
                                                        line))
                    break

        elif funcname and call_stack == 0:
            if token.type == 'operator' and token.value == ')':
                if last_argument is not None:
                    messages.append(last_argument)
                if len(messages) > 1:
                    messages = tuple(messages)
                elif messages:
                    messages = messages[0]
                else:
                    messages = None

                # Comments don't apply unless they immediately precede the
                # message
                if translator_comments and \
                   translator_comments[-1][0] < message_lineno - 1:
                    translator_comments = []

                if messages is not None:
                    yield (message_lineno, funcname, messages,
                           [comment[1] for comment in translator_comments], set())

                funcname = message_lineno = last_argument = None
                concatenate_next = False
                translator_comments = []
                messages = []
                call_stack = -1

            elif token.type in ('string', 'template_string'):
                new_value = unquote_string(token.value)
                if concatenate_next:
                    last_argument = (last_argument or '') + new_value
                    concatenate_next = False
                else:
                    last_argument = new_value

            elif token.type == 'operator':
                if token.value == ',':
                    if last_argument is not None:
                        messages.append(last_argument)
                        last_argument = None
                    else:
                        messages.append(None)
                    concatenate_next = False
                elif token.value == '+':
                    concatenate_next = True

        elif call_stack > 0 and token.type == 'operator' \
                and token.value == ')':
            call_stack -= 1

        elif funcname and call_stack == -1:
            funcname = None

        elif call_stack == -1 and token.type == 'name' and \
            token.value in keywords and \
            (last_token is None or last_token.type != 'name' or
             last_token.value != 'function'):
            funcname = token.value

        last_token = token


def extract_django(fileobj, keywords, comment_tags, options):
    """
    Extract messages from Django template files.

    :param fileobj: the file-like object the messages should be extracted from
    :param keywords: a list of keywords (i.e. function names) that should
                     be recognized as translation functions
    :param comment_tags: a list of translator tags to search for and
                         include in the results
    :param options: a dictionary of additional options (optional)
    :return: an iterator over ``(lineno, funcname, message, comments)``
             tuples
    """

    from django.template.base import Lexer, TokenType
    from django.utils.encoding import smart_str
    from django.utils.translation import trim_whitespace
    from django.utils.translation.template import (
        block_re,
        constant_re,
        context_re,
        endblock_re,
        inline_re,
        plural_re,
    )

    def strip_quotes(s: str) -> str:
        if (s[0] == s[-1]) and s.startswith(("'", '"')):
            return s[1:-1]
        return s

    def join_tokens(tokens: list[str], trim=False) -> str:
        message = "".join(tokens)
        if trim:
            message = trim_whitespace(message)
        return message

    message_context = None
    intrans = False
    inplural = False
    trimmed = False
    singular = []
    plural = []
    incomment = False
    comment = []
    flags = set()
    lineno_comment_map = {}
    comment_lineno_cache = None

    # A buffer of pending translator comments
    comments = []

    for t in Lexer(fileobj.read().decode()).tokenize():
        assert t.lineno
        if incomment:
            if t.token_type == TokenType.BLOCK and t.contents == "endcomment":
                content = "".join(comment)
                translators_comment_start = None
                for _lineno, line in enumerate(content.splitlines(True)):
                    if any(
                        line.lstrip().startswith(comment_tag)
                        for comment_tag in comment_tags
                    ):
                        translators_comment_start = _lineno
                if translators_comment_start:
                    comments += (
                        line
                        for _lineno, line in enumerate(content.splitlines(True))
                        if _lineno >= translators_comment_start
                    )
                incomment = False
                comment = []
            else:
                comment.append(t.contents)
        elif intrans:
            if t.token_type == TokenType.BLOCK:
                endbmatch = endblock_re.match(t.contents)
                pluralmatch = plural_re.match(t.contents)
                if endbmatch:
                    if inplural:
                        if message_context:
                            yield (
                                t.lineno,
                                "npgettext",
                                (
                                    smart_str(message_context),
                                    smart_str(join_tokens(singular, trimmed)),
                                    smart_str(join_tokens(plural, trimmed)),
                                ),
                                comments,
                                flags,
                            )
                            comments = []
                        else:
                            yield (
                                t.lineno,
                                "ngettext",
                                (
                                    smart_str(join_tokens(singular, trimmed)),
                                    smart_str(join_tokens(plural, trimmed)),
                                ),
                                comments,
                                flags,
                            )
                            comments = []
                    else:
                        if message_context:
                            yield (
                                t.lineno,
                                "pgettext",
                                (
                                    smart_str(message_context),
                                    smart_str(join_tokens(singular, trimmed)),
                                ),
                                comments,
                                flags,
                            )
                            comments = []
                        else:
                            yield (
                                t.lineno,
                                None,
                                smart_str(join_tokens(singular, trimmed)),
                                comments,
                                flags,
                            )
                            comments = []
                    message_context = None
                    intrans = False
                    inplural = False
                    singular = []
                    plural = []
                    flags = set()
                elif pluralmatch:
                    inplural = True
                else:
                    raise SyntaxError(
                        f"Translation blocks must not include other block tags: "
                        f"{t.contents} (line {t.lineno})"
                    )
            elif t.token_type == TokenType.VAR:
                if inplural:
                    plural.append("%%(%s)s" % t.contents)
                else:
                    singular.append("%%(%s)s" % t.contents)
                flags.add('python-format')
            elif t.token_type == TokenType.TEXT:
                contents = t.contents.replace("%", "%%")
                if inplural:
                    plural.append(contents)
                else:
                    singular.append(contents)
        else:
            # Handle comment tokens (`{# ... #}`) plus other constructs on
            # the same line:
            if comment_lineno_cache is not None:
                cur_lineno = t.lineno + t.contents.count("\n")
                if comment_lineno_cache == cur_lineno:
                    if t.token_type != TokenType.COMMENT:
                        for c in lineno_comment_map[comment_lineno_cache]:
                            print(
                                f"{fileobj.name}:{cur_lineno}: "
                                f"The translator-targeted comment '{c}' "
                                f"was ignored, because it wasn't "
                                f"the last item on the line."
                            )
                        lineno_comment_map[comment_lineno_cache] = []
                else:
                    comments += lineno_comment_map[comment_lineno_cache]
                comment_lineno_cache = None

            if t.token_type == TokenType.BLOCK:
                imatch = inline_re.match(t.contents)
                bmatch = block_re.match(t.contents)
                cmatches = constant_re.findall(t.contents)
                if imatch:
                    g = imatch[1]
                    if g[0] == '"':
                        g = g.strip('"')
                    elif g[0] == "'":
                        g = g.strip("'")
                    g = g.replace("%", "%%")
                    if imatch[2]:
                        # A context is provided
                        context_match = context_re.match(imatch[2])
                        message_context = context_match[1]
                        if message_context[0] == '"':
                            message_context = message_context.strip('"')
                        elif message_context[0] == "'":
                            message_context = message_context.strip("'")
                        yield (
                            t.lineno,
                            "pgettext",
                            (smart_str(message_context), smart_str(g)),
                            comments,
                            flags,
                        )
                        comments = []
                        message_context = None
                    else:
                        yield t.lineno, None, smart_str(g), comments, flags
                        comments = []
                elif bmatch:
                    for fmatch in constant_re.findall(t.contents):
                        stripped_fmatch = strip_quotes(fmatch)
                        yield t.lineno, None, smart_str(stripped_fmatch), comments, flags
                        comments = []
                    if bmatch[1]:
                        # A context is provided
                        context_match = context_re.match(bmatch[1])
                        message_context = context_match[1]
                        if message_context[0] == '"':
                            message_context = message_context.strip('"')
                        elif message_context[0] == "'":
                            message_context = message_context.strip("'")
                    intrans = True
                    inplural = False
                    trimmed = "trimmed" in t.split_contents()
                    singular = []
                    plural = []
                    flags = set()
                elif cmatches:
                    for cmatch in cmatches:
                        stripped_cmatch = strip_quotes(cmatch)
                        yield t.lineno, None, smart_str(stripped_cmatch), comments, flags
                        comments = []
                elif t.contents == "comment":
                    incomment = True
                else:
                    if comments:
                        print(
                            f"{fileobj.name}:{t.lineno}: "
                            "This is not a translation tag, the "
                            "translator-targeted comments above it will be ignored"
                        )
                    comments = []
            elif t.token_type == TokenType.VAR:
                parts = t.contents.split("|")
                cmatch = constant_re.match(parts[0])
                if cmatch:
                    stripped_cmatch = strip_quotes(cmatch[1])
                    yield t.lineno, None, smart_str(stripped_cmatch), comments, flags
                    comments = []
                for p in parts[1:]:
                    if p.find(":_(") >= 0:
                        p1 = p.split(":", 1)[1]
                        if p1[0] == "_":
                            p1 = p1[1:]
                        if p1[0] == "(":
                            p1 = p1.strip("()")
                        p1 = strip_quotes(p1)
                        yield t.lineno, None, smart_str(p1), comments, flags
                        comments = []

            elif t.token_type == TokenType.COMMENT:
                if any(
                    t.contents.lstrip().startswith(comment_tag)
                    for comment_tag in comment_tags
                ):
                    lineno_comment_map.setdefault(t.lineno, []).append(t.contents)
                    comment_lineno_cache = t.lineno
