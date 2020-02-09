;;; ein:markdown-mode.el --- Major mode for Markdown-formatted text -*- lexical-binding: t; -*-

;; Copyright (C) 2007-2017 Jason R. Blevins and markdown-mode
;; contributors.

;; Author: Jason R. Blevins <jblevins@xbeta.org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Pare markdown-mode for EIN (and fix some bugs)

;;; Code:

(require 'easymenu)
(require 'outline)
(require 'thingatpt)
(require 'cl-lib)
(require 'url-parse)
(require 'button)
(require 'color)
(require 'rx)

(defvar jit-lock-start)
(defvar jit-lock-end)
(defvar flyspell-generic-check-word-predicate)

(declare-function eww-open-file "eww")
(declare-function url-path-and-query "url-parse")


;;; Constants =================================================================

(defconst ein:markdown-mode-version "2.4-dev"
  "ein:markdown mode version number.")

(defconst ein:markdown-output-buffer-name "*markdown-output*"
  "Name of temporary buffer for markdown command output.")


;;; Global Variables ==========================================================

(defvar ein:markdown-reference-label-history nil
  "History of used reference labels.")

;;; Customizable Variables ====================================================

(defvar ein:markdown-mode-hook nil
  "Hook run when entering Markdown mode.")

(defgroup ein:markdown nil
  "Major mode for editing text files in Markdown format."
  :prefix "ein:markdown-"
  :group 'text)

(defcustom ein:markdown-command "ein:markdown"
  "Command to run markdown."
  :group 'ein:markdown
  :type '(choice (string :tag "Shell command") function))

(defcustom ein:markdown-command-needs-filename nil
  "Set to non-nil if `markdown-command' does not accept input from stdin.
Instead, it will be passed a filename as the final command line
option.  As a result, you will only be able to run Markdown from
buffers which are visiting a file."
  :group 'ein:markdown
  :type 'boolean)

(defcustom ein:markdown-open-command nil
  "Command used for opening Markdown files directly.
For example, a standalone Markdown previewer.  This command will
be called with a single argument: the filename of the current
buffer.  It can also be a function, which will be called without
arguments."
  :group 'ein:markdown
  :type '(choice file function (const :tag "None" nil)))

(defcustom ein:markdown-hr-strings
  '("-------------------------------------------------------------------------------"
    "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *"
    "---------------------------------------"
    "* * * * * * * * * * * * * * * * * * * *"
    "---------"
    "* * * * *")
  "Strings to use when inserting horizontal rules.
The first string in the list will be the default when inserting a
horizontal rule.  Strings should be listed in decreasing order of
prominence (as in headings from level one to six) for use with
promotion and demotion functions."
  :group 'ein:markdown
  :type '(repeat string))

(defcustom ein:markdown-bold-underscore nil
  "Use two underscores when inserting bold text instead of two asterisks."
  :group 'ein:markdown
  :type 'boolean)

(defcustom ein:markdown-italic-underscore nil
  "Use underscores when inserting italic text instead of asterisks."
  :group 'ein:markdown
  :type 'boolean)

(defcustom ein:markdown-marginalize-headers nil
  "When non-nil, put opening atx header markup in a left margin.

This setting goes well with `markdown-asymmetric-header'.  But
sadly it conflicts with `linum-mode' since they both use the
same margin."
  :group 'ein:markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(ein:markdown-mode . "2.4"))

(defcustom ein:markdown-marginalize-headers-margin-width 6
  "Character width of margin used for marginalized headers.
The default value is based on there being six heading levels
defined by Markdown and HTML.  Increasing this produces extra
whitespace on the left.  Decreasing it may be preferred when
fewer than six nested heading levels are used."
  :group 'ein:markdown
  :type 'natnump
  :safe 'natnump
  :package-version '(ein:markdown-mode . "2.4"))

(defcustom ein:markdown-asymmetric-header nil
  "Determines if atx header style will be asymmetric.
Set to a non-nil value to use asymmetric header styling, placing
header markup only at the beginning of the line. By default,
balanced markup will be inserted at the beginning and end of the
line around the header title."
  :group 'ein:markdown
  :type 'boolean)

(defcustom ein:markdown-indent-function 'ein:markdown-indent-line
  "Function to use to indent."
  :group 'ein:markdown
  :type 'function)

(defcustom ein:markdown-indent-on-enter t
  "Determines indentation behavior when pressing \\[newline].
Possible settings are nil, t, and 'indent-and-new-item.

When non-nil, pressing \\[newline] will call `newline-and-indent'
to indent the following line according to the context using
`markdown-indent-function'.  In this case, note that
\\[electric-newline-and-maybe-indent] can still be used to insert
a newline without indentation.

When set to 'indent-and-new-item and the point is in a list item
when \\[newline] is pressed, the list will be continued on the next
line, where a new item will be inserted.

When set to nil, simply call `newline' as usual.  In this case,
you can still indent lines using \\[ein:markdown-cycle] and continue
lists with \\[ein:markdown-insert-list-item].

Note that this assumes the variable `electric-indent-mode' is
non-nil (enabled).  When it is *disabled*, the behavior of
\\[newline] and `\\[electric-newline-and-maybe-indent]' are
reversed."
  :group 'ein:markdown
  :type '(choice (const :tag "Don't automatically indent" nil)
                 (const :tag "Automatically indent" t)
                 (const :tag "Automatically indent and insert new list items" indent-and-new-item)))

(defcustom ein:markdown-uri-types
  '("acap" "cid" "data" "dav" "fax" "file" "ftp"
    "gopher" "http" "https" "imap" "ldap" "mailto"
    "mid" "message" "modem" "news" "nfs" "nntp"
    "pop" "prospero" "rtsp" "service" "sip" "tel"
    "telnet" "tip" "urn" "vemmi" "wais")
  "Link types for syntax highlighting of URIs."
  :group 'ein:markdown
  :type '(repeat (string :tag "URI scheme")))

(defcustom ein:markdown-url-compose-char
  '(?∞ ?… ?⋯ ?# ?★ ?⚓)
  "Placeholder character for hidden URLs.
This may be a single character or a list of characters. In case
of a list, the first one that satisfies `char-displayable-p' will
be used."
  :type '(choice
          (character :tag "Single URL replacement character")
          (repeat :tag "List of possible URL replacement characters"
                  character))
  :package-version '(ein:markdown-mode . "2.3"))

(defcustom ein:markdown-blockquote-display-char
  '("▌" "┃" ">")
  "String to display when hiding blockquote markup.
This may be a single string or a list of string. In case of a
list, the first one that satisfies `char-displayable-p' will be
used."
  :type 'string
  :type '(choice
          (string :tag "Single blockquote display string")
          (repeat :tag "List of possible blockquote display strings" string))
  :package-version '(ein:markdown-mode . "2.3"))

(defcustom ein:markdown-hr-display-char
  '(?─ ?━ ?-)
  "Character for hiding horizontal rule markup.
This may be a single character or a list of characters.  In case
of a list, the first one that satisfies `char-displayable-p' will
be used."
  :group 'ein:markdown
  :type '(choice
          (character :tag "Single HR display character")
          (repeat :tag "List of possible HR display characters" character))
  :package-version '(ein:markdown-mode . "2.3"))

(defcustom ein:markdown-definition-display-char
  '(?⁘ ?⁙ ?≡ ?⌑ ?◊ ?:)
  "Character for replacing definition list markup.
This may be a single character or a list of characters.  In case
of a list, the first one that satisfies `char-displayable-p' will
be used."
  :type '(choice
          (character :tag "Single definition list character")
          (repeat :tag "List of possible definition list characters" character))
  :package-version '(ein:markdown-mode . "2.3"))

(defcustom ein:markdown-enable-math nil
  "Syntax highlighting for inline LaTeX and itex expressions.
Set this to a non-nil value to turn on math support by default.
Math support can be enabled, disabled, or toggled later using
`markdown-toggle-math' or \\[ein:markdown-toggle-math]."
  :group 'ein:markdown
  :type 'boolean
  :safe 'booleanp)
(make-variable-buffer-local 'ein:markdown-enable-math)

(defcustom ein:markdown-enable-html t
  "Enable font-lock support for HTML tags and attributes."
  :group 'ein:markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(ein:markdown-mode . "2.4"))

(defcustom ein:markdown-css-paths nil
  "URL of CSS file to link to in the output XHTML."
  :group 'ein:markdown
  :type '(repeat (string :tag "CSS File Path")))

(defcustom ein:markdown-content-type "text/html"
  "Content type string for the http-equiv header in XHTML output.
When set to an empty string, this attribute is omitted.  Defaults to
`text/html'."
  :group 'ein:markdown
  :type 'string)

(defcustom ein:markdown-coding-system nil
  "Character set string for the http-equiv header in XHTML output.
Defaults to `buffer-file-coding-system' (and falling back to
`utf-8' when not available).  Common settings are `iso-8859-1'
and `iso-latin-1'.  Use `list-coding-systems' for more choices."
  :group 'ein:markdown
  :type 'coding-system)

(defcustom ein:markdown-xhtml-header-content ""
  "Additional content to include in the XHTML <head> block."
  :group 'ein:markdown
  :type 'string)

(defcustom ein:markdown-xhtml-body-preamble ""
  "Content to include in the XHTML <body> block, before the output."
  :group 'ein:markdown
  :type 'string
  :safe 'stringp
  :package-version '(ein:markdown-mode . "2.4"))

(defcustom ein:markdown-xhtml-body-epilogue ""
  "Content to include in the XHTML <body> block, after the output."
  :group 'ein:markdown
  :type 'string
  :safe 'stringp
  :package-version '(ein:markdown-mode . "2.4"))

(defcustom ein:markdown-xhtml-standalone-regexp
  "^\\(<\\?xml\\|<!DOCTYPE\\|<html\\)"
  "Regexp indicating whether `markdown-command' output is standalone XHTML."
  :group 'ein:markdown
  :type 'regexp)

(defcustom ein:markdown-link-space-sub-char "_"
  "Character to use instead of spaces when mapping wiki links to filenames."
  :group 'ein:markdown
  :type 'string)

(defcustom ein:markdown-reference-location 'header
  "Position where new reference definitions are inserted in the document."
  :group 'ein:markdown
  :type '(choice (const :tag "At the end of the document" end)
                 (const :tag "Immediately after the current block" immediately)
                 (const :tag "At the end of the subtree" subtree)
                 (const :tag "Before next header" header)))

(defcustom ein:markdown-footnote-location 'end
  "Position where new footnotes are inserted in the document."
  :group 'ein:markdown
  :type '(choice (const :tag "At the end of the document" end)
                 (const :tag "Immediately after the current block" immediately)
                 (const :tag "At the end of the subtree" subtree)
                 (const :tag "Before next header" header)))

(defcustom ein:markdown-footnote-display '((raise 0.2) (height 0.8))
  "Display specification for footnote markers and inline footnotes.
By default, footnote text is reduced in size and raised.  Set to
nil to disable this."
  :group 'ein:markdown
  :type '(choice (sexp :tag "Display specification")
                 (const :tag "Don't set display property" nil))
  :package-version '(ein:markdown-mode . "2.4"))

(defcustom ein:markdown-sub-superscript-display
  '(((raise -0.3) (height 0.7)) . ((raise 0.3) (height 0.7)))
  "Display specification for subscript and superscripts.
The car is used for subscript, the cdr is used for superscripts."
  :group 'ein:markdown
  :type '(cons (choice (sexp :tag "Subscript form")
                       (const :tag "No lowering" nil))
               (choice (sexp :tag "Superscript form")
                       (const :tag "No raising" nil)))
  :package-version '(ein:markdown-mode . "2.4"))

(defcustom ein:markdown-unordered-list-item-prefix "  * "
  "String inserted before unordered list items."
  :group 'ein:markdown
  :type 'string)

(defcustom ein:markdown-nested-imenu-heading-index t
  "Use nested or flat imenu heading index.
A nested index may provide more natural browsing from the menu,
but a flat list may allow for faster keyboard navigation via tab
completion."
  :group 'ein:markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(ein:markdown-mode . "2.2"))

(defcustom ein:markdown-add-footnotes-to-imenu t
  "Add footnotes to end of imenu heading index."
  :group 'ein:markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(ein:markdown-mode . "2.4"))

(defcustom ein:markdown-use-pandoc-style-yaml-metadata nil
  "When non-nil, allow YAML metadata anywhere in the document."
  :group 'ein:markdown
  :type 'boolean)

(defcustom ein:markdown-split-window-direction 'any
  "Preference for splitting windows for static and live preview.
The default value is 'any, which instructs Emacs to use
`split-window-sensibly' to automatically choose how to split
windows based on the values of `split-width-threshold' and
`split-height-threshold' and the available windows.  To force
vertically split (left and right) windows, set this to 'vertical
or 'right.  To force horizontally split (top and bottom) windows,
set this to 'horizontal or 'below."
  :group 'ein:markdown
  :type '(choice (const :tag "Automatic" any)
                 (const :tag "Right (vertical)" right)
                 (const :tag "Below (horizontal)" below))
  :package-version '(ein:markdown-mode . "2.2"))

(defcustom ein:markdown-list-indent-width 4
  "Depth of indentation for ein:markdown lists.
Used in `markdown-demote-list-item' and
`markdown-promote-list-item'."
  :group 'ein:markdown
  :type 'integer)

(defcustom ein:markdown-enable-prefix-prompts t
  "Display prompts for certain prefix commands.
Set to nil to disable these prompts."
  :group 'ein:markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(ein:markdown-mode . "2.3"))

(defcustom ein:markdown-edit-code-block-default-mode 'normal-mode
  "Default mode to use for editing code blocks.
This mode is used when automatic detection fails, such as for GFM
code blocks with no language specified."
  :group 'ein:markdown
  :type '(choice function (const :tag "None" nil))
  :package-version '(ein:markdown-mode . "2.4"))

(defcustom ein:markdown-translate-filename-function #'identity
  "Function to use to translate filenames when following links.
\\<ein:markdown-mode-map>\\[ein:markdown-follow-thing-at-point] and \\[ein:markdown-follow-link-at-point]
call this function with the filename as only argument whenever
they encounter a filename (instead of a URL) to be visited and
use its return value instead of the filename in the link.  For
example, if absolute filenames are actually relative to a server
root directory, you can set
`markdown-translate-filename-function' to a function that
prepends the root directory to the given filename."
  :group 'ein:markdown
  :type 'function
  :risky t
  :package-version '(ein:markdown-mode . "2.4"))

(defcustom ein:markdown-max-image-size nil
  "Maximum width and height for displayed inline images.
This variable may be nil or a cons cell (MAX-WIDTH . MAX-HEIGHT).
When nil, use the actual size.  Otherwise, use ImageMagick to
resize larger images to be of the given maximum dimensions.  This
requires Emacs to be built with ImageMagick support."
  :group 'ein:markdown
  :package-version '(ein:markdown-mode . "2.4")
  :type '(choice
          (const :tag "Use actual image width" nil)
          (cons (choice (sexp :tag "Maximum width in pixels")
                        (const :tag "No maximum width" nil))
                (choice (sexp :tag "Maximum height in pixels")
                        (const :tag "No maximum height" nil)))))


;;; Markdown-Specific `rx' Macro ==============================================

;; Based on python-rx from python.el.
(eval-and-compile
  (defconst ein:markdown-rx-constituents
    `((newline . ,(rx "\n"))
      (indent . ,(rx (or (repeat 4 " ") "\t")))
      (block-end . ,(rx (and (or (one-or-more (zero-or-more blank) "\n") line-end))))
      (numeral . ,(rx (and (one-or-more (any "0-9#")) ".")))
      (bullet . ,(rx (any "*+:-")))
      (list-marker . ,(rx (or (and (one-or-more (any "0-9#")) ".")
                              (any "*+:-"))))
      (checkbox . ,(rx "[" (any " xX") "]")))
    "ein:markdown-specific sexps for `markdown-rx'")

  (defun ein:markdown-rx-to-string (form &optional no-group)
    "ein:markdown mode specialized `rx-to-string' function.
This variant supports named Markdown expressions in FORM.
NO-GROUP non-nil means don't put shy groups around the result."
    (let ((rx-constituents (append ein:markdown-rx-constituents rx-constituents)))
      (rx-to-string form no-group)))

  (defmacro ein:markdown-rx (&rest regexps)
    "ein:markdown mode specialized rx macro.
This variant of `rx' supports common Markdown named REGEXPS."
    (cond ((null regexps)
           (error "No regexp"))
          ((cdr regexps)
           (ein:markdown-rx-to-string `(and ,@regexps) t))
          (t
           (ein:markdown-rx-to-string (car regexps) t)))))


;;; Regular Expressions =======================================================

(defconst ein:markdown-regex-comment-start
  "<!--"
  "Regular expression matches HTML comment opening.")

(defconst ein:markdown-regex-comment-end
  "--[ \t]*>"
  "Regular expression matches HTML comment closing.")

(defconst ein:markdown-regex-link-inline
  "\\(!\\)?\\(\\[\\)\\([^]^][^]]*\\|\\)\\(\\]\\)\\((\\)\\([^)]*?\\)\\(?:\\s-+\\(\"[^\"]*\"\\)\\)?\\()\\)"
  "Regular expression for a [text](file) or an image link ![text](file).
Group 1 matches the leading exclamation point (optional).
Group 2 matches the opening square bracket.
Group 3 matches the text inside the square brackets.
Group 4 matches the closing square bracket.
Group 5 matches the opening parenthesis.
Group 6 matches the URL.
Group 7 matches the title (optional).
Group 8 matches the closing parenthesis.")

(defconst ein:markdown-regex-link-reference
  "\\(!\\)?\\(\\[\\)\\([^]^][^]]*\\|\\)\\(\\]\\)[ ]?\\(\\[\\)\\([^]]*?\\)\\(\\]\\)"
  "Regular expression for a reference link [text][id].
Group 1 matches the leading exclamation point (optional).
Group 2 matches the opening square bracket for the link text.
Group 3 matches the text inside the square brackets.
Group 4 matches the closing square bracket for the link text.
Group 5 matches the opening square bracket for the reference label.
Group 6 matches the reference label.
Group 7 matches the closing square bracket for the reference label.")

(defconst ein:markdown-regex-reference-definition
  "^ \\{0,3\\}\\(\\[\\)\\([^]\n]+?\\)\\(\\]\\)\\(:\\)\\s *\\(.*?\\)\\s *\\( \"[^\"]*\"$\\|$\\)"
  "Regular expression for a reference definition.
Group 1 matches the opening square bracket.
Group 2 matches the reference label.
Group 3 matches the closing square bracket.
Group 4 matches the colon.
Group 5 matches the URL.
Group 6 matches the title attribute (optional).")

(defconst ein:markdown-regex-footnote
  "\\(\\[\\^\\)\\(.+?\\)\\(\\]\\)"
  "Regular expression for a footnote marker [^fn].
Group 1 matches the opening square bracket and carat.
Group 2 matches only the label, without the surrounding markup.
Group 3 matches the closing square bracket.")

(defconst ein:markdown-regex-header
  "^\\(?:\\([^\r\n\t -].*\\)\n\\(?:\\(=+\\)\\|\\(-+\\)\\)\\|\\(#+[ \t]+\\)\\(.*?\\)\\([ \t]*#*\\)\\)$"
  "Regexp identifying Markdown headings.
Group 1 matches the text of a setext heading.
Group 2 matches the underline of a level-1 setext heading.
Group 3 matches the underline of a level-2 setext heading.
Group 4 matches the opening hash marks of an atx heading and whitespace.
Group 5 matches the text, without surrounding whitespace, of an atx heading.
Group 6 matches the closing whitespace and hash marks of an atx heading.")

(defconst ein:markdown-regex-header-setext
  "^\\([^\r\n\t -].*\\)\n\\(=+\\|-+\\)$"
  "Regular expression for generic setext-style (underline) headers.")

(defconst ein:markdown-regex-header-atx
  "^\\(#+\\)[ \t]+\\(.*?\\)[ \t]*\\(#*\\)$"
  "Regular expression for generic atx-style (hash mark) headers.")

(defconst ein:markdown-regex-hr
  (rx line-start
      (group (or (and (repeat 3 (and "*" (? " "))) (* (any "* ")))
                 (and (repeat 3 (and "-" (? " "))) (* (any "- ")))
                 (and (repeat 3 (and "_" (? " "))) (* (any "_ ")))))
      line-end)
  "Regular expression for matching ein:markdown horizontal rules.")

(defconst ein:markdown-regex-code
  "\\(?:\\`\\|[^\\]\\)\\(\\(`+\\)\\(\\(?:.\\|\n[^\n]\\)*?[^`]\\)\\(\\2\\)\\)\\(?:[^`]\\|\\'\\)"
  "Regular expression for matching inline code fragments.

Group 1 matches the entire code fragment including the backquotes.
Group 2 matches the opening backquotes.
Group 3 matches the code fragment itself, without backquotes.
Group 4 matches the closing backquotes.

The leading, unnumbered group ensures that the leading backquote
character is not escaped.
The last group, also unnumbered, requires that the character
following the code fragment is not a backquote.
Note that \\(?:.\\|\n[^\n]\\) matches any character, including newlines,
but not two newlines in a row.")

(defconst ein:markdown-regex-kbd
  "\\(<kbd>\\)\\(\\(?:.\\|\n[^\n]\\)*?\\)\\(</kbd>\\)"
  "Regular expression for matching <kbd> tags.
Groups 1 and 3 match the opening and closing tags.
Group 2 matches the key sequence.")

(defconst ein:markdown-regex-pre
  "^\\(    \\|\t\\).*$"
  "Regular expression for matching preformatted text sections.")

(defconst ein:markdown-regex-list
  (ein:markdown-rx line-start
               ;; 1. Leading whitespace
               (group (* blank))
               ;; 2. List marker: a numeral, bullet, or colon
               (group list-marker)
               ;; 3. Trailing whitespace
               (group (+ blank))
               ;; 4. Optional checkbox for GFM task list items
               (opt (group (and checkbox (* blank)))))
  "Regular expression for matching list items.")

(defconst ein:markdown-regex-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\([^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\3\\)\\)"
  "Regular expression for matching bold text.
Group 1 matches the character before the opening asterisk or
underscore, if any, ensuring that it is not a backslash escape.
Group 2 matches the entire expression, including delimiters.
Groups 3 and 5 matches the opening and closing delimiters.
Group 4 matches the text inside the delimiters.")

(defconst ein:markdown-regex-italic
  "\\(?:^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \n\t\\]\\|[^ \n\t*]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\2\\)\\)"
  "Regular expression for matching italic text.
The leading unnumbered matches the character before the opening
asterisk or underscore, if any, ensuring that it is not a
backslash escape.
Group 1 matches the entire expression, including delimiters.
Groups 2 and 4 matches the opening and closing delimiters.
Group 3 matches the text inside the delimiters.")

(defconst ein:markdown-regex-strike-through
  "\\(^\\|[^\\]\\)\\(\\(~~\\)\\([^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(~~\\)\\)"
  "Regular expression for matching strike-through text.
Group 1 matches the character before the opening tilde, if any,
ensuring that it is not a backslash escape.
Group 2 matches the entire expression, including delimiters.
Groups 3 and 5 matches the opening and closing delimiters.
Group 4 matches the text inside the delimiters.")

(defconst ein:markdown-regex-blockquote
  "^[ \t]*\\([A-Z]?>\\)\\([ \t]*\\)\\(.*\\)$"
  "Regular expression for matching blockquote lines.
Also accounts for a potential capital letter preceding the angle
bracket, for use with Leanpub blocks (asides, warnings, info
blocks, etc.).
Group 1 matches the leading angle bracket.
Group 2 matches the separating whitespace.
Group 3 matches the text.")

(defconst ein:markdown-regex-line-break
  "[^ \n\t][ \t]*\\(  \\)$"
  "Regular expression for matching line breaks.")

(defconst ein:markdown-regex-uri
  (concat "\\(" (regexp-opt ein:markdown-uri-types) ":[^]\t\n\r<>,;() ]+\\)")
  "Regular expression for matching inline URIs.")

(defconst ein:markdown-regex-angle-uri
  (concat "\\(<\\)\\(" (regexp-opt ein:markdown-uri-types) ":[^]\t\n\r<>,;()]+\\)\\(>\\)")
  "Regular expression for matching inline URIs in angle brackets.")

(defconst ein:markdown-regex-email
  "<\\(\\(?:\\sw\\|\\s_\\|\\s.\\)+@\\(?:\\sw\\|\\s_\\|\\s.\\)+\\)>"
  "Regular expression for matching inline email addresses.")

(defsubst ein:markdown-make-regex-link-generic ()
  "Make regular expression for matching any recognized link."
  (concat "\\(?:" ein:markdown-regex-link-inline
          "\\|" ein:markdown-regex-link-reference
          "\\|" ein:markdown-regex-angle-uri "\\)"))

(defconst ein:markdown-regex-blank-line
  "^[[:blank:]]*$"
  "Regular expression that matches a blank line.")

(defconst ein:markdown-regex-block-separator
  "\n[\n\t\f ]*\n"
  "Regular expression for matching block boundaries.")

(defconst ein:markdown-regex-block-separator-noindent
  (concat "\\(\\`\\|\\(" ein:markdown-regex-block-separator "\\)[^\n\t\f ]\\)")
  "Regexp for block separators before lines with no indentation.")

(defconst ein:markdown-regex-math-inline-single
  "\\(?:^\\|[^\\]\\)\\(\\$\\)\\(\\(?:[^\\$]\\|\\\\.\\)*\\)\\(\\$\\)"
  "Regular expression for itex $..$ math mode expressions.
Groups 1 and 3 match the opening and closing dollar signs.
Group 2 matches the mathematical expression contained within.")

(defconst ein:markdown-regex-math-inline-double
  "\\(?:^\\|[^\\]\\)\\(\\$\\$\\)\\(\\(?:[^\\$]\\|\\\\.\\)*\\)\\(\\$\\$\\)"
  "Regular expression for itex $$..$$ math mode expressions.
Groups 1 and 3 match opening and closing dollar signs.
Group 2 matches the mathematical expression contained within.")

(defconst ein:markdown-regex-math-display
  (rx line-start (* blank)
      (group (group (repeat 1 2 "\\")) "[")
      (group (*? anything))
      (group (backref 2) "]")
      line-end)
  "Regular expression for \[..\] or \\[..\\] display math.
Groups 1 and 4 match the opening and closing markup.
Group 3 matches the mathematical expression contained within.
Group 2 matches the opening slashes, and is used internally to
match the closing slashes.")

(defsubst ein:markdown-make-tilde-fence-regex (num-tildes &optional end-of-line)
  "Return regexp matching a tilde code fence at least NUM-TILDES long.
END-OF-LINE is the regexp construct to indicate end of line; $ if
missing."
  (format "%s%d%s%s" "^[[:blank:]]*\\([~]\\{" num-tildes ",\\}\\)"
          (or end-of-line "$")))

(defconst ein:markdown-regex-tilde-fence-begin
  (ein:markdown-make-tilde-fence-regex
   3 "\\([[:blank:]]*{?\\)[[:blank:]]*\\([^[:space:]]+?\\)?\\(?:[[:blank:]]+\\(.+?\\)\\)?\\([[:blank:]]*}?[[:blank:]]*\\)$")
  "Regular expression for matching tilde-fenced code blocks.
Group 1 matches the opening tildes.
Group 2 matches (optional) opening brace and surrounding whitespace.
Group 3 matches the language identifier (optional).
Group 4 matches the info string (optional).
Group 5 matches the closing brace (optional) and any surrounding whitespace.
Groups need to agree with `markdown-regex-gfm-code-block-open'.")

(defconst ein:markdown-regex-declarative-metadata
  "^\\([[:alpha:]][[:alpha:] _-]*?\\)\\([:=][ \t]*\\)\\(.*\\)$"
  "Regular expression for matching declarative metadata statements.
This matches MultiMarkdown metadata as well as YAML and TOML
assignments such as the following:

    variable: value

or

    variable = value")

(defconst ein:markdown-regex-pandoc-metadata
  "^\\(%\\)\\([ \t]*\\)\\(.*\\(?:\n[ \t]+.*\\)*\\)"
  "Regular expression for matching Pandoc metadata.")

(defconst ein:markdown-regex-yaml-metadata-border
  "\\(-\\{3\\}\\)$"
  "Regular expression for matching YAML metadata.")

(defconst ein:markdown-regex-yaml-pandoc-metadata-end-border
  "^\\(\\.\\{3\\}\\|\\-\\{3\\}\\)$"
  "Regular expression for matching YAML metadata end borders.")

(defsubst ein:markdown-get-yaml-metadata-start-border ()
  "Return YAML metadata start border depending upon whether Pandoc is used."
  (concat
   (if ein:markdown-use-pandoc-style-yaml-metadata "^" "\\`")
   ein:markdown-regex-yaml-metadata-border))

(defsubst ein:markdown-get-yaml-metadata-end-border (_)
  "Return YAML metadata end border depending upon whether Pandoc is used."
  (if ein:markdown-use-pandoc-style-yaml-metadata
      ein:markdown-regex-yaml-pandoc-metadata-end-border
    ein:markdown-regex-yaml-metadata-border))

(defconst ein:markdown-regex-inline-attributes
  "[ \t]*\\({:?\\)[ \t]*\\(\\(#[[:alpha:]_.:-]+\\|\\.[[:alpha:]_.:-]+\\|\\w+=['\"]?[^\n'\"]*['\"]?\\),?[ \t]*\\)+\\(}\\)[ \t]*$"
  "Regular expression for matching inline identifiers or attribute lists.
Compatible with Pandoc, Python ein:markdown, PHP Markdown Extra, and Leanpub.")

(defconst ein:markdown-regex-leanpub-sections
  (concat
   "^\\({\\)\\("
   (regexp-opt '("frontmatter" "mainmatter" "backmatter" "appendix" "pagebreak"))
   "\\)\\(}\\)[ \t]*\n")
  "Regular expression for Leanpub section markers and related syntax.")

(defconst ein:markdown-regex-sub-superscript
  "\\(?:^\\|[^\\~^]\\)\\(\\([~^]\\)\\([[:alnum:]]+\\)\\(\\2\\)\\)"
  "The regular expression matching a sub- or superscript.
The leading un-numbered group matches the character before the
opening tilde or carat, if any, ensuring that it is not a
backslash escape, carat, or tilde.
Group 1 matches the entire expression, including markup.
Group 2 matches the opening markup--a tilde or carat.
Group 3 matches the text inside the delimiters.
Group 4 matches the closing markup--a tilde or carat.")

(defconst ein:markdown-regex-include
  "^\\(<<\\)\\(?:\\(\\[\\)\\(.*\\)\\(\\]\\)\\)?\\(?:\\((\\)\\(.*\\)\\()\\)\\)?\\(?:\\({\\)\\(.*\\)\\(}\\)\\)?$"
  "Regular expression matching common forms of include syntax.
Marked 2, Leanpub, and other processors support some of these forms:

<<[sections/section1.md]
<<(folder/filename)
<<[Code title](folder/filename)
<<{folder/raw_file.html}

Group 1 matches the opening two angle brackets.
Groups 2-4 match the opening square bracket, the text inside,
and the closing square bracket, respectively.
Groups 5-7 match the opening parenthesis, the text inside, and
the closing parenthesis.
Groups 8-10 match the opening brace, the text inside, and the brace.")

(defconst ein:markdown-regex-pandoc-inline-footnote
  "\\(\\^\\)\\(\\[\\)\\(\\(?:.\\|\n[^\n]\\)*?\\)\\(\\]\\)"
  "Regular expression for Pandoc inline footnote^[footnote text].
Group 1 matches the opening caret.
Group 2 matches the opening square bracket.
Group 3 matches the footnote text, without the surrounding markup.
Group 4 matches the closing square bracket.")

(defconst ein:markdown-regex-html-attr
  "\\(\\<[[:alpha:]:-]+\\>\\)\\(\\s-*\\(=\\)\\s-*\\(\".*?\"\\|'.*?'\\|[^'\">[:space:]]+\\)?\\)?"
  "Regular expression for matching HTML attributes and values.
Group 1 matches the attribute name.
Group 2 matches the following whitespace, equals sign, and value, if any.
Group 3 matches the equals sign, if any.
Group 4 matches single-, double-, or un-quoted attribute values.")

(defconst ein:markdown-regex-html-tag
  (concat "\\(</?\\)\\(\\w+\\)\\(\\(\\s-+" ein:markdown-regex-html-attr
          "\\)+\\s-*\\|\\s-*\\)\\(/?>\\)")
  "Regular expression for matching HTML tags.
Groups 1 and 9 match the beginning and ending angle brackets and slashes.
Group 2 matches the tag name.
Group 3 matches all attributes and whitespace following the tag name.")

(defconst ein:markdown-regex-html-entity
  "\\(&#?[[:alnum:]]+;\\)"
  "Regular expression for matching HTML entities.")


;;; Syntax ====================================================================

(defvar ein:markdown--syntax-properties
  (list 'ein:markdown-tilde-fence-begin nil
        'ein:markdown-tilde-fence-end nil
        'ein:markdown-fenced-code nil
        'ein:markdown-yaml-metadata-begin nil
        'ein:markdown-yaml-metadata-end nil
        'ein:markdown-yaml-metadata-section nil
        'ein:markdown-list-item nil
        'ein:markdown-pre nil
        'ein:markdown-blockquote nil
        'ein:markdown-hr nil
        'ein:markdown-comment nil
        'ein:markdown-heading nil
        'ein:markdown-heading-1-setext nil
        'ein:markdown-heading-2-setext nil
        'ein:markdown-heading-1-atx nil
        'ein:markdown-heading-2-atx nil
        'ein:markdown-heading-3-atx nil
        'ein:markdown-heading-4-atx nil
        'ein:markdown-heading-5-atx nil
        'ein:markdown-heading-6-atx nil
        'ein:markdown-metadata-key nil
        'ein:markdown-metadata-value nil
        'ein:markdown-metadata-markup nil)
  "Property list of all Markdown syntactic properties.")

(defsubst ein:markdown-in-comment-p (&optional pos)
  "Return non-nil if POS is in a comment.
If POS is not given, use point instead."
  (get-text-property (or pos (point)) 'ein:markdown-comment))

(defun ein:markdown--cur-list-item-bounds ()
  "Return a list describing the list item at point.
Assumes that match data is set for `markdown-regex-list'.  See the
documentation for `markdown-cur-list-item-bounds' for the format of
the returned list."
  (save-excursion
    (let* ((begin (match-beginning 0))
           (indent (length (match-string-no-properties 1)))
           (nonlist-indent (- (match-end 3) (match-beginning 0)))
           (marker (buffer-substring-no-properties
                    (match-beginning 2) (match-end 3)))
           (checkbox (match-string-no-properties 4))
           (match (butlast (match-data t)))
           (end (ein:markdown-cur-list-item-end nonlist-indent)))
      (list begin end indent nonlist-indent marker checkbox match))))

(defun ein:markdown--append-list-item-bounds (marker indent cur-bounds bounds)
  "Update list item BOUNDS given list MARKER, block INDENT, and CUR-BOUNDS.
Here, MARKER is a string representing the type of list and INDENT
is an integer giving the indentation, in spaces, of the current
block.  CUR-BOUNDS is a list of the form returned by
`markdown-cur-list-item-bounds' and BOUNDS is a list of bounds
values for parent list items.  When BOUNDS is nil, it means we are
at baseline (not inside of a nested list)."
  (let ((prev-indent (or (cl-third (car bounds)) 0)))
    (cond
     ;; New list item at baseline.
     ((and marker (null bounds))
      (list cur-bounds))
     ;; List item with greater indentation (four or more spaces).
     ;; Increase list level by consing CUR-BOUNDS onto BOUNDS.
     ((and marker (>= indent (+ prev-indent 4)))
      (cons cur-bounds bounds))
     ;; List item with greater or equal indentation (less than four spaces).
     ;; Keep list level the same by replacing the car of BOUNDS.
     ((and marker (>= indent prev-indent))
      (cons cur-bounds (cdr bounds)))
     ;; Lesser indentation level.
     ;; Pop appropriate number of elements off BOUNDS list (e.g., lesser
     ;; indentation could move back more than one list level).  Note
     ;; that this block need not be the beginning of list item.
     ((< indent prev-indent)
      (while (and (> (length bounds) 1)
                  (setq prev-indent (cl-third (cadr bounds)))
                  (< indent (+ prev-indent 4)))
        (setq bounds (cdr bounds)))
      (cons cur-bounds bounds))
     ;; Otherwise, do nothing.
     (t bounds))))

(defun ein:markdown-syntax-propertize-list-items (start end)
  "Propertize list items from START to END.
Stores nested list item information in the `markdown-list-item'
text property to make later syntax analysis easier.  The value of
this property is a list with elements of the form (begin . end)
giving the bounds of the current and parent list items."
  (save-excursion
    (goto-char start)
    (let (bounds level pre-regexp)
      ;; Find a baseline point with zero list indentation
      (ein:markdown-search-backward-baseline)
      ;; Search for all list items between baseline and END
      (while (and (< (point) end)
                  (re-search-forward ein:markdown-regex-list end 'limit))
        ;; Level of list nesting
        (setq level (length bounds))
        ;; Pre blocks need to be indented one level past the list level
        (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" (1+ level)))
        (beginning-of-line)
        (cond
         ;; Reset at headings, horizontal rules, and top-level blank lines.
         ;; Propertize baseline when in range.
         ((ein:markdown-new-baseline)
          (setq bounds nil))
         ;; Make sure this is not a line from a pre block
         ((looking-at-p pre-regexp))
         ;; If not, then update levels and propertize list item when in range.
         (t
          (let* ((indent (current-indentation))
                 (cur-bounds (ein:markdown--cur-list-item-bounds))
                 (first (cl-first cur-bounds))
                 (last (cl-second cur-bounds))
                 (marker (cl-fifth cur-bounds)))
            (setq bounds (ein:markdown--append-list-item-bounds
                          marker indent cur-bounds bounds))
          (when (and (<= start (point)) (<= (point) end))
            (put-text-property first last 'ein:markdown-list-item bounds)))))
        (end-of-line)))))

(defun ein:markdown-syntax-propertize-pre-blocks (start end)
  "Match preformatted text blocks from START to END."
  (save-excursion
    (goto-char start)
    (let ((levels (ein:markdown-calculate-list-levels))
          indent pre-regexp close-regexp open close)
      (while (and (< (point) end) (not close))
        ;; Search for a region with sufficient indentation
        (if (null levels)
            (setq indent 1)
          (setq indent (1+ (length levels))))
        (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" indent))
        (setq close-regexp (format "^\\(    \\|\t\\)\\{0,%d\\}\\([^ \t]\\)" (1- indent)))

        (cond
         ;; If not at the beginning of a line, move forward
         ((not (bolp)) (forward-line))
         ;; Move past blank lines
         ((ein:markdown-cur-line-blank-p) (forward-line))
         ;; At headers and horizontal rules, reset levels
         ((ein:markdown-new-baseline) (forward-line) (setq levels nil))
         ;; If the current line has sufficient indentation, mark out pre block
         ;; The opening should be preceded by a blank line.
         ((and (ein:markdown-prev-line-blank) (looking-at pre-regexp))
          (setq open (match-beginning 0))
          (while (and (or (looking-at-p pre-regexp) (ein:markdown-cur-line-blank-p))
                      (not (eobp)))
            (forward-line))
          (skip-syntax-backward "-")
          (setq close (point)))
         ;; If current line has a list marker, update levels, move to end of block
         ((looking-at ein:markdown-regex-list)
          (setq levels (ein:markdown-update-list-levels
                        (match-string 2) (current-indentation) levels))
          (ein:markdown-end-of-text-block))
         ;; If this is the end of the indentation level, adjust levels accordingly.
         ;; Only match end of indentation level if levels is not the empty list.
         ((and (car levels) (looking-at-p close-regexp))
          (setq levels (ein:markdown-update-list-levels
                        nil (current-indentation) levels))
          (ein:markdown-end-of-text-block))
         (t (ein:markdown-end-of-text-block))))

      (when (and open close)
        ;; Set text property data
        (put-text-property open close 'ein:markdown-pre (list open close))
        ;; Recursively search again
        (ein:markdown-syntax-propertize-pre-blocks (point) end)))))

(defconst ein:markdown-fenced-block-pairs
  `(((,ein:markdown-regex-tilde-fence-begin ein:markdown-tilde-fence-begin)
     (ein:markdown-make-tilde-fence-regex ein:markdown-tilde-fence-end)
     ein:markdown-fenced-code)
    ((ein:markdown-get-yaml-metadata-start-border ein:markdown-yaml-metadata-begin)
     (ein:markdown-get-yaml-metadata-end-border ein:markdown-yaml-metadata-end)
     ein:markdown-yaml-metadata-section))
  "Mapping of regular expressions to \"fenced-block\" constructs.
These constructs are distinguished by having a distinctive start
and end pattern, both of which take up an entire line of text,
but no special pattern to identify text within the fenced
blocks (unlike blockquotes and indented-code sections).

Each element within this list takes the form:

  ((START-REGEX-OR-FUN START-PROPERTY)
   (END-REGEX-OR-FUN END-PROPERTY)
   MIDDLE-PROPERTY)

Each *-REGEX-OR-FUN element can be a regular expression as a string, or a
function which evaluates to same. Functions for START-REGEX-OR-FUN accept no
arguments, but functions for END-REGEX-OR-FUN accept a single numerical argument
which is the length of the first group of the START-REGEX-OR-FUN match, which
can be ignored if unnecessary. `markdown-maybe-funcall-regexp' is used to
evaluate these into \"real\" regexps.

The *-PROPERTY elements are the text properties applied to each part of the
block construct when it is matched using
`markdown-syntax-propertize-fenced-block-constructs'. START-PROPERTY is applied
to the text matching START-REGEX-OR-FUN, END-PROPERTY to END-REGEX-OR-FUN, and
MIDDLE-PROPERTY to the text in between the two. The value of *-PROPERTY is the
`match-data' when the regexp was matched to the text. In the case of
MIDDLE-PROPERTY, the value is a false match data of the form '(begin end), with
begin and end set to the edges of the \"middle\" text. This makes fontification
easier.")

(defun ein:markdown-text-property-at-point (prop)
  (get-text-property (point) prop))

(defsubst ein:markdown-maybe-funcall-regexp (object &optional arg)
  (cond ((functionp object)
         (if arg (funcall object arg) (funcall object)))
        ((stringp object) object)
        (t (error "Object cannot be turned into regex"))))

(defsubst ein:markdown-get-start-fence-regexp ()
  "Return regexp to find all \"start\" sections of fenced block constructs.
Which construct is actually contained in the match must be found separately."
  (mapconcat
   #'identity
   (mapcar (lambda (entry) (ein:markdown-maybe-funcall-regexp (caar entry)))
           ein:markdown-fenced-block-pairs)
   "\\|"))

(defun ein:markdown-get-fenced-block-begin-properties ()
  (cl-mapcar (lambda (entry) (cl-cadar entry)) ein:markdown-fenced-block-pairs))

(defun ein:markdown-get-fenced-block-end-properties ()
  (cl-mapcar (lambda (entry) (cl-cadadr entry)) ein:markdown-fenced-block-pairs))

(defun ein:markdown-get-fenced-block-middle-properties ()
  (cl-mapcar #'cl-third ein:markdown-fenced-block-pairs))

(defun ein:markdown-find-previous-prop (prop &optional lim)
  "Find previous place where property PROP is non-nil, up to LIM.
Return a cons of (pos . property). pos is point if point contains
non-nil PROP."
  (let ((res
         (if (get-text-property (point) prop) (point)
           (previous-single-property-change
            (point) prop nil (or lim (point-min))))))
    (when (and (not (get-text-property res prop))
               (> res (point-min))
               (get-text-property (1- res) prop))
      (cl-decf res))
    (when (and res (get-text-property res prop)) (cons res prop))))

(defun ein:markdown-find-next-prop (prop &optional lim)
  "Find next place where property PROP is non-nil, up to LIM.
Return a cons of (POS . PROPERTY) where POS is point if point
contains non-nil PROP."
  (let ((res
         (if (get-text-property (point) prop) (point)
           (next-single-property-change
            (point) prop nil (or lim (point-max))))))
    (when (and res (get-text-property res prop)) (cons res prop))))

(defun ein:markdown-min-of-seq (map-fn seq)
  "Apply MAP-FN to SEQ and return element of SEQ with minimum value of MAP-FN."
  (cl-loop for el in seq
           with min = 1.0e+INF          ; infinity
           with min-el = nil
           do (let ((res (funcall map-fn el)))
                (when (< res min)
                  (setq min res)
                  (setq min-el el)))
           finally return min-el))

(defun ein:markdown-max-of-seq (map-fn seq)
  "Apply MAP-FN to SEQ and return element of SEQ with maximum value of MAP-FN."
  (cl-loop for el in seq
           with max = -1.0e+INF          ; negative infinity
           with max-el = nil
           do (let ((res (funcall map-fn el)))
                (when (and res (> res max))
                  (setq max res)
                  (setq max-el el)))
           finally return max-el))

(defun ein:markdown-find-previous-block ()
  "Find previous block.
Detect whether `markdown-syntax-propertize-fenced-block-constructs' was
unable to propertize the entire block, but was able to propertize the beginning
of the block. If so, return a cons of (pos . property) where the beginning of
the block was propertized."
  (let ((start-pt (point))
        (closest-open
         (ein:markdown-max-of-seq
          #'car
          (cl-remove-if
           #'null
           (cl-mapcar
            #'ein:markdown-find-previous-prop
            (ein:markdown-get-fenced-block-begin-properties))))))
    (when closest-open
      (let* ((length-of-open-match
              (let ((match-d
                     (get-text-property (car closest-open) (cdr closest-open))))
                (- (cl-fourth match-d) (cl-third match-d))))
             (end-regexp
              (ein:markdown-maybe-funcall-regexp
               (cl-caadr
                (cl-find-if
                 (lambda (entry) (eq (cl-cadar entry) (cdr closest-open)))
                 ein:markdown-fenced-block-pairs))
               length-of-open-match))
             (end-prop-loc
              (save-excursion
                (save-match-data
                  (goto-char (car closest-open))
                  (and (re-search-forward end-regexp start-pt t)
                       (match-beginning 0))))))
        (and (not end-prop-loc) closest-open)))))

(defun ein:markdown-get-fenced-block-from-start (prop)
  "Return limits of an enclosing fenced block from its start, using PROP.
Return value is a list usable as `match-data'."
  (catch 'no-rest-of-block
    (let* ((correct-entry
            (cl-find-if
             (lambda (entry) (eq (cl-cadar entry) prop))
             ein:markdown-fenced-block-pairs))
           (begin-of-begin (cl-first (ein:markdown-text-property-at-point prop)))
           (middle-prop (cl-third correct-entry))
           (end-prop (cl-cadadr correct-entry))
           (end-of-end
            (save-excursion
              (goto-char (match-end 0))   ; end of begin
              (unless (eobp) (forward-char))
              (let ((mid-prop-v (ein:markdown-text-property-at-point middle-prop)))
                (if (not mid-prop-v)    ; no middle
                    (progn
                      ;; try to find end by advancing one
                      (let ((end-prop-v
                             (ein:markdown-text-property-at-point end-prop)))
                        (if end-prop-v (cl-second end-prop-v)
                          (throw 'no-rest-of-block nil))))
                  (set-match-data mid-prop-v)
                  (goto-char (match-end 0))   ; end of middle
                  (beginning-of-line)         ; into end
                  (cl-second (ein:markdown-text-property-at-point end-prop)))))))
      (list begin-of-begin end-of-end))))

(defun ein:markdown-get-fenced-block-from-middle (prop)
  "Return limits of an enclosing fenced block from its middle, using PROP.
Return value is a list usable as `match-data'."
  (let* ((correct-entry
          (cl-find-if
           (lambda (entry) (eq (cl-third entry) prop))
           ein:markdown-fenced-block-pairs))
         (begin-prop (cl-cadar correct-entry))
         (begin-of-begin
          (save-excursion
            (goto-char (match-beginning 0))
            (unless (bobp) (forward-line -1))
            (beginning-of-line)
            (cl-first (ein:markdown-text-property-at-point begin-prop))))
         (end-prop (cl-cadadr correct-entry))
         (end-of-end
          (save-excursion
            (goto-char (match-end 0))
            (beginning-of-line)
            (cl-second (ein:markdown-text-property-at-point end-prop)))))
    (list begin-of-begin end-of-end)))

(defun ein:markdown-get-fenced-block-from-end (prop)
  "Return limits of an enclosing fenced block from its end, using PROP.
Return value is a list usable as `match-data'."
  (let* ((correct-entry
          (cl-find-if
           (lambda (entry) (eq (cl-cadadr entry) prop))
           ein:markdown-fenced-block-pairs))
         (end-of-end (cl-second (ein:markdown-text-property-at-point prop)))
         (middle-prop (cl-third correct-entry))
         (begin-prop (cl-cadar correct-entry))
         (begin-of-begin
          (save-excursion
            (goto-char (match-beginning 0)) ; beginning of end
            (unless (bobp) (backward-char)) ; into middle
            (let ((mid-prop-v (ein:markdown-text-property-at-point middle-prop)))
              (if (not mid-prop-v)
                  (progn
                    (beginning-of-line)
                    (cl-first (ein:markdown-text-property-at-point begin-prop)))
                (set-match-data mid-prop-v)
                (goto-char (match-beginning 0))   ; beginning of middle
                (unless (bobp) (forward-line -1)) ; into beginning
                (beginning-of-line)
                (cl-first (ein:markdown-text-property-at-point begin-prop)))))))
    (list begin-of-begin end-of-end)))

(defun ein:markdown-get-enclosing-fenced-block-construct (&optional pos)
  "Get \"fake\" match data for block enclosing POS.
Returns fake match data which encloses the start, middle, and end
of the block construct enclosing POS, if it exists. Used in
`markdown-code-block-at-pos'."
  (save-excursion
    (when pos (goto-char pos))
    (beginning-of-line)
    (car
     (cl-remove-if
      #'null
      (cl-mapcar
       (lambda (fun-and-prop)
         (cl-destructuring-bind (fun prop) fun-and-prop
           (when prop
             (save-match-data
               (set-match-data (ein:markdown-text-property-at-point prop))
               (funcall fun prop)))))
       `((ein:markdown-get-fenced-block-from-start
          ,(cl-find-if
            #'ein:markdown-text-property-at-point
            (ein:markdown-get-fenced-block-begin-properties)))
         (ein:markdown-get-fenced-block-from-middle
          ,(cl-find-if
            #'ein:markdown-text-property-at-point
            (ein:markdown-get-fenced-block-middle-properties)))
         (ein:markdown-get-fenced-block-from-end
          ,(cl-find-if
            #'ein:markdown-text-property-at-point
            (ein:markdown-get-fenced-block-end-properties)))))))))

(defun ein:markdown-propertize-end-match (reg end fence-spec middle-begin)
  "Get match for REG up to END, if exists, and propertize appropriately.
FENCE-SPEC is an entry in `markdown-fenced-block-pairs' and
MIDDLE-BEGIN is the start of the \"middle\" section of the block."
  (when (re-search-forward reg end t)
    (let ((close-begin (match-beginning 0)) ; Start of closing line.
          (close-end (match-end 0))         ; End of closing line.
          (close-data (match-data t)))      ; Match data for closing line.
      ;; Propertize middle section of fenced block.
      (put-text-property middle-begin close-begin
                         (cl-third fence-spec)
                         (list middle-begin close-begin))
      ;; If the block is a YAML block, propertize the declarations inside
      (ein:markdown-syntax-propertize-yaml-metadata middle-begin close-begin)
      ;; Propertize closing line of fenced block.
      (put-text-property close-begin close-end
                         (cl-cadadr fence-spec) close-data))))

(defun ein:markdown-syntax-propertize-fenced-block-constructs (start end)
  "Propertize according to `markdown-fenced-block-pairs' from START to END.
If unable to propertize an entire block (if the start of a block is within START
and END, but the end of the block is not), propertize the start section of a
block, then in a subsequent call propertize both middle and end by finding the
start which was previously propertized."
  (let ((start-reg (ein:markdown-get-start-fence-regexp)))
    (save-excursion
      (goto-char start)
      ;; start from previous unclosed block, if exists
      (let ((prev-begin-block (ein:markdown-find-previous-block)))
        (when prev-begin-block
          (let* ((correct-entry
                  (cl-find-if (lambda (entry)
                                (eq (cdr prev-begin-block) (cl-cadar entry)))
                              ein:markdown-fenced-block-pairs))
                 (enclosed-text-start (1+ (car prev-begin-block)))
                 (start-length
                  (save-excursion
                    (goto-char (car prev-begin-block))
                    (string-match
                     (ein:markdown-maybe-funcall-regexp
                      (caar correct-entry))
                     (buffer-substring
                      (point-at-bol) (point-at-eol)))
                    (- (match-end 1) (match-beginning 1))))
                 (end-reg (ein:markdown-maybe-funcall-regexp
                           (cl-caadr correct-entry) start-length)))
            (ein:markdown-propertize-end-match
             end-reg end correct-entry enclosed-text-start))))
      ;; find all new blocks within region
      (while (re-search-forward start-reg end t)
        ;; we assume the opening constructs take up (only) an entire line,
        ;; so we re-check the current line
        (let* ((cur-line (buffer-substring (point-at-bol) (point-at-eol)))
               ;; find entry in `markdown-fenced-block-pairs' corresponding
               ;; to regex which was matched
               (correct-entry
                (cl-find-if
                 (lambda (fenced-pair)
                   (string-match-p
                    (ein:markdown-maybe-funcall-regexp (caar fenced-pair))
                    cur-line))
                 ein:markdown-fenced-block-pairs))
               (enclosed-text-start
                (save-excursion (1+ (point-at-eol))))
               (end-reg
                (ein:markdown-maybe-funcall-regexp
                 (cl-caadr correct-entry)
                 (if (and (match-beginning 1) (match-end 1))
                     (- (match-end 1) (match-beginning 1))
                   0))))
          ;; get correct match data
          (save-excursion
            (beginning-of-line)
            (re-search-forward
             (ein:markdown-maybe-funcall-regexp (caar correct-entry))
             (point-at-eol)))
          ;; mark starting, even if ending is outside of region
          (put-text-property (match-beginning 0) (match-end 0)
                             (cl-cadar correct-entry) (match-data t))
          (ein:markdown-propertize-end-match
           end-reg end correct-entry enclosed-text-start))))))

(defun ein:markdown-syntax-propertize-blockquotes (start end)
  "Match blockquotes from START to END."
  (save-excursion
    (goto-char start)
    (while (and (re-search-forward ein:markdown-regex-blockquote end t)
                (not (ein:markdown-code-block-at-pos (match-beginning 0))))
      (put-text-property (match-beginning 0) (match-end 0)
                         'ein:markdown-blockquote
                         (match-data t)))))

(defun ein:markdown-syntax-propertize-hrs (start end)
  "Match horizontal rules from START to END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward ein:markdown-regex-hr end t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (goto-char beg)
        (unless (or (ein:markdown-on-heading-p)
                    (ein:markdown-code-block-at-point-p))
          (put-text-property beg end 'ein:markdown-hr (match-data t)))
        (goto-char end)))))

(defun ein:markdown-syntax-propertize-yaml-metadata (start end)
  "Propertize elements inside YAML metadata blocks from START to END.
Assumes region from START and END is already known to be the interior
region of a YAML metadata block as propertized by
`markdown-syntax-propertize-fenced-block-constructs'."
  (save-excursion
    (goto-char start)
    (cl-loop
     while (re-search-forward ein:markdown-regex-declarative-metadata end t)
     do (progn
          (put-text-property (match-beginning 1) (match-end 1)
                             'ein:markdown-metadata-key (match-data t))
          (put-text-property (match-beginning 2) (match-end 2)
                             'ein:markdown-metadata-markup (match-data t))
          (put-text-property (match-beginning 3) (match-end 3)
                             'ein:markdown-metadata-value (match-data t))))))

(defun ein:markdown-syntax-propertize-headings (start end)
  "Match headings of type SYMBOL with REGEX from START to END."
  (goto-char start)
  (while (re-search-forward ein:markdown-regex-header end t)
    (unless (ein:markdown-code-block-at-pos (match-beginning 0))
      (put-text-property
       (match-beginning 0) (match-end 0) 'ein:markdown-heading
       (match-data t))
      (put-text-property
       (match-beginning 0) (match-end 0)
       (cond ((match-string-no-properties 2) 'ein:markdown-heading-1-setext)
             ((match-string-no-properties 3) 'ein:markdown-heading-2-setext)
             (t (let ((atx-level (length (ein:markdown-trim-whitespace
                                          (match-string-no-properties 4)))))
                  (intern (format "ein:markdown-heading-%d-atx" atx-level)))))
       (match-data t)))))

(defun ein:markdown-syntax-propertize-comments (start end)
  "Match HTML comments from the START to END."
  (let* ((in-comment (nth 4 (syntax-ppss)))
         (comment-begin (nth 8 (syntax-ppss))))
    (goto-char start)
    (cond
     ;; Comment start
     ((and (not in-comment)
           (re-search-forward ein:markdown-regex-comment-start end t)
           (not (ein:markdown-inline-code-at-point-p))
           (not (ein:markdown-code-block-at-point-p)))
      (let ((open-beg (match-beginning 0)))
        (put-text-property open-beg (1+ open-beg)
                           'syntax-table (string-to-syntax "<"))
        (ein:markdown-syntax-propertize-comments
         (min (1+ (match-end 0)) end (point-max)) end)))
     ;; Comment end
     ((and in-comment comment-begin
           (re-search-forward ein:markdown-regex-comment-end end t))
      (let ((comment-end (match-end 0)))
        (put-text-property (1- comment-end) comment-end
                           'syntax-table (string-to-syntax ">"))
        ;; Remove any other text properties inside the comment
        (remove-text-properties comment-begin comment-end
                                ein:markdown--syntax-properties)
        (put-text-property comment-begin comment-end
                           'ein:markdown-comment (list comment-begin comment-end))
        (ein:markdown-syntax-propertize-comments
         (min (1+ comment-end) end (point-max)) end)))
     ;; Nothing found
     (t nil))))

(defun ein:markdown-syntax-propertize (start end)
  "Function used as `syntax-propertize-function'.
START and END delimit region to propertize."
  (with-silent-modifications
    (save-excursion
      (remove-text-properties start end ein:markdown--syntax-properties)
      (ein:markdown-syntax-propertize-fenced-block-constructs start end)
      (ein:markdown-syntax-propertize-list-items start end)
      (ein:markdown-syntax-propertize-pre-blocks start end)
      (ein:markdown-syntax-propertize-blockquotes start end)
      (ein:markdown-syntax-propertize-headings start end)
      (ein:markdown-syntax-propertize-hrs start end)
      (ein:markdown-syntax-propertize-comments start end))))


;;; Markup Hiding =============================================================

(defconst ein:markdown-markup-properties
  '(face ein:markdown-markup-face)
  "List of properties and values to apply to markup.")

(defconst ein:markdown-language-keyword-properties
  '(face ein:markdown-language-keyword-face)
  "List of properties and values to apply to code block language names.")

(defconst ein:markdown-language-info-properties
  '(face ein:markdown-language-info-face)
  "List of properties and values to apply to code block language info strings.")

(defconst ein:markdown-include-title-properties
  '(face ein:markdown-link-title-face)
  "List of properties and values to apply to included code titles.")

;;; Font Lock =================================================================

(require 'font-lock)

(defvar ein:markdown-italic-face 'ein:markdown-italic-face
  "Face name to use for italic text.")

(defvar ein:markdown-bold-face 'ein:markdown-bold-face
  "Face name to use for bold text.")

(defvar ein:markdown-strike-through-face 'ein:markdown-strike-through-face
  "Face name to use for strike-through text.")

(defvar ein:markdown-header-delimiter-face 'ein:markdown-header-delimiter-face
  "Face name to use as a base for header delimiters.")

(defvar ein:markdown-header-rule-face 'ein:markdown-header-rule-face
  "Face name to use as a base for header rules.")

(defvar ein:markdown-header-face 'ein:markdown-header-face
  "Face name to use as a base for headers.")

(defvar ein:markdown-header-face-1 'ein:markdown-header-face-1
  "Face name to use for level-1 headers.")

(defvar ein:markdown-header-face-2 'ein:markdown-header-face-2
  "Face name to use for level-2 headers.")

(defvar ein:markdown-header-face-3 'ein:markdown-header-face-3
  "Face name to use for level-3 headers.")

(defvar ein:markdown-header-face-4 'ein:markdown-header-face-4
  "Face name to use for level-4 headers.")

(defvar ein:markdown-header-face-5 'ein:markdown-header-face-5
  "Face name to use for level-5 headers.")

(defvar ein:markdown-header-face-6 'ein:markdown-header-face-6
  "Face name to use for level-6 headers.")

(defvar ein:markdown-inline-code-face 'ein:markdown-inline-code-face
  "Face name to use for inline code.")

(defvar ein:markdown-list-face 'ein:markdown-list-face
  "Face name to use for list markers.")

(defvar ein:markdown-blockquote-face 'ein:markdown-blockquote-face
  "Face name to use for blockquote.")

(defvar ein:markdown-pre-face 'ein:markdown-pre-face
  "Face name to use for preformatted text.")

(defvar ein:markdown-language-keyword-face 'ein:markdown-language-keyword-face
  "Face name to use for programming language identifiers.")

(defvar ein:markdown-language-info-face 'ein:markdown-language-info-face
  "Face name to use for programming info strings.")

(defvar ein:markdown-link-face 'ein:markdown-link-face
  "Face name to use for links.")

(defvar ein:markdown-missing-link-face 'ein:markdown-missing-link-face
  "Face name to use for links where the linked file does not exist.")

(defvar ein:markdown-reference-face 'ein:markdown-reference-face
  "Face name to use for reference.")

(defvar ein:markdown-footnote-marker-face 'ein:markdown-footnote-marker-face
  "Face name to use for footnote markers.")

(defvar ein:markdown-url-face 'ein:markdown-url-face
  "Face name to use for URLs.")

(defvar ein:markdown-link-title-face 'ein:markdown-link-title-face
  "Face name to use for reference link titles.")

(defvar ein:markdown-line-break-face 'ein:markdown-line-break-face
  "Face name to use for hard line breaks.")

(defvar ein:markdown-comment-face 'ein:markdown-comment-face
  "Face name to use for HTML comments.")

(defvar ein:markdown-math-face 'ein:markdown-math-face
  "Face name to use for LaTeX expressions.")

(defvar ein:markdown-metadata-key-face 'ein:markdown-metadata-key-face
  "Face name to use for metadata keys.")

(defvar ein:markdown-metadata-value-face 'ein:markdown-metadata-value-face
  "Face name to use for metadata values.")

(defvar ein:markdown-highlight-face 'ein:markdown-highlight-face
  "Face name to use for mouse highlighting.")

(defvar ein:markdown-markup-face 'ein:markdown-markup-face
  "Face name to use for markup elements.")

(make-obsolete-variable 'ein:markdown-italic-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-bold-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-strike-through-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-header-delimiter-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-header-rule-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-header-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-header-face-1 "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-header-face-2 "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-header-face-3 "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-header-face-4 "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-header-face-5 "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-header-face-6 "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-inline-code-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-list-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-blockquote-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-pre-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-language-keyword-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-language-info-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-link-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-missing-link-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-reference-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-footnote-marker-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-url-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-link-title-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-line-break-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-comment-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-math-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-metadata-key-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-metadata-value-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-highlight-face "Use face name directly" "v2.4")
(make-obsolete-variable 'ein:markdown-markup-face "Use face name directly" "v2.4")

(defgroup ein:markdown-faces nil
  "Faces used in ein:markdown Mode"
  :group 'ein:markdown
  :group 'faces)

(defface ein:markdown-italic-face
  '((t (:inherit italic)))
  "Face for italic text."
  :group 'ein:markdown-faces)

(defface ein:markdown-bold-face
  '((t (:inherit bold)))
  "Face for bold text."
  :group 'ein:markdown-faces)

(defface ein:markdown-strike-through-face
  '((t (:strike-through t)))
  "Face for strike-through text."
  :group 'ein:markdown-faces)

(defface ein:markdown-markup-face
  '((t (:inherit shadow :slant normal :weight normal)))
  "Face for markup elements."
  :group 'ein:markdown-faces)

(defface ein:markdown-header-rule-face
  '((t (:inherit ein:markdown-markup-face)))
  "Base face for headers rules."
  :group 'ein:markdown-faces)

(defface ein:markdown-header-delimiter-face
  '((t (:inherit ein:markdown-markup-face)))
  "Base face for headers hash delimiter."
  :group 'ein:markdown-faces)

(defface ein:markdown-list-face
  '((t (:inherit ein:markdown-markup-face)))
  "Face for list item markers."
  :group 'ein:markdown-faces)

(defface ein:markdown-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face for blockquote sections."
  :group 'ein:markdown-faces)

(defface ein:markdown-code-face
  '((t (:inherit fixed-pitch)))
  "Face for inline code, pre blocks, and fenced code blocks.
This may be used, for example, to add a contrasting background to
inline code fragments and code blocks."
  :group 'ein:markdown-faces)

(defface ein:markdown-inline-code-face
  '((t (:inherit (ein:markdown-code-face font-lock-constant-face))))
  "Face for inline code."
  :group 'ein:markdown-faces)

(defface ein:markdown-pre-face
  '((t (:inherit (ein:markdown-code-face font-lock-constant-face))))
  "Face for preformatted text."
  :group 'ein:markdown-faces)

(defface ein:markdown-table-face
  '((t (:inherit (ein:markdown-code-face))))
  "Face for tables."
  :group 'ein:markdown-faces)

(defface ein:markdown-language-keyword-face
  '((t (:inherit font-lock-type-face)))
  "Face for programming language identifiers."
  :group 'ein:markdown-faces)

(defface ein:markdown-language-info-face
  '((t (:inherit font-lock-string-face)))
  "Face for programming language info strings."
  :group 'ein:markdown-faces)

(defface ein:markdown-link-face
  '((t (:inherit link)))
  "Face for links."
  :group 'ein:markdown-faces)

(defface ein:markdown-missing-link-face
  '((t (:inherit font-lock-warning-face)))
  "Face for missing links."
  :group 'ein:markdown-faces)

(defface ein:markdown-reference-face
  '((t (:inherit ein:markdown-markup-face)))
  "Face for link references."
  :group 'ein:markdown-faces)

(define-obsolete-face-alias 'ein:markdown-footnote-face
  'ein:markdown-footnote-marker-face "v2.3")

(defface ein:markdown-footnote-marker-face
  '((t (:inherit ein:markdown-markup-face)))
  "Face for footnote markers."
  :group 'ein:markdown-faces)

(defface ein:markdown-footnote-text-face
  '((t (:inherit font-lock-comment-face)))
  "Face for footnote text."
  :group 'ein:markdown-faces)

(defface ein:markdown-url-face
  '((t (:inherit font-lock-string-face)))
  "Face for URLs that are part of markup.
For example, this applies to URLs in inline links:
[link text](http://example.com/)."
  :group 'ein:markdown-faces)

(defface ein:markdown-plain-url-face
  '((t (:inherit ein:markdown-link-face)))
  "Face for URLs that are also links.
For example, this applies to plain angle bracket URLs:
<http://example.com/>."
  :group 'ein:markdown-faces)

(defface ein:markdown-link-title-face
  '((t (:inherit font-lock-comment-face)))
  "Face for reference link titles."
  :group 'ein:markdown-faces)

(defface ein:markdown-line-break-face
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for hard line breaks."
  :group 'ein:markdown-faces)

(defface ein:markdown-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for HTML comments."
  :group 'ein:markdown-faces)

(defface ein:markdown-math-face
  '((t (:inherit font-lock-string-face)))
  "Face for LaTeX expressions."
  :group 'ein:markdown-faces)

(defface ein:markdown-metadata-key-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for metadata keys."
  :group 'ein:markdown-faces)

(defface ein:markdown-metadata-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for metadata values."
  :group 'ein:markdown-faces)

(defface ein:markdown-highlight-face
  '((t (:inherit highlight)))
  "Face for mouse highlighting."
  :group 'ein:markdown-faces)

(defface ein:markdown-hr-face
  '((t (:inherit ein:markdown-markup-face)))
  "Face for horizontal rules."
  :group 'ein:markdown-faces)

(defface ein:markdown-html-tag-name-face
  '((t (:inherit font-lock-type-face)))
  "Face for HTML tag names."
  :group 'ein:markdown-faces)

(defface ein:markdown-html-tag-delimiter-face
  '((t (:inherit ein:markdown-markup-face)))
  "Face for HTML tag delimiters."
  :group 'ein:markdown-faces)

(defface ein:markdown-html-attr-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTML attribute names."
  :group 'ein:markdown-faces)

(defface ein:markdown-html-attr-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for HTML attribute values."
  :group 'ein:markdown-faces)

(defface ein:markdown-html-entity-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTML entities."
  :group 'ein:markdown-faces)

(defcustom ein:markdown-header-scaling nil
  "Whether to use variable-height faces for headers.
When non-nil, `markdown-header-face' will inherit from
`variable-pitch' and the scaling values in
`markdown-header-scaling-values' will be applied to
headers of levels one through six respectively."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (ein:markdown-update-header-faces))
  :group 'ein:markdown-faces
  :package-version '(ein:markdown-mode . "2.2"))

(defcustom ein:markdown-header-scaling-values
  '(2.0 1.7 1.4 1.1 1.0 1.0)
  "List of scaling values for headers of level one through six.
Used when `markdown-header-scaling' is non-nil."
  :type 'list
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (ein:markdown-update-header-faces))
  :group 'ein:markdown-faces)

(defun ein:markdown-make-header-faces ()
  "Build the faces used for ein:markdown headers."
  (unless (facep 'ein:markdown-header-face)
    (defface ein:markdown-header-face
      '((t (:inherit (font-lock-function-name-face) :weight bold)))
      "Base face for headers."
      :group 'ein:markdown-faces)
    (dotimes (num 6)
      (let* ((num1 (1+ num))
             (face-name (intern (format "ein:markdown-header-face-%s" num1)))
             (scale (float (nth num ein:markdown-header-scaling-values))))
        (eval
         `(defface ,face-name
            '((t (:inherit (variable-pitch ein:markdown-header-face) :height ,scale)))
            (format "Face for level %s headers.
You probably don't want to customize this face directly. Instead
you can customize the base face `markdown-header-face' or the
variable-height variable `markdown-header-scaling'." ,num1)
            :group 'ein:markdown-faces))))))

(defun ein:markdown-update-header-faces (&optional _scaling _scaling-values)
  "Update header faces using current values of ein:markdown-header-scaling and ein:markdown-header-scaling-values.  Arguments are ignored but retained to avoid breakage."
  (ein:markdown-make-header-faces)
  (dotimes (num 6)
    (let* ((face-name (intern (format "ein:markdown-header-face-%s" (1+ num))))
           (scale (if ein:markdown-header-scaling
                      (float (nth num ein:markdown-header-scaling-values))
                    1.0)))
      (unless (get face-name 'saved-face) ; Don't update customized faces
        (set-face-attribute face-name nil :height scale)))))

(defun ein:markdown-syntactic-face (state)
  "Return font-lock face for characters with given STATE.
See `font-lock-syntactic-face-function' for details."
  (let ((in-comment (nth 4 state)))
    (cond
     (in-comment 'ein:markdown-comment-face)
     (t nil))))

(defcustom ein:markdown-list-item-bullets
  '("●" "◎" "○" "◆" "◇" "►" "•")
  "List of bullets to use for unordered lists.
It can contain any number of symbols, which will be repeated.
Depending on your font, some reasonable choices are:
♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ❀ ◆ ◖ ▶ ► • ★ ▸."
  :group 'ein:markdown
  :type '(repeat (string :tag "Bullet character"))
  :package-version '(ein:markdown-mode . "2.3"))

(defun ein:markdown--footnote-marker-properties ()
  "Return a font-lock facespec expression for footnote marker text."
  `(face ein:markdown-footnote-marker-face))

(defun ein:markdown--pandoc-inline-footnote-properties ()
  "Return a font-lock facespec expression for Pandoc inline footnote text."
  `(face ein:markdown-footnote-text-face))

(define-obsolete-variable-alias
 'ein:markdown-mode-font-lock-keywords-basic
 'ein:markdown-mode-font-lock-keywords "v2.4")

(defvar ein:markdown-mode-font-lock-keywords
  `((ein:markdown-match-yaml-metadata-begin . ((1 'ein:markdown-markup-face)))
    (ein:markdown-match-yaml-metadata-end . ((1 'ein:markdown-markup-face)))
    (ein:markdown-match-yaml-metadata-key . ((1 'ein:markdown-metadata-key-face)
                                         (2 'ein:markdown-markup-face)
                                         (3 'ein:markdown-metadata-value-face)))
    (ein:markdown-fontify-tables)
    (ein:markdown-match-fenced-start-code-block . ((1 ein:markdown-markup-properties)
                                               (2 ein:markdown-markup-properties nil t)
                                               (3 ein:markdown-language-keyword-properties nil t)
                                               (4 ein:markdown-language-info-properties nil t)
                                               (5 ein:markdown-markup-properties nil t)))
    (ein:markdown-match-fenced-end-code-block . ((0 ein:markdown-markup-properties)))
    (ein:markdown-fontify-fenced-code-blocks)
    (ein:markdown-match-pre-blocks . ((0 'ein:markdown-pre-face)))
    (ein:markdown-fontify-headings)
    (ein:markdown-match-declarative-metadata . ((1 'ein:markdown-metadata-key-face)
                                              (2 'ein:markdown-markup-face)
                                              (3 'ein:markdown-metadata-value-face)))
    (ein:markdown-match-pandoc-metadata . ((1 'ein:markdown-markup-face)
                                       (2 'ein:markdown-markup-face)
                                       (3 'ein:markdown-metadata-value-face)))
    (ein:markdown-fontify-hrs)
    (ein:markdown-match-code . ((1 ein:markdown-markup-properties prepend)
                            (2 'ein:markdown-inline-code-face prepend)
                            (3 ein:markdown-markup-properties prepend)))
    (,ein:markdown-regex-kbd . ((1 ein:markdown-markup-properties)
                            (2 'ein:markdown-inline-code-face)
                            (3 ein:markdown-markup-properties)))
    (ein:markdown-fontify-angle-uris)
    (,ein:markdown-regex-email . 'ein:markdown-plain-url-face)
    (ein:markdown-match-html-tag . ((1 'ein:markdown-html-tag-delimiter-face t)
                                (2 'ein:markdown-html-tag-name-face t)
                                (3 'ein:markdown-html-tag-delimiter-face t)
                                ;; Anchored matcher for HTML tag attributes
                                (,ein:markdown-regex-html-attr
                                 ;; Before searching, move past tag
                                 ;; name; set limit at tag close.
                                 (progn
                                   (goto-char (match-end 2)) (match-end 3))
                                 nil
                                 . ((1 'ein:markdown-html-attr-name-face)
                                    (3 'ein:markdown-html-tag-delimiter-face nil t)
                                    (4 'ein:markdown-html-attr-value-face nil t)))))
    (,ein:markdown-regex-html-entity . 'ein:markdown-html-entity-face)
    (ein:markdown-fontify-list-items)
    (,ein:markdown-regex-footnote . ((1 ein:markdown-markup-properties)    ; [^
                                 (2 (ein:markdown--footnote-marker-properties)) ; label
                                 (3 ein:markdown-markup-properties)))  ; ]
    (,ein:markdown-regex-pandoc-inline-footnote . ((1 ein:markdown-markup-properties)   ; ^
                                               (2 ein:markdown-markup-properties)   ; [
                                               (3 (ein:markdown--pandoc-inline-footnote-properties)) ; text
                                               (4 ein:markdown-markup-properties))) ; ]
    (ein:markdown-match-includes . ((1 ein:markdown-markup-properties)
                                (2 ein:markdown-markup-properties nil t)
                                (3 ein:markdown-include-title-properties nil t)
                                (4 ein:markdown-markup-properties nil t)
                                (5 ein:markdown-markup-properties)
                                (6 'ein:markdown-url-face)
                                (7 ein:markdown-markup-properties)))
    (ein:markdown-fontify-inline-links)
    (ein:markdown-fontify-reference-links)
    (,ein:markdown-regex-reference-definition . ((1 'ein:markdown-markup-face) ; [
                                             (2 'ein:markdown-reference-face) ; label
                                             (3 'ein:markdown-markup-face)    ; ]
                                             (4 'ein:markdown-markup-face)    ; :
                                             (5 'ein:markdown-url-face)       ; url
                                             (6 'ein:markdown-link-title-face))) ; "title" (optional)
    (ein:markdown-fontify-plain-uris)
    ;; Math mode $..$
    (ein:markdown-match-math-single . ((1 'ein:markdown-markup-face prepend)
                                   (2 'ein:markdown-math-face append)
                                   (3 'ein:markdown-markup-face prepend)))
    ;; Math mode $$..$$
    (ein:markdown-match-math-double . ((1 'ein:markdown-markup-face prepend)
                                   (2 'ein:markdown-math-face append)
                                   (3 'ein:markdown-markup-face prepend)))
    ;; Math mode \[..\] and \\[..\\]
    (ein:markdown-match-math-display . ((1 'ein:markdown-markup-face prepend)
                                    (3 'ein:markdown-math-face append)
                                    (4 'ein:markdown-markup-face prepend)))
    (ein:markdown-match-bold . ((1 ein:markdown-markup-properties prepend)
                            (2 'ein:markdown-bold-face append)
                            (3 ein:markdown-markup-properties prepend)))
    (ein:markdown-match-italic . ((1 ein:markdown-markup-properties prepend)
                              (2 'ein:markdown-italic-face append)
                              (3 ein:markdown-markup-properties prepend)))
    (,ein:markdown-regex-strike-through . ((3 ein:markdown-markup-properties)
                                       (4 'ein:markdown-strike-through-face)
                                       (5 ein:markdown-markup-properties)))
    (,ein:markdown-regex-line-break . (1 'ein:markdown-line-break-face prepend))
    (ein:markdown-fontify-sub-superscripts)
    (ein:markdown-match-inline-attributes . ((0 ein:markdown-markup-properties prepend)))
    (ein:markdown-match-leanpub-sections . ((0 ein:markdown-markup-properties)))
    (ein:markdown-fontify-blockquotes))
  "Syntax highlighting for ein:markdown files.")

;; Footnotes
(defvar ein:markdown-footnote-counter 0
  "Counter for footnote numbers.")
(make-variable-buffer-local 'ein:markdown-footnote-counter)

(defconst ein:markdown-footnote-chars
  "[[:alnum:]-]"
  "Regular expression matching any character that is allowed in a footnote identifier.")

(defconst ein:markdown-regex-footnote-definition
  (concat "^ \\{0,3\\}\\[\\(\\^" ein:markdown-footnote-chars "*?\\)\\]:\\(?:[ \t]+\\|$\\)")
  "Regular expression matching a footnote definition, capturing the label.")


;;; Compatibility =============================================================

(defun ein:markdown-replace-regexp-in-string (regexp rep string)
  "Replace ocurrences of REGEXP with REP in STRING.
This is a compatibility wrapper to provide `replace-regexp-in-string'
in XEmacs 21."
  (if (featurep 'xemacs)
      (replace-in-string string regexp rep)
    (replace-regexp-in-string regexp rep string)))

;; `markdown-use-region-p' is a compatibility function which checks
;; for an active region, with fallbacks for older Emacsen and XEmacs.
(eval-and-compile
  (cond
   ;; Emacs 24 and newer
   ((fboundp 'use-region-p)
    (defalias 'ein:markdown-use-region-p 'use-region-p))
   ;; XEmacs
   ((fboundp 'region-active-p)
    (defalias 'ein:markdown-use-region-p 'region-active-p))))

;; Use new names for outline-mode functions in Emacs 25 and later.
(eval-and-compile
  (defalias 'ein:markdown-hide-sublevels
    (if (fboundp 'outline-hide-sublevels)
        'outline-hide-sublevels
      'hide-sublevels))
  (defalias 'ein:markdown-show-all
    (if (fboundp 'outline-show-all)
        'outline-show-all
      'show-all))
  (defalias 'ein:markdown-hide-body
    (if (fboundp 'outline-hide-body)
        'outline-hide-body
      'hide-body))
  (defalias 'ein:markdown-show-children
    (if (fboundp 'outline-show-children)
        'outline-show-children
      'show-children))
  (defalias 'ein:markdown-show-subtree
    (if (fboundp 'outline-show-subtree)
        'outline-show-subtree
      'show-subtree))
  (defalias 'ein:markdown-hide-subtree
    (if (fboundp 'outline-hide-subtree)
        'outline-hide-subtree
      'hide-subtree)))

;; Provide directory-name-p to Emacs 24
(defsubst ein:markdown-directory-name-p (name)
  "Return non-nil if NAME ends with a directory separator character.
Taken from `directory-name-p' from Emacs 25 and provided here for
backwards compatibility."
  (let ((len (length name))
        (lastc ?.))
    (if (> len 0)
        (setq lastc (aref name (1- len))))
    (or (= lastc ?/)
        (and (memq system-type '(windows-nt ms-dos))
             (= lastc ?\\)))))

;; Provide a function to find files recursively in Emacs 24.
(defalias 'ein:markdown-directory-files-recursively
  (if (fboundp 'directory-files-recursively)
      'directory-files-recursively
    (lambda (dir regexp)
    "Return list of all files under DIR that have file names matching REGEXP.
This function works recursively.  Files are returned in \"depth first\"
order, and files from each directory are sorted in alphabetical order.
Each file name appears in the returned list in its absolute form.
Based on `directory-files-recursively' from Emacs 25 and provided
here for backwards compatibility."
  (let ((result nil)
        (files nil)
        ;; When DIR is "/", remote file names like "/method:" could
        ;; also be offered.  We shall suppress them.
        (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (member file '("./" "../"))
        (if (ein:markdown-directory-name-p file)
            (let* ((leaf (substring file 0 (1- (length file))))
                   (full-file (expand-file-name leaf dir)))
              (setq result
                    (nconc result (ein:markdown-directory-files-recursively
                                   full-file regexp))))
          (when (string-match-p regexp file)
            (push (expand-file-name file dir) files)))))
    (nconc result (nreverse files))))))

(defun ein:markdown-flyspell-check-word-p ()
  "Return t if `flyspell' should check word just before point.
Used for `flyspell-generic-check-word-predicate'."
  (save-excursion
    (goto-char (1- (point)))
    (not (or (ein:markdown-code-block-at-point-p)
             (ein:markdown-inline-code-at-point-p)
             (ein:markdown-in-comment-p)
             (let ((faces (get-text-property (point) 'face)))
               (if (listp faces)
                   (or (memq 'ein:markdown-reference-face faces)
                       (memq 'ein:markdown-markup-face faces)
                       (memq 'ein:markdown-plain-url-face faces)
                       (memq 'ein:markdown-inline-code-face faces)
                       (memq 'ein:markdown-url-face faces))
                 (memq faces '(ein:markdown-reference-face
                               ein:markdown-markup-face
                               ein:markdown-plain-url-face
                               ein:markdown-inline-code-face
                               ein:markdown-url-face))))))))

;;; ein:markdown Parsing Functions ================================================

(define-obsolete-function-alias
  'ein:markdown-cur-line-blank 'ein:markdown-cur-line-blank-p "v2.4")
(define-obsolete-function-alias
  'ein:markdown-next-line-blank 'ein:markdown-next-line-blank-p "v2.4")

(defun ein:markdown-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at-p ein:markdown-regex-blank-line)))

(defun ein:markdown-prev-line-blank ()
  "Return t if the previous line is blank and nil otherwise.
If we are at the first line, then consider the previous line to be blank."
  (or (= (line-beginning-position) (point-min))
      (save-excursion
        (forward-line -1)
        (looking-at ein:markdown-regex-blank-line))))

(defun ein:markdown-prev-line-blank-p ()
  "Like `markdown-prev-line-blank', but preserve `match-data'."
  (save-match-data (ein:markdown-prev-line-blank)))

(defun ein:markdown-next-line-blank-p ()
  "Return t if the next line is blank and nil otherwise.
If we are at the last line, then consider the next line to be blank."
  (or (= (line-end-position) (point-max))
      (save-excursion
        (forward-line 1)
        (ein:markdown-cur-line-blank-p))))

(defun ein:markdown-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line.
Return 0 if the current line is the first line in the buffer."
  (save-excursion
    (if (= (line-beginning-position) (point-min))
        0
      (forward-line -1)
      (current-indentation))))

(defun ein:markdown-next-line-indent ()
  "Return the number of leading whitespace characters in the next line.
Return 0 if line is the last line in the buffer."
  (save-excursion
    (if (= (line-end-position) (point-max))
        0
      (forward-line 1)
      (current-indentation))))

(defun ein:markdown-new-baseline ()
  "Determine if the current line begins a new baseline level.
Assume point is positioned at beginning of line."
  (or (looking-at ein:markdown-regex-header)
      (looking-at ein:markdown-regex-hr)
      (and (= (current-indentation) 0)
           (not (looking-at ein:markdown-regex-list))
           (ein:markdown-prev-line-blank))))

(defun ein:markdown-search-backward-baseline ()
  "Search backward baseline point with no indentation and not a list item."
  (end-of-line)
  (let (stop)
    (while (not (or stop (bobp)))
      (re-search-backward ein:markdown-regex-block-separator-noindent nil t)
      (when (match-end 2)
        (goto-char (match-end 2))
        (cond
         ((ein:markdown-new-baseline)
          (setq stop t))
         ((looking-at-p ein:markdown-regex-list)
          (setq stop nil))
         (t (setq stop t)))))))

(defun ein:markdown-update-list-levels (marker indent levels)
  "Update list levels given list MARKER, block INDENT, and current LEVELS.
Here, MARKER is a string representing the type of list, INDENT is an integer
giving the indentation, in spaces, of the current block, and LEVELS is a
list of the indentation levels of parent list items.  When LEVELS is nil,
it means we are at baseline (not inside of a nested list)."
  (cond
   ;; New list item at baseline.
   ((and marker (null levels))
    (setq levels (list indent)))
   ;; List item with greater indentation (four or more spaces).
   ;; Increase list level.
   ((and marker (>= indent (+ (car levels) 4)))
    (setq levels (cons indent levels)))
   ;; List item with greater or equal indentation (less than four spaces).
   ;; Do not increase list level.
   ((and marker (>= indent (car levels)))
    levels)
   ;; Lesser indentation level.
   ;; Pop appropriate number of elements off LEVELS list (e.g., lesser
   ;; indentation could move back more than one list level).  Note
   ;; that this block need not be the beginning of list item.
   ((< indent (car levels))
    (while (and (> (length levels) 1)
                (< indent (+ (cadr levels) 4)))
      (setq levels (cdr levels)))
    levels)
   ;; Otherwise, do nothing.
   (t levels)))

(defun ein:markdown-calculate-list-levels ()
  "Calculate list levels at point.
Return a list of the form (n1 n2 n3 ...) where n1 is the
indentation of the deepest nested list item in the branch of
the list at the point, n2 is the indentation of the parent
list item, and so on.  The depth of the list item is therefore
the length of the returned list.  If the point is not at or
immediately  after a list item, return nil."
  (save-excursion
    (let ((first (point)) levels indent pre-regexp)
      ;; Find a baseline point with zero list indentation
      (ein:markdown-search-backward-baseline)
      ;; Search for all list items between baseline and LOC
      (while (and (< (point) first)
                  (re-search-forward ein:markdown-regex-list first t))
        (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" (1+ (length levels))))
        (beginning-of-line)
        (cond
         ;; Make sure this is not a header or hr
         ((ein:markdown-new-baseline) (setq levels nil))
         ;; Make sure this is not a line from a pre block
         ((looking-at-p pre-regexp))
         ;; If not, then update levels
         (t
          (setq indent (current-indentation))
          (setq levels (ein:markdown-update-list-levels (match-string 2)
                                                    indent levels))))
        (end-of-line))
      levels)))

(defun ein:markdown-prev-list-item (level)
  "Search backward from point for a list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent prev)
    (setq prev (point))
    (forward-line -1)
    (setq indent (current-indentation))
    (while
        (cond
         ;; List item
         ((and (looking-at-p ein:markdown-regex-list)
               (setq bounds (ein:markdown-cur-list-item-bounds)))
          (cond
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq prev (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq prev nil)
            nil)
           ;; Stop at beginning of buffer
           ((bobp) (setq prev nil))
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)))
         ;; Stop at beginning of buffer
         ((bobp) (setq prev nil))
         ;; Continue if current line is blank
         ((ein:markdown-cur-line-blank-p) t)
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the next is blank
         ((and (< indent level)
               (ein:markdown-next-line-blank-p))
          (setq prev nil))
         ;; Stop at a header
         ((looking-at-p ein:markdown-regex-header) (setq prev nil))
         ;; Stop at a horizontal rule
         ((looking-at-p ein:markdown-regex-hr) (setq prev nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line -1)
      (setq indent (current-indentation)))
    prev))

(defun ein:markdown-next-list-item (level)
  "Search forward from point for the next list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent next)
    (setq next (point))
    (if (looking-at ein:markdown-regex-header-setext)
        (goto-char (match-end 0)))
    (forward-line)
    (setq indent (current-indentation))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) nil)
         ;; Continue if the current line is blank
         ((ein:markdown-cur-line-blank-p) t)
         ;; List item
         ((and (looking-at-p ein:markdown-regex-list)
               (setq bounds (ein:markdown-cur-list-item-bounds)))
          (cond
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq next (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq next nil)
            nil)))
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (ein:markdown-prev-line-blank-p))
          (setq next nil))
         ;; Stop at a header
         ((looking-at-p ein:markdown-regex-header) (setq next nil))
         ;; Stop at a horizontal rule
         ((looking-at-p ein:markdown-regex-hr) (setq next nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (current-indentation)))
    next))

(defun ein:markdown-cur-list-item-end (level)
  "Move to end of list item with pre-marker indentation LEVEL.
Return the point at the end when a list item was found at the
original point.  If the point is not in a list item, do nothing."
  (let (indent)
    (forward-line)
    (setq indent (current-indentation))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) nil)
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Continue if the current line is blank
         ((looking-at ein:markdown-regex-blank-line) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (ein:markdown-prev-line-blank))
          nil)
         ;; Stop at a new list items of the same or lesser
         ;; indentation, headings, and horizontal rules.
         ((looking-at (concat "\\(?:" ein:markdown-regex-list
                              "\\|" ein:markdown-regex-header
                              "\\|" ein:markdown-regex-hr "\\)"))
          nil)
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (current-indentation)))
    ;; Don't skip over whitespace for empty list items (marker and
    ;; whitespace only), just move to end of whitespace.
    (if (save-excursion
          (beginning-of-line)
          (looking-at (concat ein:markdown-regex-list "[ \t]*$")))
        (goto-char (match-end 3))
      (skip-chars-backward " \t\n"))
    (end-of-line)
    (point)))

(defun ein:markdown-cur-list-item-bounds ()
  "Return bounds for list item at point.
Return a list of the following form:

    (begin end indent nonlist-indent marker checkbox match)

The named components are:

  - begin: Position of beginning of list item, including leading indentation.
  - end: Position of the end of the list item, including list item text.
  - indent: Number of characters of indentation before list marker (an integer).
  - nonlist-indent: Number characters of indentation, list
    marker, and whitespace following list marker (an integer).
  - marker: String containing the list marker and following whitespace
            (e.g., \"- \" or \"* \").
  - checkbox: String containing the GFM checkbox portion, if any,
    including any trailing whitespace before the text
    begins (e.g., \"[x] \").
  - match: match data for ein:markdown-regex-list

As an example, for the following unordered list item

   - item

the returned list would be

    (1 14 3 5 \"- \" nil (1 6 1 4 4 5 5 6))

If the point is not inside a list item, return nil."
  (car (get-text-property (point-at-bol) 'ein:markdown-list-item)))

(defun ein:markdown-list-item-at-point-p ()
  "Return t if there is a list item at the point and nil otherwise."
  (save-match-data (ein:markdown-cur-list-item-bounds)))

(defun ein:markdown-prev-list-item-bounds ()
  "Return bounds of previous item in the same list of any level.
The return value has the same form as that of
`markdown-cur-list-item-bounds'."
  (save-excursion
    (let ((cur-bounds (ein:markdown-cur-list-item-bounds))
          (beginning-of-list (save-excursion (ein:markdown-beginning-of-list)))
          stop)
      (when cur-bounds
        (goto-char (nth 0 cur-bounds))
        (while (and (not stop) (not (bobp))
                    (re-search-backward ein:markdown-regex-list
                                        beginning-of-list t))
          (unless (or (looking-at ein:markdown-regex-hr)
                      (ein:markdown-code-block-at-point-p))
            (setq stop (point))))
        (ein:markdown-cur-list-item-bounds)))))

(defun ein:markdown-next-list-item-bounds ()
  "Return bounds of next item in the same list of any level.
The return value has the same form as that of
`markdown-cur-list-item-bounds'."
  (save-excursion
    (let ((cur-bounds (ein:markdown-cur-list-item-bounds))
          (end-of-list (save-excursion (ein:markdown-end-of-list)))
          stop)
      (when cur-bounds
        (goto-char (nth 0 cur-bounds))
        (end-of-line)
        (while (and (not stop) (not (eobp))
                    (re-search-forward ein:markdown-regex-list
                                       end-of-list t))
          (unless (or (looking-at ein:markdown-regex-hr)
                      (ein:markdown-code-block-at-point-p))
            (setq stop (point))))
        (when stop
          (ein:markdown-cur-list-item-bounds))))))

(defun ein:markdown-beginning-of-list ()
  "Move point to beginning of list at point, if any."
  (interactive)
  (let ((orig-point (point))
        (list-begin (save-excursion
                      (ein:markdown-search-backward-baseline)
                      ;; Stop at next list item, regardless of the indentation.
                      (ein:markdown-next-list-item (point-max))
                      (when (looking-at ein:markdown-regex-list)
                        (point)))))
    (when (and list-begin (<= list-begin orig-point))
      (goto-char list-begin))))

(defun ein:markdown-end-of-list ()
  "Move point to end of list at point, if any."
  (interactive)
  (let ((start (point))
        (end (save-excursion
               (when (ein:markdown-beginning-of-list)
                 ;; Items can't have nonlist-indent <= 1, so this
                 ;; moves past all list items.
                 (ein:markdown-next-list-item 1)
                 (skip-syntax-backward "-")
                 (unless (eobp) (forward-char 1))
                 (point)))))
    (when (and end (>= end start))
      (goto-char end))))

(defun ein:markdown-up-list ()
  "Move point to beginning of parent list item."
  (interactive)
  (let ((cur-bounds (ein:markdown-cur-list-item-bounds)))
    (when cur-bounds
      (ein:markdown-prev-list-item (1- (nth 3 cur-bounds)))
      (let ((up-bounds (ein:markdown-cur-list-item-bounds)))
        (when (and up-bounds (< (nth 3 up-bounds) (nth 3 cur-bounds)))
          (point))))))

(defun ein:markdown-bounds-of-thing-at-point (thing)
  "Call `bounds-of-thing-at-point' for THING with slight modifications.
Does not include trailing newlines when THING is 'line.  Handles the
end of buffer case by setting both endpoints equal to the value of
`point-max', since an empty region will trigger empty markup insertion.
Return bounds of form (beg . end) if THING is found, or nil otherwise."
  (let* ((bounds (bounds-of-thing-at-point thing))
         (a (car bounds))
         (b (cdr bounds)))
    (when bounds
      (when (eq thing 'line)
        (cond ((and (eobp) (ein:markdown-cur-line-blank-p))
               (setq a b))
              ((char-equal (char-before b) ?\^J)
               (setq b (1- b)))))
      (cons a b))))

(defun ein:markdown-reference-definition (reference)
  "Find out whether ein:markdown REFERENCE is defined.
REFERENCE should not include the square brackets.
When REFERENCE is defined, return a list of the form (text start end)
containing the definition text itself followed by the start and end
locations of the text.  Otherwise, return nil.
Leave match data for `markdown-regex-reference-definition'
intact additional processing."
  (let ((reference (downcase reference)))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward ein:markdown-regex-reference-definition nil t)
          (when (string= reference (downcase (match-string-no-properties 2)))
            (throw 'found
                   (list (match-string-no-properties 5)
                         (match-beginning 5) (match-end 5)))))))))

(defun ein:markdown-get-defined-references ()
  "Return all defined reference labels and their line numbers (not including square brackets)."
  (save-excursion
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward ein:markdown-regex-reference-definition nil t)
        (let ((target (match-string-no-properties 2)))
          (cl-pushnew
           (cons (downcase target)
                 (ein:markdown-line-number-at-pos (match-beginning 2)))
           refs :test #'equal :key #'car)))
      (reverse refs))))

(defun ein:markdown-get-used-uris ()
  "Return a list of all used URIs in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let (uris)
      (while (re-search-forward
              (concat "\\(?:" ein:markdown-regex-link-inline
                      "\\|" ein:markdown-regex-angle-uri
                      "\\|" ein:markdown-regex-uri
                      "\\|" ein:markdown-regex-email
                      "\\)")
              nil t)
        (unless (or (ein:markdown-inline-code-at-point-p)
                    (ein:markdown-code-block-at-point-p))
          (cl-pushnew (or (match-string-no-properties 6)
                          (match-string-no-properties 10)
                          (match-string-no-properties 12)
                          (match-string-no-properties 13))
                      uris :test #'equal)))
      (reverse uris))))

(defun ein:markdown-inline-code-at-pos (pos)
  "Return non-nil if there is an inline code fragment at POS.
Return nil otherwise.  Set match data according to
`markdown-match-code' upon success.
This function searches the block for a code fragment that
contains the point using `markdown-match-code'.  We do this
because `thing-at-point-looking-at' does not work reliably with
`markdown-regex-code'.

The match data is set as follows:
Group 1 matches the opening backquotes.
Group 2 matches the code fragment itself, without backquotes.
Group 3 matches the closing backquotes."
  (save-excursion
    (goto-char pos)
    (let ((old-point (point))
          (end-of-block (progn (ein:markdown-end-of-text-block) (point)))
          found)
      (ein:markdown-beginning-of-text-block)
      (while (and (ein:markdown-match-code end-of-block)
                  (setq found t)
                  (< (match-end 0) old-point)))
      (and found                              ; matched something
           (<= (match-beginning 0) old-point) ; match contains old-point
           (> (match-end 0) old-point)))))

(defun ein:markdown-inline-code-at-pos-p (pos)
  "Return non-nil if there is an inline code fragment at POS.
Like `markdown-inline-code-at-pos`, but preserves match data."
  (save-match-data (ein:markdown-inline-code-at-pos pos)))

(defun ein:markdown-inline-code-at-point ()
  "Return non-nil if the point is at an inline code fragment.
See `markdown-inline-code-at-pos' for details."
  (ein:markdown-inline-code-at-pos (point)))

(defun ein:markdown-inline-code-at-point-p (&optional pos)
  "Return non-nil if there is inline code at the POS.
This is a predicate function counterpart to
`markdown-inline-code-at-point' which does not modify the match
data.  See `markdown-code-block-at-point-p' for code blocks."
  (save-match-data (ein:markdown-inline-code-at-pos (or pos (point)))))

(make-obsolete 'ein:markdown-code-at-point-p 'ein:markdown-inline-code-at-point-p "v2.2")

(defun ein:markdown-code-block-at-pos (pos)
  "Return match data list if there is a code block at POS.
Uses text properties at the beginning of the line position.
This includes pre blocks, tilde-fenced code blocks, and GFM
quoted code blocks.  Return nil otherwise."
  (let ((bol (save-excursion (goto-char pos) (point-at-bol))))
    (or (get-text-property bol 'ein:markdown-pre)
        (let* ((bounds (ein:markdown-get-enclosing-fenced-block-construct pos))
               (second (cl-second bounds)))
          (if second
              ;; chunks are right open
              (when (< pos second)
                bounds)
            bounds)))))

;; Function was renamed to emphasize that it does not modify match-data.
(defalias 'ein:markdown-code-block-at-point 'ein:markdown-code-block-at-point-p)

(defun ein:markdown-code-block-at-point-p (&optional pos)
  "Return non-nil if there is a code block at the POS.
This includes pre blocks, tilde-fenced code blocks, and GFM
quoted code blocks.  This function does not modify the match
data.  See `markdown-inline-code-at-point-p' for inline code."
  (save-match-data (ein:markdown-code-block-at-pos (or pos (point)))))

(defun ein:markdown-heading-at-point (&optional pos)
  "Return non-nil if there is a heading at the POS.
Set match data for `markdown-regex-header'."
  (let ((match-data (get-text-property (or pos (point)) 'ein:markdown-heading)))
    (when match-data
      (set-match-data match-data)
      t)))

(defun ein:markdown-pipe-at-bol-p ()
  "Return non-nil if the line begins with a pipe symbol.
This may be useful for tables and Pandoc's line_blocks extension."
  (char-equal (char-after (point-at-bol)) ?|))


;;; ein:markdown Font Lock Matching Functions =====================================

(defun ein:markdown-range-property-any (begin end prop prop-values)
  "Return t if PROP from BEGIN to END is equal to one of the given PROP-VALUES.
Also returns t if PROP is a list containing one of the PROP-VALUES.
Return nil otherwise."
  (let (props)
    (catch 'found
      (dolist (loc (number-sequence begin end))
        (when (setq props (get-text-property loc prop))
          (cond ((listp props)
                 ;; props is a list, check for membership
                 (dolist (val prop-values)
                   (when (memq val props) (throw 'found loc))))
                (t
                 ;; props is a scalar, check for equality
                 (dolist (val prop-values)
                   (when (eq val props) (throw 'found loc))))))))))

(defun ein:markdown-range-properties-exist (begin end props)
  (cl-loop
   for loc in (number-sequence begin end)
   with result = nil
   while (not
          (setq result
                (cl-some (lambda (prop) (get-text-property loc prop)) props)))
   finally return result))

(defun ein:markdown-match-inline-generic (regex last &optional faceless)
  "Match inline REGEX from the point to LAST.
When FACELESS is non-nil, do not return matches where faces have been applied."
  (when (re-search-forward regex last t)
    (let ((bounds (ein:markdown-code-block-at-pos (match-beginning 1)))
          (face (and faceless (text-property-not-all
                               (match-beginning 0) (match-end 0) 'face nil))))
      (cond
       ;; In code block: move past it and recursively search again
       (bounds
        (when (< (goto-char (cl-second bounds)) last)
          (ein:markdown-match-inline-generic regex last faceless)))
       ;; When faces are found in the match range, skip over the match and
       ;; recursively search again.
       (face
        (when (< (goto-char (match-end 0)) last)
          (ein:markdown-match-inline-generic regex last faceless)))
       ;; Keep match data and return t when in bounds.
       (t
        (<= (match-end 0) last))))))

(defun ein:markdown-match-code (last)
  "Match inline code fragments from point to LAST."
  (unless (bobp)
    (backward-char 1))
  (when (ein:markdown-search-until-condition
         (lambda ()
           (and
            ;; Advance point in case of failure, but without exceeding last.
            (goto-char (min (1+ (match-beginning 1)) last))
            (not (ein:markdown-in-comment-p (match-beginning 1)))
            (not (ein:markdown-in-comment-p (match-end 1)))
            (not (ein:markdown-code-block-at-pos (match-beginning 1)))))
         ein:markdown-regex-code last t)
      (set-match-data (list (match-beginning 1) (match-end 1)
                            (match-beginning 2) (match-end 2)
                            (match-beginning 3) (match-end 3)
                            (match-beginning 4) (match-end 4)))
      (goto-char (min (1+ (match-end 0)) last (point-max)))
      t))

(defun ein:markdown-match-bold (last)
  "Match inline bold from the point to LAST."
  (when (ein:markdown-match-inline-generic ein:markdown-regex-bold last)
    (let ((begin (match-beginning 2))
          (end (match-end 2)))
      (if (or (ein:markdown-inline-code-at-pos-p begin)
              (ein:markdown-inline-code-at-pos-p end)
              (ein:markdown-in-comment-p)
              (ein:markdown-range-property-any
               begin begin 'face '(ein:markdown-url-face
                                   ein:markdown-plain-url-face))
              (ein:markdown-range-property-any
               begin end 'face '(ein:markdown-hr-face
                                 ein:markdown-math-face)))
          (progn (goto-char (min (1+ begin) last))
                 (when (< (point) last)
                   (ein:markdown-match-italic last)))
        (set-match-data (list (match-beginning 2) (match-end 2)
                              (match-beginning 3) (match-end 3)
                              (match-beginning 4) (match-end 4)
                              (match-beginning 5) (match-end 5)))
        t))))

(defun ein:markdown-match-italic (last)
  "Match inline italics from the point to LAST."
  (let ((regex ein:markdown-regex-italic))
    (when (ein:markdown-match-inline-generic regex last)
      (let ((begin (match-beginning 1))
            (end (match-end 1)))
        (if (or (ein:markdown-inline-code-at-pos-p begin)
                (ein:markdown-inline-code-at-pos-p end)
                (ein:markdown-in-comment-p)
                (ein:markdown-range-property-any
                 begin begin 'face '(ein:markdown-url-face
                                     ein:markdown-plain-url-face))
                (ein:markdown-range-property-any
                 begin end 'face '(ein:markdown-bold-face
                                   ein:markdown-list-face
                                   ein:markdown-hr-face
                                   ein:markdown-math-face)))
            (progn (goto-char (min (1+ begin) last))
                   (when (< (point) last)
                     (ein:markdown-match-italic last)))
          (set-match-data (list (match-beginning 1) (match-end 1)
                                (match-beginning 2) (match-end 2)
                                (match-beginning 3) (match-end 3)
                                (match-beginning 4) (match-end 4)))
          t)))))

(defun ein:markdown-match-math-generic (regex last)
  "Match REGEX from point to LAST.
REGEX is either `markdown-regex-math-inline-single' for matching
$..$ or `markdown-regex-math-inline-double' for matching $$..$$."
  (when (and ein:markdown-enable-math (ein:markdown-match-inline-generic regex last))
    (let ((begin (match-beginning 1)) (end (match-end 1)))
      (prog1
          (if (or (ein:markdown-range-property-any
                   begin end 'face
                   '(ein:markdown-inline-code-face ein:markdown-bold-face))
                  (ein:markdown-range-properties-exist
                   begin end
                   (ein:markdown-get-fenced-block-middle-properties)))
              (ein:markdown-match-math-generic regex last)
            t)
        (goto-char (1+ (match-end 0)))))))

(defun ein:markdown-match-list-items (last)
  "Match list items from point to LAST."
  (let* ((first (point))
         (pos first)
         (prop 'ein:markdown-list-item)
         (bounds (car (get-text-property pos prop))))
    (while
        (and (or (null (setq bounds (car (get-text-property pos prop))))
                 (< (cl-first bounds) pos))
             (< (point) last)
             (setq pos (next-single-property-change pos prop nil last))
             (goto-char pos)))
    (when bounds
      (set-match-data (cl-seventh bounds))
      ;; Step at least one character beyond point. Otherwise
      ;; `font-lock-fontify-keywords-region' infloops.
      (goto-char (min (1+ (max (point-at-eol) first))
                      (point-max)))
      t)))

(defun ein:markdown-match-math-single (last)
  "Match single quoted $..$ math from point to LAST."
  (ein:markdown-match-math-generic ein:markdown-regex-math-inline-single last))

(defun ein:markdown-match-math-double (last)
  "Match double quoted $$..$$ math from point to LAST."
  (ein:markdown-match-math-generic ein:markdown-regex-math-inline-double last))

(defun ein:markdown-match-math-display (last)
  "Match bracketed display math \[..\] and \\[..\\] from point to LAST."
  (ein:markdown-match-math-generic ein:markdown-regex-math-display last))

(defun ein:markdown-match-propertized-text (property last)
  "Match text with PROPERTY from point to LAST.
Restore match data previously stored in PROPERTY."
  (let ((saved (get-text-property (point) property))
        pos)
    (unless saved
      (setq pos (next-single-property-change (point) property nil last))
      (unless (= pos last)
        (setq saved (get-text-property pos property))))
    (when saved
      (set-match-data saved)
      ;; Step at least one character beyond point. Otherwise
      ;; `font-lock-fontify-keywords-region' infloops.
      (goto-char (min (1+ (max (match-end 0) (point)))
                      (point-max)))
      saved)))

(defun ein:markdown-match-pre-blocks (last)
  "Match preformatted blocks from point to LAST.
Use data stored in 'ein:markdown-pre text property during syntax
analysis."
  (ein:markdown-match-propertized-text 'ein:markdown-pre last))

(defun ein:markdown-match-fenced-code-blocks (last)
  "Match fenced code blocks from the point to LAST."
  (ein:markdown-match-propertized-text 'ein:markdown-fenced-code last))

(defun ein:markdown-match-fenced-start-code-block (last)
  (ein:markdown-match-propertized-text 'ein:markdown-tilde-fence-begin last))

(defun ein:markdown-match-fenced-end-code-block (last)
  (ein:markdown-match-propertized-text 'ein:markdown-tilde-fence-end last))

(defun ein:markdown-match-blockquotes (last)
  "Match blockquotes from point to LAST.
Use data stored in 'ein:markdown-blockquote text property during syntax
analysis."
  (ein:markdown-match-propertized-text 'ein:markdown-blockquote last))

(defun ein:markdown-match-hr (last)
  "Match horizontal rules comments from the point to LAST."
  (ein:markdown-match-propertized-text 'ein:markdown-hr last))

(defun ein:markdown-match-comments (last)
  "Match HTML comments from the point to LAST."
  (when (and (skip-syntax-forward "^<" last))
    (let ((beg (point)))
      (when (and (skip-syntax-forward "^>" last) (< (point) last))
        (forward-char)
        (set-match-data (list beg (point)))
        t))))

(defun ein:markdown-match-generic-links (last ref)
  "Match inline links from point to LAST.
When REF is non-nil, match reference links instead of standard
links with URLs.
This function should only be used during font-lock, as it
determines syntax based on the presence of faces for previously
processed elements."
  ;; Search for the next potential link (not in a code block).
  (let ((prohibited-faces '(ein:markdown-pre-face
                            ein:markdown-code-face
                            ein:markdown-inline-code-face
                            ein:markdown-comment-face))
        found)
    (while
        (and (not found) (< (point) last)
             (progn
               ;; Clear match data to test for a match after functions returns.
               (set-match-data nil)
               ;; Preliminary regular expression search so we can return
               ;; quickly upon failure.  This doesn't handle malformed links
               ;; or nested square brackets well, so if it passes we back up
               ;; continue with a more precise search.
               (re-search-forward
                (if ref
                    ein:markdown-regex-link-reference
                  ein:markdown-regex-link-inline)
                last 'limit)))
      ;; Keep searching if this is in a code block, inline code, or a
      ;; comment, or if it is include syntax. The link text portion
      ;; (group 3) may contain inline code or comments, but the
      ;; markup, URL, and title should not be part of such elements.
      (if (or (ein:markdown-range-property-any
               (match-beginning 0) (match-end 2) 'face prohibited-faces)
              (ein:markdown-range-property-any
               (match-beginning 4) (match-end 0) 'face prohibited-faces)
              (and (char-equal (char-after (point-at-bol)) ?<)
                   (char-equal (char-after (1+ (point-at-bol))) ?<)))
          (set-match-data nil)
        (setq found t))))
  ;; Match opening exclamation point (optional) and left bracket.
  (when (match-beginning 2)
    (let* ((bang (match-beginning 1))
           (first-begin (match-beginning 2))
           ;; Find end of block to prevent matching across blocks.
           (end-of-block (save-excursion
                           (progn
                             (goto-char (match-beginning 2))
                             (ein:markdown-end-of-text-block)
                             (point))))
           ;; Move over balanced expressions to closing right bracket.
           ;; Catch unbalanced expression errors and return nil.
           (first-end (condition-case nil
                           (and (goto-char first-begin)
                                (scan-sexps (point) 1))
                         (error nil)))
           ;; Continue with point at CONT-POINT upon failure.
           (cont-point (min (1+ first-begin) last))
           second-begin second-end url-begin url-end
           title-begin title-end)
      ;; When bracket found, in range, and followed by a left paren/bracket...
      (when (and first-end (< first-end end-of-block) (goto-char first-end)
                 (char-equal (char-after (point)) (if ref ?\[ ?\()))
        ;; Scan across balanced expressions for closing parenthesis/bracket.
        (setq second-begin (point)
              second-end (condition-case nil
                            (scan-sexps (point) 1)
                          (error nil)))
        ;; Check that closing parenthesis/bracket is in range.
        (if (and second-end (<= second-end end-of-block) (<= second-end last))
            (progn
              ;; Search for (optional) title inside closing parenthesis
              (when (and (not ref) (search-forward "\"" second-end t))
                (setq title-begin (1- (point))
                      title-end (and (goto-char second-end)
                                     (search-backward "\"" (1+ title-begin) t))
                      title-end (and title-end (1+ title-end))))
              ;; Store URL/reference range
              (setq url-begin (1+ second-begin)
                    url-end (1- (or title-begin second-end)))
              ;; Set match data, move point beyond link, and return
              (set-match-data
               (list (or bang first-begin) second-end  ; 0 - all
                     bang (and bang (1+ bang))         ; 1 - bang
                     first-begin (1+ first-begin)      ; 2 - markup
                     (1+ first-begin) (1- first-end)   ; 3 - link text
                     (1- first-end) first-end          ; 4 - markup
                     second-begin (1+ second-begin)    ; 5 - markup
                     url-begin url-end                 ; 6 - url/reference
                     title-begin title-end             ; 7 - title
                     (1- second-end) second-end))      ; 8 - markup
              ;; Nullify cont-point and leave point at end and
              (setq cont-point nil)
              (goto-char second-end))
          ;; If no closing parenthesis in range, update continuation point
          (setq cont-point (min end-of-block second-begin))))
      (cond
       ;; On failure, continue searching at cont-point
       ((and cont-point (< cont-point last))
        (goto-char cont-point)
        (ein:markdown-match-generic-links last ref))
       ;; No more text, return nil
       ((and cont-point (= cont-point last))
        nil)
       ;; Return t if a match occurred
       (t t)))))

(defun ein:markdown-match-angle-uris (last)
  "Match angle bracket URIs from point to LAST."
  (when (ein:markdown-match-inline-generic ein:markdown-regex-angle-uri last)
    (goto-char (1+ (match-end 0)))))

(defun ein:markdown-match-plain-uris (last)
  "Match plain URIs from point to LAST."
  (when (ein:markdown-match-inline-generic ein:markdown-regex-uri last t)
    (goto-char (1+ (match-end 0)))))

(defvar ein:markdown-conditional-search-function #'re-search-forward
  "Conditional search function used in `markdown-search-until-condition'.
Made into a variable to allow for dynamic let-binding.")

(defun ein:markdown-search-until-condition (condition &rest args)
  (let (ret)
    (while (and (not ret) (apply ein:markdown-conditional-search-function args))
      (setq ret (funcall condition)))
    ret))

(defun ein:markdown-match-generic-metadata (regexp last)
  "Match metadata declarations specified by REGEXP from point to LAST.
These declarations must appear inside a metadata block that begins at
the beginning of the buffer and ends with a blank line (or the end of
the buffer)."
  (let* ((first (point))
         (end-re "\n[ \t]*\n\\|\n\\'\\|\\'")
         (block-begin (goto-char 1))
         (block-end (re-search-forward end-re nil t)))
    (if (and block-end (> first block-end))
        ;; Don't match declarations if there is no metadata block or if
        ;; the point is beyond the block.  Move point to point-max to
        ;; prevent additional searches and return return nil since nothing
        ;; was found.
        (progn (goto-char (point-max)) nil)
      ;; If a block was found that begins before LAST and ends after
      ;; point, search for declarations inside it.  If the starting is
      ;; before the beginning of the block, start there. Otherwise,
      ;; move back to FIRST.
      (goto-char (if (< first block-begin) block-begin first))
      (if (re-search-forward regexp (min last block-end) t)
          ;; If a metadata declaration is found, set match-data and return t.
          (let ((key-beginning (match-beginning 1))
                (key-end (match-end 1))
                (markup-begin (match-beginning 2))
                (markup-end (match-end 2))
                (value-beginning (match-beginning 3)))
            (set-match-data (list key-beginning (point) ; complete metadata
                                  key-beginning key-end ; key
                                  markup-begin markup-end ; markup
                                  value-beginning (point))) ; value
            t)
        ;; Otherwise, move the point to last and return nil
        (goto-char last)
        nil))))

(defun ein:markdown-match-declarative-metadata (last)
  "Match declarative metadata from the point to LAST."
  (ein:markdown-match-generic-metadata ein:markdown-regex-declarative-metadata last))

(defun ein:markdown-match-pandoc-metadata (last)
  "Match Pandoc metadata from the point to LAST."
  (ein:markdown-match-generic-metadata ein:markdown-regex-pandoc-metadata last))

(defun ein:markdown-match-yaml-metadata-begin (last)
  (ein:markdown-match-propertized-text 'ein:markdown-yaml-metadata-begin last))

(defun ein:markdown-match-yaml-metadata-end (last)
  (ein:markdown-match-propertized-text 'ein:markdown-yaml-metadata-end last))

(defun ein:markdown-match-yaml-metadata-key (last)
  (ein:markdown-match-propertized-text 'ein:markdown-metadata-key last))

(defun ein:markdown-match-inline-attributes (last)
  "Match inline attributes from point to LAST."
  (when (ein:markdown-match-inline-generic ein:markdown-regex-inline-attributes last)
    (unless (or (ein:markdown-inline-code-at-pos-p (match-beginning 0))
                (ein:markdown-inline-code-at-pos-p (match-end 0))
                (ein:markdown-in-comment-p))
      t)))

(defun ein:markdown-match-leanpub-sections (last)
  "Match Leanpub section markers from point to LAST."
  (when (ein:markdown-match-inline-generic ein:markdown-regex-leanpub-sections last)
    (unless (or (ein:markdown-inline-code-at-pos-p (match-beginning 0))
                (ein:markdown-inline-code-at-pos-p (match-end 0))
                (ein:markdown-in-comment-p))
      t)))

(defun ein:markdown-match-includes (last)
  "Match include statements from point to LAST.
Sets match data for the following seven groups:
Group 1: opening two angle brackets
Group 2: opening title delimiter (optional)
Group 3: title text (optional)
Group 4: closing title delimiter (optional)
Group 5: opening filename delimiter
Group 6: filename
Group 7: closing filename delimiter"
  (when (ein:markdown-match-inline-generic ein:markdown-regex-include last)
    (let ((valid (not (or (ein:markdown-in-comment-p (match-beginning 0))
                          (ein:markdown-in-comment-p (match-end 0))
                          (ein:markdown-code-block-at-pos (match-beginning 0))))))
      (cond
       ;; Parentheses and maybe square brackets, but no curly braces:
       ;; match optional title in square brackets and file in parentheses.
       ((and valid (match-beginning 5)
             (not (match-beginning 8)))
        (set-match-data (list (match-beginning 1) (match-end 7)
                              (match-beginning 1) (match-end 1)
                              (match-beginning 2) (match-end 2)
                              (match-beginning 3) (match-end 3)
                              (match-beginning 4) (match-end 4)
                              (match-beginning 5) (match-end 5)
                              (match-beginning 6) (match-end 6)
                              (match-beginning 7) (match-end 7))))
       ;; Only square brackets present: match file in square brackets.
       ((and valid (match-beginning 2)
             (not (match-beginning 5))
             (not (match-beginning 7)))
        (set-match-data (list (match-beginning 1) (match-end 4)
                              (match-beginning 1) (match-end 1)
                              nil nil
                              nil nil
                              nil nil
                              (match-beginning 2) (match-end 2)
                              (match-beginning 3) (match-end 3)
                              (match-beginning 4) (match-end 4))))
       ;; Only curly braces present: match file in curly braces.
       ((and valid (match-beginning 8)
             (not (match-beginning 2))
             (not (match-beginning 5)))
        (set-match-data (list (match-beginning 1) (match-end 10)
                              (match-beginning 1) (match-end 1)
                              nil nil
                              nil nil
                              nil nil
                              (match-beginning 8) (match-end 8)
                              (match-beginning 9) (match-end 9)
                              (match-beginning 10) (match-end 10))))
       (t
        ;; Not a valid match, move to next line and search again.
        (forward-line)
        (when (< (point) last)
          (setq valid (ein:markdown-match-includes last)))))
      valid)))

(defun ein:markdown-match-html-tag (last)
  "Match HTML tags from point to LAST."
  (when (and ein:markdown-enable-html
             (ein:markdown-match-inline-generic ein:markdown-regex-html-tag last t))
    (set-match-data (list (match-beginning 0) (match-end 0)
                          (match-beginning 1) (match-end 1)
                          (match-beginning 2) (match-end 2)
                          (match-beginning 9) (match-end 9)))
    t))


;;; ein:markdown Font Fontification Functions =====================================

(defun ein:markdown--first-displayable (seq)
  "Return the first displayable character or string in SEQ.
SEQ may be an atom or a sequence."
  (let ((seq (if (listp seq) seq (list seq))))
    (cond ((stringp (car seq))
           (cl-find-if
            (lambda (str)
              (and (mapcar #'char-displayable-p (string-to-list str))))
            seq))
          ((characterp (car seq))
           (cl-find-if #'char-displayable-p seq)))))

(defun ein:markdown--marginalize-string (level)
  "Generate atx markup string of given LEVEL for left margin."
  (let ((margin-left-space-count
         (- ein:markdown-marginalize-headers-margin-width level)))
    (concat (make-string margin-left-space-count ? )
                           (make-string level ?#))))

(defun ein:markdown-fontify-headings (last)
  "Add text properties to headings from point to LAST."
  (when (ein:markdown-match-propertized-text 'ein:markdown-heading last)
    (let* ((level (ein:markdown-outline-level))
           (heading-face
            (intern (format "ein:markdown-header-face-%d" level)))
           (heading-props `(face ,heading-face))
           (left-markup-props
            `(face ein:markdown-header-delimiter-face
                   ,@(cond
                      (ein:markdown-marginalize-headers
                       `(display ((margin left-margin)
                                  ,(ein:markdown--marginalize-string level)))))))
           (right-markup-props
            `(face ein:markdown-header-delimiter-face))
           (rule-props `(face ein:markdown-header-rule-face)))
      (if (match-end 1)
          ;; Setext heading
          (progn (add-text-properties
                  (match-beginning 1) (match-end 1) heading-props)
                 (if (= level 1)
                     (add-text-properties
                      (match-beginning 2) (match-end 2) rule-props)
                   (add-text-properties
                    (match-beginning 3) (match-end 3) rule-props)))
        ;; atx heading
        (add-text-properties
         (match-beginning 4) (match-end 4) left-markup-props)
        (add-text-properties
         (match-beginning 5) (match-end 5) heading-props)
        (when (match-end 6)
          (add-text-properties
           (match-beginning 6) (match-end 6) right-markup-props))))
    t))

(defun ein:markdown-fontify-tables (last)
  (when (and (re-search-forward "|" last t)
             (ein:markdown-table-at-point-p))
    (font-lock-append-text-property
     (line-beginning-position) (min (1+ (line-end-position)) (point-max))
     'face 'ein:markdown-table-face)
    (forward-line 1)
    t))

(defun ein:markdown-fontify-blockquotes (last)
  "Apply font-lock properties to blockquotes from point to LAST."
  (when (ein:markdown-match-blockquotes last)
    (add-text-properties
     (match-beginning 1) (match-end 1)
     `(face ein:markdown-markup-face))
    (font-lock-append-text-property
     (match-beginning 0) (match-end 0) 'face 'ein:markdown-blockquote-face)
    t))

(defun ein:markdown-fontify-list-items (last)
  "Apply font-lock properties to list markers from point to LAST."
  (when (ein:markdown-match-list-items last)
    (add-text-properties
     (match-beginning 2) (match-end 2) '(face ein:markdown-list-face))
    t))

(defun ein:markdown-fontify-hrs (last)
  "Add text properties to horizontal rules from point to LAST."
  (when (ein:markdown-match-hr last)
    (add-text-properties
     (match-beginning 0) (match-end 0)
     `(face ein:markdown-hr-face
            font-lock-multiline t))
    t))

(defun ein:markdown-fontify-sub-superscripts (last)
  "Apply text properties to sub- and superscripts from point to LAST."
  (when (ein:markdown-search-until-condition
         (lambda () (and (not (ein:markdown-code-block-at-point-p))
                         (not (ein:markdown-inline-code-at-point-p))
                         (not (ein:markdown-in-comment-p))))
         ein:markdown-regex-sub-superscript last t)
    (let ((mp (list 'face 'ein:markdown-markup-face)))
      (add-text-properties (match-beginning 2) (match-end 2) mp)
      (add-text-properties (match-beginning 4) (match-end 4) mp)
      t)))


;;; Syntax Table ==============================================================

(defvar ein:markdown-mode-syntax-table
  (let ((tab (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\" "." tab)
    tab)
  "Syntax table for `ein:markdown-mode'.")


;;; Element Insertion =========================================================

(defun ein:markdown-ensure-blank-line-before ()
  "If previous line is not already blank, insert a blank line before point."
  (unless (bolp) (insert "\n"))
  (unless (or (bobp) (looking-back "\n\\s-*\n" nil)) (insert "\n")))

(defun ein:markdown-ensure-blank-line-after ()
  "If following line is not already blank, insert a blank line after point.
Return the point where it was originally."
  (save-excursion
    (unless (eolp) (insert "\n"))
    (unless (or (eobp) (looking-at-p "\n\\s-*\n")) (insert "\n"))))

(defun ein:markdown-wrap-or-insert (s1 s2 &optional thing beg end)
  "Insert the strings S1 and S2, wrapping around region or THING.
If a region is specified by the optional BEG and END arguments,
wrap the strings S1 and S2 around that region.
If there is an active region, wrap the strings S1 and S2 around
the region.  If there is not an active region but the point is at
THING, wrap that thing (which defaults to word).  Otherwise, just
insert S1 and S2 and place the point in between.  Return the
bounds of the entire wrapped string, or nil if nothing was wrapped
and S1 and S2 were only inserted."
  (let (a b bounds new-point)
    (cond
     ;; Given region
     ((and beg end)
      (setq a beg
            b end
            new-point (+ (point) (length s1))))
     ;; Active region
     ((ein:markdown-use-region-p)
      (setq a (region-beginning)
            b (region-end)
            new-point (+ (point) (length s1))))
     ;; Thing (word) at point
     ((setq bounds (ein:markdown-bounds-of-thing-at-point (or thing 'word)))
      (setq a (car bounds)
            b (cdr bounds)
            new-point (+ (point) (length s1))))
     ;; No active region and no word
     (t
      (setq a (point)
            b (point))))
    (goto-char b)
    (insert s2)
    (goto-char a)
    (insert s1)
    (when new-point (goto-char new-point))
    (if (= a b)
        nil
      (setq b (+ b (length s1) (length s2)))
      (cons a b))))

(defun ein:markdown-point-after-unwrap (cur prefix suffix)
  "Return desired position of point after an unwrapping operation.
CUR gives the position of the point before the operation.
Additionally, two cons cells must be provided.  PREFIX gives the
bounds of the prefix string and SUFFIX gives the bounds of the
suffix string."
  (cond ((< cur (cdr prefix)) (car prefix))
        ((< cur (car suffix)) (- cur (- (cdr prefix) (car prefix))))
        ((<= cur (cdr suffix))
         (- cur (+ (- (cdr prefix) (car prefix))
                   (- cur (car suffix)))))
        (t cur)))

(defun ein:markdown-unwrap-thing-at-point (regexp all text)
  "Remove prefix and suffix of thing at point and reposition the point.
When the thing at point matches REGEXP, replace the subexpression
ALL with the string in subexpression TEXT.  Reposition the point
in an appropriate location accounting for the removal of prefix
and suffix strings.  Return new bounds of string from group TEXT.
When REGEXP is nil, assumes match data is already set."
  (when (or (null regexp)
            (thing-at-point-looking-at regexp))
    (let ((cur (point))
          (prefix (cons (match-beginning all) (match-beginning text)))
          (suffix (cons (match-end text) (match-end all)))
          (bounds (cons (match-beginning text) (match-end text))))
      ;; Replace the thing at point
      (replace-match (match-string text) t t nil all)
      ;; Reposition the point
      (goto-char (ein:markdown-point-after-unwrap cur prefix suffix))
      ;; Adjust bounds
      (setq bounds (cons (car prefix)
                         (- (cdr bounds) (- (cdr prefix) (car prefix))))))))

(defun ein:markdown-unwrap-things-in-region (beg end regexp all text)
  "Remove prefix and suffix of all things in region from BEG to END.
When a thing in the region matches REGEXP, replace the
subexpression ALL with the string in subexpression TEXT.
Return a cons cell containing updated bounds for the region."
  (save-excursion
    (goto-char beg)
    (let ((removed 0) len-all len-text)
      (while (re-search-forward regexp (- end removed) t)
        (setq len-all (length (match-string-no-properties all)))
        (setq len-text (length (match-string-no-properties text)))
        (setq removed (+ removed (- len-all len-text)))
        (replace-match (match-string text) t t nil all))
      (cons beg (- end removed)))))

(defun ein:markdown-insert-hr (arg)
  "Insert or replace a horizonal rule.
By default, use the first element of `markdown-hr-strings'.  When
ARG is non-nil, as when given a prefix, select a different
element as follows.  When prefixed with \\[universal-argument],
use the last element of `markdown-hr-strings' instead.  When
prefixed with an integer from 1 to the length of
`markdown-hr-strings', use the element in that position instead."
  (interactive "*P")
  (when (thing-at-point-looking-at ein:markdown-regex-hr)
    (delete-region (match-beginning 0) (match-end 0)))
  (ein:markdown-ensure-blank-line-before)
  (cond ((equal arg '(4))
         (insert (car (reverse ein:markdown-hr-strings))))
        ((and (integerp arg) (> arg 0)
              (<= arg (length ein:markdown-hr-strings)))
         (insert (nth (1- arg) ein:markdown-hr-strings)))
        (t
         (insert (car ein:markdown-hr-strings))))
  (ein:markdown-ensure-blank-line-after))

(defun ein:markdown-insert-bold ()
  "Insert markup to make a region or word bold.
If there is an active region, make the region bold.  If the point
is at a non-bold word, make the word bold.  If the point is at a
bold word or phrase, remove the bold markup.  Otherwise, simply
insert bold delimiters and place the point in between them."
  (interactive)
  (let ((delim (if ein:markdown-bold-underscore "__" "**")))
    (if (ein:markdown-use-region-p)
        ;; Active region
        (let ((bounds (ein:markdown-unwrap-things-in-region
                       (region-beginning) (region-end)
                       ein:markdown-regex-bold 2 4)))
          (ein:markdown-wrap-or-insert delim delim nil (car bounds) (cdr bounds)))
      ;; Bold markup removal, bold word at point, or empty markup insertion
      (if (thing-at-point-looking-at ein:markdown-regex-bold)
          (ein:markdown-unwrap-thing-at-point nil 2 4)
        (ein:markdown-wrap-or-insert delim delim 'word nil nil)))))

(defun ein:markdown-insert-italic ()
  "Insert markup to make a region or word italic.
If there is an active region, make the region italic.  If the point
is at a non-italic word, make the word italic.  If the point is at an
italic word or phrase, remove the italic markup.  Otherwise, simply
insert italic delimiters and place the point in between them."
  (interactive)
  (let ((delim (if ein:markdown-italic-underscore "_" "*")))
    (if (ein:markdown-use-region-p)
        ;; Active region
        (let ((bounds (ein:markdown-unwrap-things-in-region
                       (region-beginning) (region-end)
                       ein:markdown-regex-italic 1 3)))
          (ein:markdown-wrap-or-insert delim delim nil (car bounds) (cdr bounds)))
      ;; Italic markup removal, italic word at point, or empty markup insertion
      (if (thing-at-point-looking-at ein:markdown-regex-italic)
          (ein:markdown-unwrap-thing-at-point nil 1 3)
        (ein:markdown-wrap-or-insert delim delim 'word nil nil)))))

(defun ein:markdown-insert-strike-through ()
  "Insert markup to make a region or word strikethrough.
If there is an active region, make the region strikethrough.  If the point
is at a non-bold word, make the word strikethrough.  If the point is at a
strikethrough word or phrase, remove the strikethrough markup.  Otherwise,
simply insert bold delimiters and place the point in between them."
  (interactive)
  (let ((delim "~~"))
    (if (ein:markdown-use-region-p)
        ;; Active region
        (let ((bounds (ein:markdown-unwrap-things-in-region
                       (region-beginning) (region-end)
                       ein:markdown-regex-strike-through 2 4)))
          (ein:markdown-wrap-or-insert delim delim nil (car bounds) (cdr bounds)))
      ;; Strikethrough markup removal, strikethrough word at point, or empty markup insertion
      (if (thing-at-point-looking-at ein:markdown-regex-strike-through)
          (ein:markdown-unwrap-thing-at-point nil 2 4)
        (ein:markdown-wrap-or-insert delim delim 'word nil nil)))))

(defun ein:markdown-insert-code ()
  "Insert markup to make a region or word an inline code fragment.
If there is an active region, make the region an inline code
fragment.  If the point is at a word, make the word an inline
code fragment.  Otherwise, simply insert code delimiters and
place the point in between them."
  (interactive)
  (if (ein:markdown-use-region-p)
      ;; Active region
      (let ((bounds (ein:markdown-unwrap-things-in-region
                     (region-beginning) (region-end)
                     ein:markdown-regex-code 1 3)))
        (ein:markdown-wrap-or-insert "`" "`" nil (car bounds) (cdr bounds)))
    ;; Code markup removal, code markup for word, or empty markup insertion
    (if (ein:markdown-inline-code-at-point)
        (ein:markdown-unwrap-thing-at-point nil 0 2)
      (ein:markdown-wrap-or-insert "`" "`" 'word nil nil))))

(defun ein:markdown-insert-kbd ()
  "Insert markup to wrap region or word in <kbd> tags.
If there is an active region, use the region.  If the point is at
a word, use the word.  Otherwise, simply insert <kbd> tags and
place the point in between them."
  (interactive)
  (if (ein:markdown-use-region-p)
      ;; Active region
      (let ((bounds (ein:markdown-unwrap-things-in-region
                     (region-beginning) (region-end)
                     ein:markdown-regex-kbd 0 2)))
        (ein:markdown-wrap-or-insert "<kbd>" "</kbd>" nil (car bounds) (cdr bounds)))
    ;; Markup removal, markup for word, or empty markup insertion
    (if (thing-at-point-looking-at ein:markdown-regex-kbd)
        (ein:markdown-unwrap-thing-at-point nil 0 2)
      (ein:markdown-wrap-or-insert "<kbd>" "</kbd>" 'word nil nil))))

(defun ein:markdown-insert-inline-link (text url &optional title)
  "Insert an inline link with TEXT pointing to URL.
Optionally, the user can provide a TITLE."
  (let ((cur (point)))
    (setq title (and title (concat " \"" title "\"")))
    (insert (concat "[" text "](" url title ")"))
    (cond ((not text) (goto-char (+ 1 cur)))
          ((not url) (goto-char (+ 3 (length text) cur))))))

(defun ein:markdown-insert-inline-image (text url &optional title)
  "Insert an inline link with alt TEXT pointing to URL.
Optionally, also provide a TITLE."
  (let ((cur (point)))
    (setq title (and title (concat " \"" title "\"")))
    (insert (concat "![" text "](" url title ")"))
    (cond ((not text) (goto-char (+ 2 cur)))
          ((not url) (goto-char (+ 4 (length text) cur))))))

(defun ein:markdown-insert-reference-link (text label &optional url title)
  "Insert a reference link and, optionally, a reference definition.
The link TEXT will be inserted followed by the optional LABEL.
If a URL is given, also insert a definition for the reference
LABEL according to `markdown-reference-location'.  If a TITLE is
given, it will be added to the end of the reference definition
and will be used to populate the title attribute when converted
to XHTML.  If URL is nil, insert only the link portion (for
example, when a reference label is already defined)."
  (insert (concat "[" text "][" label "]"))
  (when url
    (ein:markdown-insert-reference-definition
     (if (string-equal label "") text label)
     url title)))

(defun ein:markdown-insert-reference-image (text label &optional url title)
  "Insert a reference image and, optionally, a reference definition.
The alt TEXT will be inserted followed by the optional LABEL.
If a URL is given, also insert a definition for the reference
LABEL according to `markdown-reference-location'.  If a TITLE is
given, it will be added to the end of the reference definition
and will be used to populate the title attribute when converted
to XHTML.  If URL is nil, insert only the link portion (for
example, when a reference label is already defined)."
  (insert (concat "![" text "][" label "]"))
  (when url
    (ein:markdown-insert-reference-definition
     (if (string-equal label "") text label)
     url title)))

(defun ein:markdown-insert-reference-definition (label &optional url title)
  "Add definition for reference LABEL with URL and TITLE.
LABEL is a ein:markdown reference label without square brackets.
URL and TITLE are optional.  When given, the TITLE will
be used to populate the title attribute when converted to XHTML."
  ;; END specifies where to leave the point upon return
  (let ((end (point)))
    (cl-case ein:markdown-reference-location
      (end         (goto-char (point-max)))
      (immediately (ein:markdown-end-of-text-block))
      (subtree     (ein:markdown-end-of-subtree))
      (header      (ein:markdown-end-of-defun)))
    ;; Skip backwards over local variables.  This logic is similar to the one
    ;; used in ‘hack-local-variables’.
    (when (and enable-local-variables (eobp))
      (search-backward "\n\f" (max (- (point) 3000) (point-min)) :move)
      (when (let ((case-fold-search t))
              (search-forward "Local Variables:" nil :move))
        (beginning-of-line 0)
        (when (eq (char-before) ?\n) (backward-char))))
    (unless (or (ein:markdown-cur-line-blank-p)
                (thing-at-point-looking-at ein:markdown-regex-reference-definition))
      (insert "\n"))
    (insert "\n[" label "]: ")
    (if url
        (insert url)
      ;; When no URL is given, leave point at END following the colon
      (setq end (point)))
    (when (> (length title) 0)
      (insert " \"" title "\""))
    (unless (looking-at-p "\n")
      (insert "\n"))
    (goto-char end)
    (when url
      (message
       (ein:markdown--substitute-command-keys
        "Reference [%s] was defined, press \\[ein:markdown-do] to jump there")
       label))))

(define-obsolete-function-alias
  'ein:markdown-insert-inline-link-dwim 'ein:markdown-insert-link "v2.3")
(define-obsolete-function-alias
  'ein:markdown-insert-reference-link-dwim 'ein:markdown-insert-link "v2.3")

(defun ein:markdown--insert-link-or-image (image)
  "Interactively insert new or update an existing link or image.
When IMAGE is non-nil, insert an image.  Otherwise, insert a link.
This is an internal function called by
`markdown-insert-link' and `markdown-insert-image'."
  (cl-multiple-value-bind (begin end text uri ref title)
      (if (ein:markdown-use-region-p)
          ;; Use region as either link text or URL as appropriate.
          (let ((region (buffer-substring-no-properties
                         (region-beginning) (region-end))))
            (if (string-match ein:markdown-regex-uri region)
                ;; Region contains a URL; use it as such.
                (list (region-beginning) (region-end)
                      nil (match-string 0 region) nil nil)
              ;; Region doesn't contain a URL, so use it as text.
              (list (region-beginning) (region-end)
                    region nil nil nil)))
        ;; Extract and use properties of existing link, if any.
        (ein:markdown-link-at-pos (point)))
    (let* ((ref (when ref (concat "[" ref "]")))
           (defined-refs (append
                          (mapcar (lambda (ref) (concat "[" ref "]"))
                                  (mapcar #'car (ein:markdown-get-defined-references)))))
           (used-uris (ein:markdown-get-used-uris))
           (uri-or-ref (completing-read
                        "URL or [reference]: "
                        (append defined-refs used-uris)
                        nil nil (or uri ref)))
           (ref (cond ((string-match "\\`\\[\\(.*\\)\\]\\'" uri-or-ref)
                       (match-string 1 uri-or-ref))
                      ((string-equal "" uri-or-ref)
                       "")))
           (uri (unless ref uri-or-ref))
           (text-prompt (if image
                            "Alt text: "
                          (if ref
                              "Link text: "
                            "Link text (blank for plain URL): ")))
           (text (read-string text-prompt text))
           (text (if (= (length text) 0) nil text))
           (plainp (and uri (not text)))
           (implicitp (string-equal ref ""))
           (ref (if implicitp text ref))
           (definedp (and ref (ein:markdown-reference-definition ref)))
           (ref-url (unless (or uri definedp)
                      (completing-read "Reference URL: " used-uris)))
           (title (unless (or plainp definedp)
                    (read-string "Title (tooltip text, optional): " title)))
           (title (if (= (length title) 0) nil title)))
      (when (and image implicitp)
        (user-error "Reference required: implicit image references are invalid"))
      (when (and begin end)
        (delete-region begin end))
      (cond
       ((and (not image) uri text)
        (ein:markdown-insert-inline-link text uri title))
       ((and image uri text)
        (ein:markdown-insert-inline-image text uri title))
       ((and ref text)
        (if image
            (ein:markdown-insert-reference-image text (unless implicitp ref) nil title)
          (ein:markdown-insert-reference-link text (unless implicitp ref) nil title))
        (unless definedp
          (ein:markdown-insert-reference-definition ref ref-url title)))
       ((and (not image) uri)
        (ein:markdown-insert-uri uri))))))

(defun ein:markdown-insert-link ()
  "Insert new or update an existing link, with interactive prompts.
If the point is at an existing link or URL, update the link text,
URL, reference label, and/or title.  Otherwise, insert a new link.
The type of link inserted (inline, reference, or plain URL)
depends on which values are provided:

*   If a URL and TEXT are given, insert an inline link: [TEXT](URL).
*   If [REF] and TEXT are given, insert a reference link: [TEXT][REF].
*   If only TEXT is given, insert an implicit reference link: [TEXT][].
*   If only a URL is given, insert a plain link: <URL>.

In other words, to create an implicit reference link, leave the
URL prompt empty and to create a plain URL link, leave the link
text empty.

If there is an active region, use the text as the default URL, if
it seems to be a URL, or link text value otherwise.

If a given reference is not defined, this function will
additionally prompt for the URL and optional title.  In this case,
the reference definition is placed at the location determined by
`markdown-reference-location'.

Through updating the link, this function can be used to convert a
link of one type (inline, reference, or plain) to another type by
selectively adding or removing information via the prompts."
  (interactive)
  (ein:markdown--insert-link-or-image nil))

(defun ein:markdown-insert-image ()
  "Insert new or update an existing image, with interactive prompts.
If the point is at an existing image, update the alt text, URL,
reference label, and/or title. Otherwise, insert a new image.
The type of image inserted (inline or reference) depends on which
values are provided:

*   If a URL and ALT-TEXT are given, insert an inline image:
    ![ALT-TEXT](URL).
*   If [REF] and ALT-TEXT are given, insert a reference image:
    ![ALT-TEXT][REF].

If there is an active region, use the text as the default URL, if
it seems to be a URL, or alt text value otherwise.

If a given reference is not defined, this function will
additionally prompt for the URL and optional title.  In this case,
the reference definition is placed at the location determined by
`markdown-reference-location'.

Through updating the image, this function can be used to convert an
image of one type (inline or reference) to another type by
selectively adding or removing information via the prompts."
  (interactive)
  (ein:markdown--insert-link-or-image t))

(defun ein:markdown-insert-uri (&optional uri)
  "Insert markup for an inline URI.
If there is an active region, use it as the URI.  If the point is
at a URI, wrap it with angle brackets.  If the point is at an
inline URI, remove the angle brackets.  Otherwise, simply insert
angle brackets place the point between them."
  (interactive)
  (if (ein:markdown-use-region-p)
      ;; Active region
      (let ((bounds (ein:markdown-unwrap-things-in-region
                     (region-beginning) (region-end)
                     ein:markdown-regex-angle-uri 0 2)))
        (ein:markdown-wrap-or-insert "<" ">" nil (car bounds) (cdr bounds)))
    ;; Markup removal, URI at point, new URI, or empty markup insertion
    (if (thing-at-point-looking-at ein:markdown-regex-angle-uri)
        (ein:markdown-unwrap-thing-at-point nil 0 2)
      (if uri
          (insert "<" uri ">")
        (ein:markdown-wrap-or-insert "<" ">" 'url nil nil)))))

(defun ein:markdown-remove-header ()
  "Remove header markup if point is at a header.
Return bounds of remaining header text if a header was removed
and nil otherwise."
  (interactive "*")
  (or (ein:markdown-unwrap-thing-at-point ein:markdown-regex-header-atx 0 2)
      (ein:markdown-unwrap-thing-at-point ein:markdown-regex-header-setext 0 1)))

(defun ein:markdown-insert-header (&optional level text setext)
  "Insert or replace header markup.
The level of the header is specified by LEVEL and header text is
given by TEXT.  LEVEL must be an integer from 1 and 6, and the
default value is 1.
When TEXT is nil, the header text is obtained as follows.
If there is an active region, it is used as the header text.
Otherwise, the current line will be used as the header text.
If there is not an active region and the point is at a header,
remove the header markup and replace with level N header.
Otherwise, insert empty header markup and place the point in
between.
The style of the header will be atx (hash marks) unless
SETEXT is non-nil, in which case a setext-style (underlined)
header will be inserted."
  (interactive "p\nsHeader text: ")
  (setq level (min (max (or level 1) 1) (if setext 2 6)))
  ;; Determine header text if not given
  (when (null text)
    (if (ein:markdown-use-region-p)
        ;; Active region
        (setq text (delete-and-extract-region (region-beginning) (region-end)))
      ;; No active region
      (ein:markdown-remove-header)
      (setq text (delete-and-extract-region
                  (line-beginning-position) (line-end-position)))
      (when (and setext (string-match-p "^[ \t]*$" text))
        (setq text (read-string "Header text: "))))
    (setq text (ein:markdown-compress-whitespace-string text)))
  ;; Insertion with given text
  (ein:markdown-ensure-blank-line-before)
  (let (hdr)
    (cond (setext
           (setq hdr (make-string (string-width text) (if (= level 2) ?- ?=)))
           (insert text "\n" hdr))
          (t
           (setq hdr (make-string level ?#))
           (insert hdr " " text)
           (when (null ein:markdown-asymmetric-header) (insert " " hdr)))))
  (ein:markdown-ensure-blank-line-after)
  ;; Leave point at end of text
  (cond (setext
         (backward-char (1+ (string-width text))))
        ((null ein:markdown-asymmetric-header)
         (backward-char (1+ level)))))

(defun ein:markdown-insert-header-dwim (&optional arg setext)
  "Insert or replace header markup.
The level and type of the header are determined automatically by
the type and level of the previous header, unless a prefix
argument is given via ARG.
With a numeric prefix valued 1 to 6, insert a header of the given
level, with the type being determined automatically (note that
only level 1 or 2 setext headers are possible).

With a \\[universal-argument] prefix (i.e., when ARG is (4)),
promote the heading by one level.
With two \\[universal-argument] prefixes (i.e., when ARG is (16)),
demote the heading by one level.
When SETEXT is non-nil, prefer setext-style headers when
possible (levels one and two).

When there is an active region, use it for the header text.  When
the point is at an existing header, change the type and level
according to the rules above.
Otherwise, if the line is not empty, create a header using the
text on the current line as the header text.
Finally, if the point is on a blank line, insert empty header
markup (atx) or prompt for text (setext).
See `markdown-insert-header' for more details about how the
header text is determined."
  (interactive "*P")
  (let (level)
    (save-excursion
      (when (or (thing-at-point-looking-at ein:markdown-regex-header)
                (re-search-backward ein:markdown-regex-header nil t))
        ;; level of current or previous header
        (setq level (ein:markdown-outline-level))
        ;; match group 1 indicates a setext header
        (setq setext (match-end 1))))
    ;; check prefix argument
    (cond
     ((and (equal arg '(4)) level (> level 1)) ;; C-u
      (cl-decf level))
     ((and (equal arg '(16)) level (< level 6)) ;; C-u C-u
      (cl-incf level))
     (arg ;; numeric prefix
      (setq level (prefix-numeric-value arg))))
    ;; setext headers must be level one or two
    (and level (setq setext (and setext (<= level 2))))
    ;; insert the heading
    (ein:markdown-insert-header level nil setext)))

(defun ein:markdown-insert-header-setext-dwim (&optional arg)
  "Insert or replace header markup, with preference for setext.
See `markdown-insert-header-dwim' for details, including how ARG is handled."
  (interactive "*P")
  (ein:markdown-insert-header-dwim arg t))

(defun ein:markdown-insert-header-atx-1 ()
  "Insert a first level atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (ein:markdown-insert-header 1 nil nil))

(defun ein:markdown-insert-header-atx-2 ()
  "Insert a level two atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (ein:markdown-insert-header 2 nil nil))

(defun ein:markdown-insert-header-atx-3 ()
  "Insert a level three atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (ein:markdown-insert-header 3 nil nil))

(defun ein:markdown-insert-header-atx-4 ()
  "Insert a level four atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (ein:markdown-insert-header 4 nil nil))

(defun ein:markdown-insert-header-atx-5 ()
  "Insert a level five atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (ein:markdown-insert-header 5 nil nil))

(defun ein:markdown-insert-header-atx-6 ()
  "Insert a sixth level atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (ein:markdown-insert-header 6 nil nil))

(defun ein:markdown-insert-header-setext-1 ()
  "Insert a setext-style (underlined) first-level header.
See `markdown-insert-header'."
  (interactive "*")
  (ein:markdown-insert-header 1 nil t))

(defun ein:markdown-insert-header-setext-2 ()
  "Insert a setext-style (underlined) second-level header.
See `markdown-insert-header'."
  (interactive "*")
  (ein:markdown-insert-header 2 nil t))

(defun ein:markdown-blockquote-indentation (loc)
  "Return string containing necessary indentation for a blockquote at LOC.
Also see `markdown-pre-indentation'."
  (save-excursion
    (goto-char loc)
    (let* ((list-level (length (ein:markdown-calculate-list-levels)))
           (indent ""))
      (dotimes (_ list-level indent)
        (setq indent (concat indent "    "))))))

(defun ein:markdown-insert-blockquote ()
  "Start a blockquote section (or blockquote the region).
If Transient Mark mode is on and a region is active, it is used as
the blockquote text."
  (interactive)
  (if (ein:markdown-use-region-p)
      (ein:markdown-blockquote-region (region-beginning) (region-end))
    (ein:markdown-ensure-blank-line-before)
    (insert (ein:markdown-blockquote-indentation (point)) "> ")
    (ein:markdown-ensure-blank-line-after)))

(defun ein:markdown-block-region (beg end prefix)
  "Format the region using a block prefix.
Arguments BEG and END specify the beginning and end of the
region.  The characters PREFIX will appear at the beginning
of each line."
  (save-excursion
    (let* ((end-marker (make-marker))
           (beg-marker (make-marker))
           (prefix-without-trailing-whitespace
            (replace-regexp-in-string (rx (+ blank) eos) "" prefix)))
      ;; Ensure blank line after and remove extra whitespace
      (goto-char end)
      (skip-syntax-backward "-")
      (set-marker end-marker (point))
      (delete-horizontal-space)
      (ein:markdown-ensure-blank-line-after)
      ;; Ensure blank line before and remove extra whitespace
      (goto-char beg)
      (skip-syntax-forward "-")
      (delete-horizontal-space)
      (ein:markdown-ensure-blank-line-before)
      (set-marker beg-marker (point))
      ;; Insert PREFIX before each line
      (goto-char beg-marker)
      (while (and (< (line-beginning-position) end-marker)
                  (not (eobp)))
        ;; Don’t insert trailing whitespace.
        (insert (if (eolp) prefix-without-trailing-whitespace prefix))
        (forward-line)))))

(defun ein:markdown-blockquote-region (beg end)
  "Blockquote the region.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (ein:markdown-block-region
   beg end (concat (ein:markdown-blockquote-indentation
                    (max (point-min) (1- beg))) "> ")))

(defun ein:markdown-pre-indentation (loc)
  "Return string containing necessary whitespace for a pre block at LOC.
Also see `markdown-blockquote-indentation'."
  (save-excursion
    (goto-char loc)
    (let* ((list-level (length (ein:markdown-calculate-list-levels)))
           indent)
      (dotimes (_ (1+ list-level) indent)
        (setq indent (concat indent "    "))))))

(defun ein:markdown-insert-pre ()
  "Start a preformatted section (or apply to the region).
If Transient Mark mode is on and a region is active, it is marked
as preformatted text."
  (interactive)
  (if (ein:markdown-use-region-p)
      (ein:markdown-pre-region (region-beginning) (region-end))
    (ein:markdown-ensure-blank-line-before)
    (insert (ein:markdown-pre-indentation (point)))
    (ein:markdown-ensure-blank-line-after)))

(defun ein:markdown-pre-region (beg end)
  "Format the region as preformatted text.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (let ((indent (ein:markdown-pre-indentation (max (point-min) (1- beg)))))
    (ein:markdown-block-region beg end indent)))

(defun ein:markdown-electric-backquote (arg)
  "Insert a backquote.
The numeric prefix argument ARG says how many times to repeat the insertion.
Call `markdown-insert-gfm-code-block' interactively
if three backquotes inserted at the beginning of line."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg)))

(defun ein:markdown-trim-whitespace (str)
  (ein:markdown-replace-regexp-in-string
   "\\(?:[[:space:]\r\n]+\\'\\|\\`[[:space:]\r\n]+\\)" "" str))

(defun ein:markdown-clean-language-string (str)
  (ein:markdown-replace-regexp-in-string
   "{\\.?\\|}" "" (ein:markdown-trim-whitespace str)))

(defun ein:markdown-validate-language-string (widget)
  (let ((str (widget-value widget)))
    (unless (string= str (ein:markdown-clean-language-string str))
      (widget-put widget :error (format "Invalid language spec: '%s'" str))
      widget)))

(defcustom ein:markdown-spaces-after-code-fence 1
  "Number of space characters to insert after a code fence.
\\<gfm-mode-map>\\[ein:markdown-insert-gfm-code-block] inserts this many spaces between an
opening code fence and an info string."
  :group 'ein:markdown
  :type 'integer
  :safe #'natnump
  :package-version '(ein:markdown-mode . "2.3"))

(defun ein:markdown-code-block-lang (&optional pos-prop)
  "Return the language name for a GFM or tilde fenced code block.
The beginning of the block may be described by POS-PROP,
a cons of (pos . prop) giving the position and property
at the beginning of the block."
  (or pos-prop
      (setq pos-prop
            (ein:markdown-max-of-seq
             #'car
             (cl-remove-if
              #'null
              (cl-mapcar
               #'ein:markdown-find-previous-prop
               (ein:markdown-get-fenced-block-begin-properties))))))
  (when pos-prop
    (goto-char (car pos-prop))
    (set-match-data (get-text-property (point) (cdr pos-prop)))
    ;; Note: Hard-coded group number assumes tilde
    ;; and GFM fenced code regexp groups agree.
    (let ((begin (match-beginning 3))
          (end (match-end 3)))
      (when (and begin end)
        ;; Fix language strings beginning with periods, like ".ruby".
        (when (eq (char-after begin) ?.)
          (setq begin (1+ begin)))
        (buffer-substring-no-properties begin end)))))

;;; Footnotes =================================================================

(defun ein:markdown-footnote-counter-inc ()
  "Increment `markdown-footnote-counter' and return the new value."
  (when (= ein:markdown-footnote-counter 0) ; hasn't been updated in this buffer yet.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\[\\^\\(" ein:markdown-footnote-chars "*?\\)\\]:")
                                (point-max) t)
        (let ((fn (string-to-number (match-string 1))))
          (when (> fn ein:markdown-footnote-counter)
            (setq ein:markdown-footnote-counter fn))))))
  (cl-incf ein:markdown-footnote-counter))

(defun ein:markdown-insert-footnote ()
  "Insert footnote with a new number and move point to footnote definition."
  (interactive)
  (let ((fn (ein:markdown-footnote-counter-inc)))
    (insert (format "[^%d]" fn))
    (ein:markdown-footnote-text-find-new-location)
    (ein:markdown-ensure-blank-line-before)
    (unless (ein:markdown-cur-line-blank-p)
      (insert "\n"))
    (insert (format "[^%d]: " fn))
    (ein:markdown-ensure-blank-line-after)))

(defun ein:markdown-footnote-text-find-new-location ()
  "Position the point at the proper location for a new footnote text."
  (cond
   ((eq ein:markdown-footnote-location 'end) (goto-char (point-max)))
   ((eq ein:markdown-footnote-location 'immediately) (ein:markdown-end-of-text-block))
   ((eq ein:markdown-footnote-location 'subtree) (ein:markdown-end-of-subtree))
   ((eq ein:markdown-footnote-location 'header) (ein:markdown-end-of-defun))))

(defun ein:markdown-footnote-kill ()
  "Kill the footnote at point.
The footnote text is killed (and added to the kill ring), the
footnote marker is deleted.  Point has to be either at the
footnote marker or in the footnote text."
  (interactive)
  (let ((marker-pos nil)
        (skip-deleting-marker nil)
        (starting-footnote-text-positions
         (ein:markdown-footnote-text-positions)))
    (when starting-footnote-text-positions
      ;; We're starting in footnote text, so mark our return position and jump
      ;; to the marker if possible.
      (let ((marker-pos (ein:markdown-footnote-find-marker
                         (cl-first starting-footnote-text-positions))))
        (if marker-pos
            (goto-char (1- marker-pos))
          ;; If there isn't a marker, we still want to kill the text.
          (setq skip-deleting-marker t))))
    ;; Either we didn't start in the text, or we started in the text and jumped
    ;; to the marker. We want to assume we're at the marker now and error if
    ;; we're not.
    (unless skip-deleting-marker
      (let ((marker (ein:markdown-footnote-delete-marker)))
        (unless marker
          (error "Not at a footnote"))
        ;; Even if we knew the text position before, it changed when we deleted
        ;; the label.
        (setq marker-pos (cl-second marker))
        (let ((new-text-pos (ein:markdown-footnote-find-text (cl-first marker))))
          (unless new-text-pos
            (error "No text for footnote `%s'" (cl-first marker)))
          (goto-char new-text-pos))))
    (let ((pos (ein:markdown-footnote-kill-text)))
      (goto-char (if starting-footnote-text-positions
                     pos
                   marker-pos)))))

(defun ein:markdown-footnote-delete-marker ()
  "Delete a footnote marker at point.
Returns a list (ID START) containing the footnote ID and the
start position of the marker before deletion.  If no footnote
marker was deleted, this function returns NIL."
  (let ((marker (ein:markdown-footnote-marker-positions)))
    (when marker
      (delete-region (cl-second marker) (cl-third marker))
      (butlast marker))))

(defun ein:markdown-footnote-kill-text ()
  "Kill footnote text at point.
Returns the start position of the footnote text before deletion,
or NIL if point was not inside a footnote text.

The killed text is placed in the kill ring (without the footnote
number)."
  (let ((fn (ein:markdown-footnote-text-positions)))
    (when fn
      (let ((text (delete-and-extract-region (cl-second fn) (cl-third fn))))
        (string-match (concat "\\[\\" (cl-first fn) "\\]:[[:space:]]*\\(\\(.*\n?\\)*\\)") text)
        (kill-new (match-string 1 text))
        (when (and (ein:markdown-cur-line-blank-p)
                   (ein:markdown-prev-line-blank-p)
                   (not (bobp)))
          (delete-region (1- (point)) (point)))
        (cl-second fn)))))

(defun ein:markdown-footnote-goto-text ()
  "Jump to the text of the footnote at point."
  (interactive)
  (let ((fn (car (ein:markdown-footnote-marker-positions))))
    (unless fn
      (user-error "Not at a footnote marker"))
    (let ((new-pos (ein:markdown-footnote-find-text fn)))
      (unless new-pos
        (error "No definition found for footnote `%s'" fn))
      (goto-char new-pos))))

(defun ein:markdown-footnote-return ()
  "Return from a footnote to its footnote number in the main text."
  (interactive)
  (let ((fn (save-excursion
              (car (ein:markdown-footnote-text-positions)))))
    (unless fn
      (user-error "Not in a footnote"))
    (let ((new-pos (ein:markdown-footnote-find-marker fn)))
      (unless new-pos
        (error "Footnote marker `%s' not found" fn))
      (goto-char new-pos))))

(defun ein:markdown-footnote-find-marker (id)
  "Find the location of the footnote marker with ID.
The actual buffer position returned is the position directly
following the marker's closing bracket.  If no marker is found,
NIL is returned."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "\\[" id "\\]\\([^:]\\|\\'\\)") nil t)
      (skip-chars-backward "^]")
      (point))))

(defun ein:markdown-footnote-find-text (id)
  "Find the location of the text of footnote ID.
The actual buffer position returned is the position of the first
character of the text, after the footnote's identifier.  If no
footnote text is found, NIL is returned."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^ \\{0,3\\}\\[" id "\\]:") nil t)
      (skip-chars-forward "[ \t]")
      (point))))

(defun ein:markdown-footnote-marker-positions ()
  "Return the position and ID of the footnote marker point is on.
The return value is a list (ID START END).  If point is not on a
footnote, NIL is returned."
  ;; first make sure we're at a footnote marker
  (if (or (looking-back (concat "\\[\\^" ein:markdown-footnote-chars "*\\]?") (line-beginning-position))
          (looking-at-p (concat "\\[?\\^" ein:markdown-footnote-chars "*?\\]")))
      (save-excursion
        ;; move point between [ and ^:
        (if (looking-at-p "\\[")
            (forward-char 1)
          (skip-chars-backward "^["))
        (looking-at (concat "\\(\\^" ein:markdown-footnote-chars "*?\\)\\]"))
        (list (match-string 1) (1- (match-beginning 1)) (1+ (match-end 1))))))

(defun ein:markdown-footnote-text-positions ()
  "Return the start and end positions of the footnote text point is in.
The exact return value is a list of three elements: (ID START END).
The start position is the position of the opening bracket
of the footnote id.  The end position is directly after the
newline that ends the footnote.  If point is not in a footnote,
NIL is returned instead."
  (save-excursion
    (let (result)
      (move-beginning-of-line 1)
      ;; Try to find the label. If we haven't found the label and we're at a blank
      ;; or indented line, back up if possible.
      (while (and
              (not (and (looking-at ein:markdown-regex-footnote-definition)
                        (setq result (list (match-string 1) (point)))))
              (and (not (bobp))
                   (or (ein:markdown-cur-line-blank-p)
                       (>= (current-indentation) 4))))
        (forward-line -1))
      (when result
        ;; Advance if there is a next line that is either blank or indented.
        ;; (Need to check if we're on the last line, because
        ;; ein:markdown-next-line-blank-p returns true for last line in buffer.)
        (while (and (/= (line-end-position) (point-max))
                    (or (ein:markdown-next-line-blank-p)
                        (>= (ein:markdown-next-line-indent) 4)))
          (forward-line))
        ;; Move back while the current line is blank.
        (while (ein:markdown-cur-line-blank-p)
          (forward-line -1))
        ;; Advance to capture this line and a single trailing newline (if there
        ;; is one).
        (forward-line)
        (append result (list (point)))))))

(defun ein:markdown-get-defined-footnotes ()
  "Return a list of all defined footnotes.
Result is an alist of pairs (MARKER . LINE), where MARKER is the
footnote marker, a string, and LINE is the line number containing
the footnote definition.

For example, suppose the following footnotes are defined at positions
448 and 475:

\[^1]: First footnote here.
\[^marker]: Second footnote.

Then the returned list is: ((\"^1\" . 478) (\"^marker\" . 475))"
  (save-excursion
    (goto-char (point-min))
    (let (footnotes)
      (while (ein:markdown-search-until-condition
              (lambda () (and (not (ein:markdown-code-block-at-point-p))
                              (not (ein:markdown-inline-code-at-point-p))
                              (not (ein:markdown-in-comment-p))))
              ein:markdown-regex-footnote-definition nil t)
        (let ((marker (match-string-no-properties 1))
              (pos (match-beginning 0)))
          (unless (zerop (length marker))
            (cl-pushnew (cons marker pos) footnotes :test #'equal))))
      (reverse footnotes))))


;;; Element Removal ===========================================================

(defun ein:markdown-kill-thing-at-point ()
  "Kill thing at point and add important text, without markup, to kill ring.
Possible things to kill include (roughly in order of precedence):
inline code, headers, horizonal rules, links (add link text to
kill ring), images (add alt text to kill ring), angle uri, email
addresses, bold, italics, reference definition (add URI to kill
ring), footnote markers and text (kill both marker and text, add
text to kill ring), and list items."
  (interactive "*")
  (let (val)
    (cond
     ;; Inline code
     ((ein:markdown-inline-code-at-point)
      (kill-new (match-string 2))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; ATX header
     ((thing-at-point-looking-at ein:markdown-regex-header-atx)
      (kill-new (match-string 2))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Setext header
     ((thing-at-point-looking-at ein:markdown-regex-header-setext)
      (kill-new (match-string 1))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Horizonal rule
     ((thing-at-point-looking-at ein:markdown-regex-hr)
      (kill-new (match-string 0))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Inline link or image (add link or alt text to kill ring)
     ((thing-at-point-looking-at ein:markdown-regex-link-inline)
      (kill-new (match-string 3))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Reference link or image (add link or alt text to kill ring)
     ((thing-at-point-looking-at ein:markdown-regex-link-reference)
      (kill-new (match-string 3))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Angle URI (add URL to kill ring)
     ((thing-at-point-looking-at ein:markdown-regex-angle-uri)
      (kill-new (match-string 2))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Email address in angle brackets (add email address to kill ring)
     ((thing-at-point-looking-at ein:markdown-regex-email)
      (kill-new (match-string 1))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Bold
     ((thing-at-point-looking-at ein:markdown-regex-bold)
      (kill-new (match-string 4))
      (delete-region (match-beginning 2) (match-end 2)))
     ;; Italics
     ((thing-at-point-looking-at ein:markdown-regex-italic)
      (kill-new (match-string 3))
      (delete-region (match-beginning 1) (match-end 1)))
     ;; Strikethrough
     ((thing-at-point-looking-at ein:markdown-regex-strike-through)
      (kill-new (match-string 4))
      (delete-region (match-beginning 2) (match-end 2)))
     ;; Footnote marker (add footnote text to kill ring)
     ((thing-at-point-looking-at ein:markdown-regex-footnote)
      (ein:markdown-footnote-kill))
     ;; Footnote text (add footnote text to kill ring)
     ((setq val (ein:markdown-footnote-text-positions))
      (ein:markdown-footnote-kill))
     ;; Reference definition (add URL to kill ring)
     ((thing-at-point-looking-at ein:markdown-regex-reference-definition)
      (kill-new (match-string 5))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; List item
     ((setq val (ein:markdown-cur-list-item-bounds))
      (kill-new (delete-and-extract-region (cl-first val) (cl-second val))))
     (t
      (user-error "Nothing found at point to kill")))))

(defun ein:markdown-kill-outline ()
  "Kill visible heading and add it to `kill-ring'."
  (interactive)
  (save-excursion
    (ein:markdown-outline-previous)
    (kill-region (point) (progn (ein:markdown-outline-next) (point)))))

(defun ein:markdown-kill-block ()
  "Kill visible code block, list item, or blockquote and add it to `kill-ring'."
  (interactive)
  (save-excursion
    (ein:markdown-backward-block)
    (kill-region (point) (progn (ein:markdown-forward-block) (point)))))


;;; Indentation ===============================================================

(defun ein:markdown-indent-find-next-position (cur-pos positions)
  "Return the position after the index of CUR-POS in POSITIONS.
Positions are calculated by `markdown-calc-indents'."
  (while (and positions
              (not (equal cur-pos (car positions))))
    (setq positions (cdr positions)))
  (or (cadr positions) 0))

(define-obsolete-function-alias 'ein:markdown-exdent-find-next-position
  'ein:markdown-outdent-find-next-position "v2.3")

(defun ein:markdown-outdent-find-next-position (cur-pos positions)
  "Return the maximal element that precedes CUR-POS from POSITIONS.
Positions are calculated by `markdown-calc-indents'."
  (let ((result 0))
    (dolist (i positions)
      (when (< i cur-pos)
        (setq result (max result i))))
    result))

(defun ein:markdown-indent-line ()
  "Indent the current line using some heuristics.
If the _previous_ command was either `markdown-enter-key' or
`markdown-cycle', then we should cycle to the next
reasonable indentation position.  Otherwise, we could have been
called directly by `markdown-enter-key', by an initial call of
`markdown-cycle', or indirectly by `auto-fill-mode'.  In
these cases, indent to the default position.
Positions are calculated by `markdown-calc-indents'."
  (interactive)
  (let ((positions (ein:markdown-calc-indents))
        (point-pos (current-column))
        (_ (back-to-indentation))
        (cur-pos (current-column)))
    (if (not (equal this-command 'ein:markdown-cycle))
        (indent-line-to (car positions))
      (setq positions (sort (delete-dups positions) '<))
      (let* ((next-pos (ein:markdown-indent-find-next-position cur-pos positions))
             (new-point-pos (max (+ point-pos (- next-pos cur-pos)) 0)))
        (indent-line-to next-pos)
        (move-to-column new-point-pos)))))

(defun ein:markdown-calc-indents ()
  "Return a list of indentation columns to cycle through.
The first element in the returned list should be considered the
default indentation level.  This function does not worry about
duplicate positions, which are handled up by calling functions."
  (let (pos prev-line-pos positions)

    ;; Indentation of previous line
    (setq prev-line-pos (ein:markdown-prev-line-indent))
    (setq positions (cons prev-line-pos positions))

    ;; Indentation of previous non-list-marker text
    (when (setq pos (save-excursion
                      (forward-line -1)
                      (when (looking-at ein:markdown-regex-list)
                        (- (match-end 3) (match-beginning 0)))))
      (setq positions (cons pos positions)))

    ;; Indentation required for a pre block in current context
    (setq pos (length (ein:markdown-pre-indentation (point))))
    (setq positions (cons pos positions))

    ;; Indentation of the previous line + tab-width
    (if prev-line-pos
        (setq positions (cons (+ prev-line-pos tab-width) positions))
      (setq positions (cons tab-width positions)))

    ;; Indentation of the previous line - tab-width
    (if (and prev-line-pos (> prev-line-pos tab-width))
        (setq positions (cons (- prev-line-pos tab-width) positions)))

    ;; Indentation of all preceeding list markers (when in a list)
    (when (setq pos (ein:markdown-calculate-list-levels))
      (setq positions (append pos positions)))

    ;; First column
    (setq positions (cons 0 positions))

    ;; Return reversed list
    (reverse positions)))

(defun ein:markdown-enter-key ()
  "Handle RET depending on the context.
If the point is at a table, move to the next row.  Otherwise,
indent according to value of `markdown-indent-on-enter'.
When it is nil, simply call `newline'.  Otherwise, indent the next line
following RET using `markdown-indent-line'.  Furthermore, when it
is set to 'indent-and-new-item and the point is in a list item,
start a new item with the same indentation. If the point is in an
empty list item, remove it (so that pressing RET twice when in a
list simply adds a blank line)."
  (interactive)
  (cond
   ;; Table
   ((ein:markdown-table-at-point-p)
    (call-interactively #'ein:markdown-table-next-row))
   ;; Indent non-table text
   (ein:markdown-indent-on-enter
    (let (bounds)
      (if (and (memq ein:markdown-indent-on-enter '(indent-and-new-item))
               (setq bounds (ein:markdown-cur-list-item-bounds)))
          (let ((beg (cl-first bounds))
                (end (cl-second bounds))
                (length (cl-fourth bounds)))
            ;; Point is in a list item
            (if (= (- end beg) length)
                ;; Delete blank list
                (progn
                  (delete-region beg end)
                  (newline)
                  (ein:markdown-indent-line))
              (call-interactively #'ein:markdown-insert-list-item)))
        ;; Point is not in a list
        (newline)
        (ein:markdown-indent-line))))
   ;; Insert a raw newline
   (t (newline))))

(define-obsolete-function-alias 'ein:markdown-exdent-or-delete
  'ein:markdown-outdent-or-delete "v2.3")

(defun ein:markdown-outdent-or-delete (arg)
  "Handle BACKSPACE by cycling through indentation points.
When BACKSPACE is pressed, if there is only whitespace
before the current point, then outdent the line one level.
Otherwise, do normal delete by repeating
`backward-delete-char-untabify' ARG times."
  (interactive "*p")
  (if (use-region-p)
      (backward-delete-char-untabify arg)
    (let ((cur-pos (current-column))
          (start-of-indention (save-excursion
                                (back-to-indentation)
                                (current-column)))
          (positions (ein:markdown-calc-indents)))
      (if (and (> cur-pos 0) (= cur-pos start-of-indention))
          (indent-line-to (ein:markdown-outdent-find-next-position cur-pos positions))
        (backward-delete-char-untabify arg)))))

(defun ein:markdown-find-leftmost-column (beg end)
  "Find the leftmost column in the region from BEG to END."
  (let ((mincol 1000))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (back-to-indentation)
        (unless (looking-at-p "[ \t]*$")
          (setq mincol (min mincol (current-column))))
        (forward-line 1)
        ))
    mincol))

(defun ein:markdown-indent-region (beg end arg)
  "Indent the region from BEG to END using some heuristics.
When ARG is non-nil, outdent the region instead.
See `markdown-indent-line' and `markdown-indent-line'."
  (interactive "*r\nP")
  (let* ((positions (sort (delete-dups (ein:markdown-calc-indents)) '<))
         (leftmostcol (ein:markdown-find-leftmost-column beg end))
         (next-pos (if arg
                       (ein:markdown-outdent-find-next-position leftmostcol positions)
                     (ein:markdown-indent-find-next-position leftmostcol positions))))
    (indent-rigidly beg end (- next-pos leftmostcol))
    (setq deactivate-mark nil)))

(define-obsolete-function-alias 'ein:markdown-exdent-region
  'ein:markdown-outdent-region "v2.3")

(defun ein:markdown-outdent-region (beg end)
  "Call `markdown-indent-region' on region from BEG to END with prefix."
  (interactive "*r")
  (ein:markdown-indent-region beg end t))


;;; Markup Completion =========================================================

(defconst ein:markdown-complete-alist
  '((ein:markdown-regex-header-atx . ein:markdown-complete-atx)
    (ein:markdown-regex-header-setext . ein:markdown-complete-setext)
    (ein:markdown-regex-hr . ein:markdown-complete-hr))
  "Association list of form (regexp . function) for markup completion.")

(defun ein:markdown-incomplete-atx-p ()
  "Return t if ATX header markup is incomplete and nil otherwise.
Assumes match data is available for `markdown-regex-header-atx'.
Checks that the number of trailing hash marks equals the number of leading
hash marks, that there is only a single space before and after the text,
and that there is no extraneous whitespace in the text."
  (or
   ;; Number of starting and ending hash marks differs
   (not (= (length (match-string 1)) (length (match-string 3))))
   ;; When the header text is not empty...
   (and (> (length (match-string 2)) 0)
        ;; ...if there are extra leading, trailing, or interior spaces
        (or (not (= (match-beginning 2) (1+ (match-end 1))))
            (not (= (match-beginning 3) (1+ (match-end 2))))
            (string-match-p "[ \t\n]\\{2\\}" (match-string 2))))
   ;; When the header text is empty...
   (and (= (length (match-string 2)) 0)
        ;; ...if there are too many or too few spaces
        (not (= (match-beginning 3) (+ (match-end 1) 2))))))

(defun ein:markdown-complete-atx ()
  "Complete and normalize ATX headers.
Add or remove hash marks to the end of the header to match the
beginning.  Ensure that there is only a single space between hash
marks and header text.  Removes extraneous whitespace from header text.
Assumes match data is available for `markdown-regex-header-atx'.
Return nil if markup was complete and non-nil if markup was completed."
  (when (ein:markdown-incomplete-atx-p)
    (let* ((new-marker (make-marker))
           (new-marker (set-marker new-marker (match-end 2))))
      ;; Hash marks and spacing at end
      (goto-char (match-end 2))
      (delete-region (match-end 2) (match-end 3))
      (insert " " (match-string 1))
      ;; Remove extraneous whitespace from title
      (replace-match (ein:markdown-compress-whitespace-string (match-string 2))
                     t t nil 2)
      ;; Spacing at beginning
      (goto-char (match-end 1))
      (delete-region (match-end 1) (match-beginning 2))
      (insert " ")
      ;; Leave point at end of text
      (goto-char new-marker))))

(defun ein:markdown-incomplete-setext-p ()
  "Return t if setext header markup is incomplete and nil otherwise.
Assumes match data is available for `markdown-regex-header-setext'.
Checks that length of underline matches text and that there is no
extraneous whitespace in the text."
  (or (not (= (length (match-string 1)) (length (match-string 2))))
      (string-match-p "[ \t\n]\\{2\\}" (match-string 1))))

(defun ein:markdown-complete-setext ()
  "Complete and normalize setext headers.
Add or remove underline characters to match length of header
text.  Removes extraneous whitespace from header text.  Assumes
match data is available for `markdown-regex-header-setext'.
Return nil if markup was complete and non-nil if markup was completed."
  (when (ein:markdown-incomplete-setext-p)
    (let* ((text (ein:markdown-compress-whitespace-string (match-string 1)))
           (char (char-after (match-beginning 2)))
           (level (if (char-equal char ?-) 2 1)))
      (goto-char (match-beginning 0))
      (delete-region (match-beginning 0) (match-end 0))
      (ein:markdown-insert-header level text t)
      t)))

(defun ein:markdown-incomplete-hr-p ()
  "Return non-nil if hr is not in `markdown-hr-strings' and nil otherwise.
Assumes match data is available for `markdown-regex-hr'."
  (not (member (match-string 0) ein:markdown-hr-strings)))

(defun ein:markdown-complete-hr ()
  "Complete horizontal rules.
If horizontal rule string is a member of `markdown-hr-strings',
do nothing.  Otherwise, replace with the car of
`markdown-hr-strings'.
Assumes match data is available for `markdown-regex-hr'.
Return nil if markup was complete and non-nil if markup was completed."
  (when (ein:markdown-incomplete-hr-p)
    (replace-match (car ein:markdown-hr-strings))
    t))

(defun ein:markdown-complete ()
  "Complete markup of object near point or in region when active.
Handle all objects in `markdown-complete-alist', in order.
See `markdown-complete-at-point' and `markdown-complete-region'."
  (interactive "*")
  (if (ein:markdown-use-region-p)
      (ein:markdown-complete-region (region-beginning) (region-end))
    (ein:markdown-complete-at-point)))

(defun ein:markdown-complete-at-point ()
  "Complete markup of object near point.
Handle all elements of `markdown-complete-alist' in order."
  (interactive "*")
  (let ((list ein:markdown-complete-alist) found changed)
    (while list
      (let ((regexp (eval (caar list)))
            (function (cdar list)))
        (setq list (cdr list))
        (when (thing-at-point-looking-at regexp)
          (setq found t)
          (setq changed (funcall function))
          (setq list nil))))
    (if found
        (or changed (user-error "Markup at point is complete"))
      (user-error "Nothing to complete at point"))))

(defun ein:markdown-complete-region (beg end)
  "Complete markup of objects in region from BEG to END.
Handle all objects in `markdown-complete-alist', in order.  Each
match is checked to ensure that a previous regexp does not also
match."
  (interactive "*r")
  (let ((end-marker (set-marker (make-marker) end))
        previous)
    (dolist (element ein:markdown-complete-alist)
      (let ((regexp (eval (car element)))
            (function (cdr element)))
        (goto-char beg)
        (while (re-search-forward regexp end-marker 'limit)
          (when (match-string 0)
            ;; Make sure this is not a match for any of the preceding regexps.
            ;; This prevents mistaking an HR for a Setext subheading.
            (let (match)
              (save-match-data
                (dolist (prev-regexp previous)
                  (or match (setq match (looking-back prev-regexp nil)))))
              (unless match
                (save-excursion (funcall function))))))
        (cl-pushnew regexp previous :test #'equal)))
    previous))

(defun ein:markdown-complete-buffer ()
  "Complete markup for all objects in the current buffer."
  (interactive "*")
  (ein:markdown-complete-region (point-min) (point-max)))


;;; Markup Cycling ============================================================

(defun ein:markdown-cycle-atx (arg &optional remove)
  "Cycle ATX header markup.
Promote header (decrease level) when ARG is 1 and demote
header (increase level) if arg is -1.  When REMOVE is non-nil,
remove the header when the level reaches zero and stop cycling
when it reaches six.  Otherwise, perform a proper cycling through
levels one through six.  Assumes match data is available for
`markdown-regex-header-atx'."
  (let* ((old-level (length (match-string 1)))
         (new-level (+ old-level arg))
         (text (match-string 2)))
    (when (not remove)
      (setq new-level (% new-level 6))
      (setq new-level (cond ((= new-level 0) 6)
                            ((< new-level 0) (+ new-level 6))
                            (t new-level))))
    (cond
     ((= new-level 0)
      (ein:markdown-unwrap-thing-at-point nil 0 2))
     ((<= new-level 6)
      (goto-char (match-beginning 0))
      (delete-region (match-beginning 0) (match-end 0))
      (ein:markdown-insert-header new-level text nil)))))

(defun ein:markdown-cycle-setext (arg &optional remove)
  "Cycle setext header markup.
Promote header (increase level) when ARG is 1 and demote
header (decrease level or remove) if arg is -1.  When demoting a
level-two setext header, replace with a level-three atx header.
When REMOVE is non-nil, remove the header when the level reaches
zero.  Otherwise, cycle back to a level six atx header.  Assumes
match data is available for `markdown-regex-header-setext'."
  (let* ((char (char-after (match-beginning 2)))
         (old-level (if (char-equal char ?=) 1 2))
         (new-level (+ old-level arg)))
    (when (and (not remove) (= new-level 0))
      (setq new-level 6))
    (cond
     ((= new-level 0)
      (ein:markdown-unwrap-thing-at-point nil 0 1))
     ((<= new-level 2)
      (ein:markdown-insert-header new-level nil t))
     ((<= new-level 6)
      (ein:markdown-insert-header new-level nil nil)))))

(defun ein:markdown-cycle-hr (arg &optional remove)
  "Cycle string used for horizontal rule from `markdown-hr-strings'.
When ARG is 1, cycle forward (demote), and when ARG is -1, cycle
backwards (promote).  When REMOVE is non-nil, remove the hr instead
of cycling when the end of the list is reached.
Assumes match data is available for `markdown-regex-hr'."
  (let* ((strings (if (= arg -1)
                      (reverse ein:markdown-hr-strings)
                    ein:markdown-hr-strings))
         (tail (member (match-string 0) strings))
         (new (or (cadr tail)
                  (if remove
                      (if (= arg 1)
                          ""
                        (car tail))
                    (car strings)))))
    (replace-match new)))

(defun ein:markdown-cycle-bold ()
  "Cycle bold markup between underscores and asterisks.
Assumes match data is available for `markdown-regex-bold'."
  (save-excursion
    (let* ((old-delim (match-string 3))
           (new-delim (if (string-equal old-delim "**") "__" "**")))
      (replace-match new-delim t t nil 3)
      (replace-match new-delim t t nil 5))))

(defun ein:markdown-cycle-italic ()
  "Cycle italic markup between underscores and asterisks.
Assumes match data is available for `markdown-regex-italic'."
  (save-excursion
    (let* ((old-delim (match-string 2))
           (new-delim (if (string-equal old-delim "*") "_" "*")))
      (replace-match new-delim t t nil 2)
      (replace-match new-delim t t nil 4))))


;;; Keymap ====================================================================

(defun ein:markdown--style-map-prompt ()
  "Return a formatted prompt for ein:markdown markup insertion."
  (when ein:markdown-enable-prefix-prompts
    (concat
     "ein:markdown: "
     (propertize "bold" 'face 'ein:markdown-bold-face) ", "
     (propertize "italic" 'face 'ein:markdown-italic-face) ", "
     (propertize "code" 'face 'ein:markdown-inline-code-face) ", "
     (propertize "C = GFM code" 'face 'ein:markdown-code-face) ", "
     (propertize "pre" 'face 'ein:markdown-pre-face) ", "
     (propertize "footnote" 'face 'ein:markdown-footnote-text-face) ", "
     (propertize "q = blockquote" 'face 'ein:markdown-blockquote-face) ", "
     (propertize "h & 1-6 = heading" 'face 'ein:markdown-header-face) ", "
     (propertize "- = hr" 'face 'ein:markdown-hr-face) ", "
     "C-h = more")))

(defun ein:markdown--command-map-prompt ()
  "Return prompt for ein:markdown buffer-wide commands."
  (when ein:markdown-enable-prefix-prompts
    (concat
     "Command: "
     (propertize "m" 'face 'ein:markdown-bold-face) "arkdown, "
     (propertize "o" 'face 'ein:markdown-bold-face) "pen, "
     (propertize "c" 'face 'ein:markdown-bold-face) "heck refs, "
     (propertize "u" 'face 'ein:markdown-bold-face) "nused refs, "
     "C-h = more")))

(defvar ein:markdown-mode-style-map
  (let ((map (make-keymap (ein:markdown--style-map-prompt))))
    (define-key map (kbd "1") 'ein:markdown-insert-header-atx-1)
    (define-key map (kbd "2") 'ein:markdown-insert-header-atx-2)
    (define-key map (kbd "3") 'ein:markdown-insert-header-atx-3)
    (define-key map (kbd "4") 'ein:markdown-insert-header-atx-4)
    (define-key map (kbd "5") 'ein:markdown-insert-header-atx-5)
    (define-key map (kbd "6") 'ein:markdown-insert-header-atx-6)
    (define-key map (kbd "!") 'ein:markdown-insert-header-setext-1)
    (define-key map (kbd "@") 'ein:markdown-insert-header-setext-2)
    (define-key map (kbd "b") 'ein:markdown-insert-bold)
    (define-key map (kbd "c") 'ein:markdown-insert-code)
    (define-key map (kbd "f") 'ein:markdown-insert-footnote)
    (define-key map (kbd "h") 'ein:markdown-insert-header-dwim)
    (define-key map (kbd "H") 'ein:markdown-insert-header-setext-dwim)
    (define-key map (kbd "i") 'ein:markdown-insert-italic)
    (define-key map (kbd "k") 'ein:markdown-insert-kbd)
    (define-key map (kbd "l") 'ein:markdown-insert-link)
    (define-key map (kbd "p") 'ein:markdown-insert-pre)
    (define-key map (kbd "P") 'ein:markdown-pre-region)
    (define-key map (kbd "q") 'ein:markdown-insert-blockquote)
    (define-key map (kbd "s") 'ein:markdown-insert-strike-through)
    (define-key map (kbd "t") 'ein:markdown-insert-table)
    (define-key map (kbd "Q") 'ein:markdown-blockquote-region)
    (define-key map (kbd "-") 'ein:markdown-insert-hr)
    ;; Deprecated keys that may be removed in a future version
    (define-key map (kbd "e") 'ein:markdown-insert-italic)
    map)
  "Keymap for ein:markdown text styling commands.")

(defvar ein:markdown-mode-command-map
  (let ((map (make-keymap (ein:markdown--command-map-prompt))))
    (define-key map (kbd "m") 'ein:markdown-other-window)
    (define-key map (kbd "o") 'ein:markdown-open)
    (define-key map (kbd "w") 'ein:markdown-kill-ring-save)
    (define-key map (kbd "c") 'ein:markdown-check-refs)
    (define-key map (kbd "u") 'ein:markdown-unused-refs)
    (define-key map (kbd "n") 'ein:markdown-cleanup-list-numbers)
    (define-key map (kbd "]") 'ein:markdown-complete-buffer)
    (define-key map (kbd "^") 'ein:markdown-table-sort-lines)
    (define-key map (kbd "|") 'ein:markdown-table-convert-region)
    (define-key map (kbd "t") 'ein:markdown-table-transpose)
    map)
  "Keymap for ein:markdown buffer-wide commands.")

(defvar ein:markdown-mode-map
  (let ((map (make-keymap)))
    ;; Markup insertion & removal
    (define-key map (kbd "C-c C-s") ein:markdown-mode-style-map)
    (define-key map (kbd "C-c C-l") 'ein:markdown-insert-link)
    (define-key map (kbd "C-c C-k") 'ein:markdown-kill-thing-at-point)
    ;; Promotion, demotion, and cycling
    (define-key map (kbd "C-c C--") 'ein:markdown-promote)
    (define-key map (kbd "C-c C-=") 'ein:markdown-demote)
    (define-key map (kbd "C-c C-]") 'ein:markdown-complete)
    ;; Following and doing things
    (define-key map (kbd "C-c C-o") 'ein:markdown-follow-thing-at-point)
    (define-key map (kbd "C-c C-d") 'ein:markdown-do)
    (define-key map (kbd "C-c '") 'ein:markdown-edit-code-block)
    ;; Indentation
    (define-key map (kbd "C-m") 'ein:markdown-enter-key)
    (define-key map (kbd "DEL") 'ein:markdown-outdent-or-delete)
    (define-key map (kbd "C-c >") 'ein:markdown-indent-region)
    (define-key map (kbd "C-c <") 'ein:markdown-outdent-region)
    ;; Visibility cycling
    (define-key map (kbd "TAB") 'ein:markdown-cycle)
    (define-key map (kbd "<S-iso-lefttab>") 'ein:markdown-shifttab)
    (define-key map (kbd "<S-tab>")  'ein:markdown-shifttab)
    (define-key map (kbd "<backtab>") 'ein:markdown-shifttab)
    ;; Heading and list navigation
    (define-key map (kbd "C-c C-n") 'ein:markdown-outline-next)
    (define-key map (kbd "C-c C-p") 'ein:markdown-outline-previous)
    (define-key map (kbd "C-c C-f") 'ein:markdown-outline-next-same-level)
    (define-key map (kbd "C-c C-b") 'ein:markdown-outline-previous-same-level)
    (define-key map (kbd "C-c C-u") 'ein:markdown-outline-up)
    ;; Buffer-wide commands
    (define-key map (kbd "C-c C-c") ein:markdown-mode-command-map)
    ;; Subtree, list, and table editing
    (define-key map (kbd "C-c <up>") 'ein:markdown-move-up)
    (define-key map (kbd "C-c <down>") 'ein:markdown-move-down)
    (define-key map (kbd "C-c <left>") 'ein:markdown-promote)
    (define-key map (kbd "C-c <right>") 'ein:markdown-demote)
    (define-key map (kbd "C-c S-<up>") 'ein:markdown-table-delete-row)
    (define-key map (kbd "C-c S-<down>") 'ein:markdown-table-insert-row)
    (define-key map (kbd "C-c S-<left>") 'ein:markdown-table-delete-column)
    (define-key map (kbd "C-c S-<right>") 'ein:markdown-table-insert-column)
    (define-key map (kbd "C-c C-M-h") 'ein:markdown-mark-subtree)
    (define-key map (kbd "C-x n s") 'ein:markdown-narrow-to-subtree)
    (define-key map (kbd "M-RET") 'ein:markdown-insert-list-item)
    (define-key map (kbd "C-c C-j") 'ein:markdown-insert-list-item)
    ;; Paragraphs (ein:markdown context aware)
    (define-key map [remap backward-paragraph] 'ein:markdown-backward-paragraph)
    (define-key map [remap forward-paragraph] 'ein:markdown-forward-paragraph)
    (define-key map [remap mark-paragraph] 'ein:markdown-mark-paragraph)
    ;; Blocks (one or more paragraphs)
    (define-key map (kbd "C-M-{") 'ein:markdown-backward-block)
    (define-key map (kbd "C-M-}") 'ein:markdown-forward-block)
    (define-key map (kbd "C-c M-h") 'ein:markdown-mark-block)
    (define-key map (kbd "C-x n b") 'ein:markdown-narrow-to-block)
    ;; Pages (top-level sections)
    (define-key map [remap backward-page] 'ein:markdown-backward-page)
    (define-key map [remap forward-page] 'ein:markdown-forward-page)
    (define-key map [remap mark-page] 'ein:markdown-mark-page)
    (define-key map [remap narrow-to-page] 'ein:markdown-narrow-to-page)
    ;; Link Movement
    (define-key map (kbd "M-n") 'ein:markdown-next-link)
    (define-key map (kbd "M-p") 'ein:markdown-previous-link)
    ;; Toggling functionality
    (define-key map (kbd "C-c C-x C-e") 'ein:markdown-toggle-math)
    ;; Alternative keys (in case of problems with the arrow keys)
    (define-key map (kbd "C-c C-x u") 'ein:markdown-move-up)
    (define-key map (kbd "C-c C-x d") 'ein:markdown-move-down)
    (define-key map (kbd "C-c C-x l") 'ein:markdown-promote)
    (define-key map (kbd "C-c C-x r") 'ein:markdown-demote)
    ;; Deprecated keys that may be removed in a future version
    (define-key map (kbd "C-c C-a L") 'ein:markdown-insert-link) ;; C-c C-l
    (define-key map (kbd "C-c C-a l") 'ein:markdown-insert-link) ;; C-c C-l
    (define-key map (kbd "C-c C-a r") 'ein:markdown-insert-link) ;; C-c C-l
    (define-key map (kbd "C-c C-a u") 'ein:markdown-insert-uri) ;; C-c C-l
    (define-key map (kbd "C-c C-a f") 'ein:markdown-insert-footnote)
    (define-key map (kbd "C-c C-t 1") 'ein:markdown-insert-header-atx-1)
    (define-key map (kbd "C-c C-t 2") 'ein:markdown-insert-header-atx-2)
    (define-key map (kbd "C-c C-t 3") 'ein:markdown-insert-header-atx-3)
    (define-key map (kbd "C-c C-t 4") 'ein:markdown-insert-header-atx-4)
    (define-key map (kbd "C-c C-t 5") 'ein:markdown-insert-header-atx-5)
    (define-key map (kbd "C-c C-t 6") 'ein:markdown-insert-header-atx-6)
    (define-key map (kbd "C-c C-t !") 'ein:markdown-insert-header-setext-1)
    (define-key map (kbd "C-c C-t @") 'ein:markdown-insert-header-setext-2)
    (define-key map (kbd "C-c C-t h") 'ein:markdown-insert-header-dwim)
    (define-key map (kbd "C-c C-t H") 'ein:markdown-insert-header-setext-dwim)
    (define-key map (kbd "C-c C-t s") 'ein:markdown-insert-header-setext-2)
    (define-key map (kbd "C-c C-t t") 'ein:markdown-insert-header-setext-1)
    (define-key map (kbd "C-c C-i") 'ein:markdown-insert-image)
    (define-key map (kbd "C-c C-x m") 'ein:markdown-insert-list-item) ;; C-c C-j
    (define-key map (kbd "C-c -") 'ein:markdown-insert-hr)
    map)
  "Keymap for ein:markdown major mode.")

(defvar ein:markdown-mode-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'ein:markdown-follow-link-at-point)
    map)
  "Keymap for following links with mouse.")

;;; Menu ======================================================================

(easy-menu-define ein:markdown-mode-menu ein:markdown-mode-map
  "Menu for ein:markdown mode"
  '("ein:markdown"
    "---"
    ("Movement"
     ["Jump" ein:markdown-do]
     ["Follow Link" ein:markdown-follow-thing-at-point]
     ["Next Link" ein:markdown-next-link]
     ["Previous Link" ein:markdown-previous-link]
     "---"
     ["Next Heading or List Item" ein:markdown-outline-next]
     ["Previous Heading or List Item" ein:markdown-outline-previous]
     ["Next at Same Level" ein:markdown-outline-next-same-level]
     ["Previous at Same Level" ein:markdown-outline-previous-same-level]
     ["Up to Parent" ein:markdown-outline-up]
     "---"
     ["Forward Paragraph" ein:markdown-forward-paragraph]
     ["Backward Paragraph" ein:markdown-backward-paragraph]
     ["Forward Block" ein:markdown-forward-block]
     ["Backward Block" ein:markdown-backward-block])
    ("Show & Hide"
     ["Cycle Heading Visibility" ein:markdown-cycle
      :enable (ein:markdown-on-heading-p)]
     ["Cycle Heading Visibility (Global)" ein:markdown-shifttab]
     "---"
     ["Narrow to Region" narrow-to-region]
     ["Narrow to Block" ein:markdown-narrow-to-block]
     ["Narrow to Section" narrow-to-defun]
     ["Narrow to Subtree" ein:markdown-narrow-to-subtree]
     ["Widen" widen (buffer-narrowed-p)])
    "---"
    ("Headings & Structure"
     ["Automatic Heading" ein:markdown-insert-header-dwim
      :keys "C-c C-s h"]
     ["Automatic Heading (Setext)" ein:markdown-insert-header-setext-dwim
      :keys "C-c C-s H"]
     ("Specific Heading (atx)"
      ["First Level atx" ein:markdown-insert-header-atx-1
       :keys "C-c C-s 1"]
      ["Second Level atx" ein:markdown-insert-header-atx-2
       :keys "C-c C-s 2"]
      ["Third Level atx" ein:markdown-insert-header-atx-3
       :keys "C-c C-s 3"]
      ["Fourth Level atx" ein:markdown-insert-header-atx-4
       :keys "C-c C-s 4"]
      ["Fifth Level atx" ein:markdown-insert-header-atx-5
       :keys "C-c C-s 5"]
      ["Sixth Level atx" ein:markdown-insert-header-atx-6
       :keys "C-c C-s 6"])
     ("Specific Heading (Setext)"
      ["First Level Setext" ein:markdown-insert-header-setext-1
       :keys "C-c C-s !"]
      ["Second Level Setext" ein:markdown-insert-header-setext-2
       :keys "C-c C-s @"])
     ["Horizontal Rule" ein:markdown-insert-hr
      :keys "C-c C-s -"]
     "---"
     ["Move Subtree Up" ein:markdown-move-up
      :keys "C-c <up>"]
     ["Move Subtree Down" ein:markdown-move-down
      :keys "C-c <down>"]
     ["Promote Subtree" ein:markdown-promote
      :keys "C-c <left>"]
     ["Demote Subtree" ein:markdown-demote
      :keys "C-c <right>"])
    ("Region & Mark"
     ["Indent Region" ein:markdown-indent-region]
     ["Outdent Region" ein:markdown-outdent-region]
     "--"
     ["Mark Paragraph" mark-paragraph]
     ["Mark Block" ein:markdown-mark-block]
     ["Mark Section" mark-defun]
     ["Mark Subtree" ein:markdown-mark-subtree])
    ("Tables"
     ["Move Row Up" ein:markdown-move-up
      :enable (ein:markdown-table-at-point-p)
      :keys "C-c <up>"]
     ["Move Row Down" ein:markdown-move-down
      :enable (ein:markdown-table-at-point-p)
      :keys "C-c <down>"]
     ["Move Column Left" ein:markdown-demote
      :enable (ein:markdown-table-at-point-p)
      :keys "C-c <left>"]
     ["Move Column Right" ein:markdown-promote
      :enable (ein:markdown-table-at-point-p)
      :keys "C-c <right>"]
     ["Delete Row" ein:markdown-table-delete-row
      :enable (ein:markdown-table-at-point-p)]
     ["Insert Row" ein:markdown-table-insert-row
      :enable (ein:markdown-table-at-point-p)]
     ["Delete Column" ein:markdown-table-delete-column
      :enable (ein:markdown-table-at-point-p)]
     ["Insert Column" ein:markdown-table-insert-column
      :enable (ein:markdown-table-at-point-p)]
     ["Insert Table" ein:markdown-insert-table]
     "--"
     ["Convert Region to Table" ein:markdown-table-convert-region]
     ["Sort Table Lines" ein:markdown-table-sort-lines
      :enable (ein:markdown-table-at-point-p)]
     ["Transpose Table" ein:markdown-table-transpose
      :enable (ein:markdown-table-at-point-p)])
    ("Lists"
     ["Insert List Item" ein:markdown-insert-list-item]
     ["Move Subtree Up" ein:markdown-move-up
      :keys "C-c <up>"]
     ["Move Subtree Down" ein:markdown-move-down
      :keys "C-c <down>"]
     ["Indent Subtree" ein:markdown-demote
      :keys "C-c <right>"]
     ["Outdent Subtree" ein:markdown-promote
      :keys "C-c <left>"]
     ["Renumber List" ein:markdown-cleanup-list-numbers])
    ("Links & Images"
     ["Insert Link" ein:markdown-insert-link]
     ["Insert Image" ein:markdown-insert-image]
     ["Insert Footnote" ein:markdown-insert-footnote
      :keys "C-c C-s f"]
     "---"
     ["Check References" ein:markdown-check-refs]
     ["Find Unused References" ein:markdown-unused-refs])
    ("Styles"
     ["Bold" ein:markdown-insert-bold]
     ["Italic" ein:markdown-insert-italic]
     ["Code" ein:markdown-insert-code]
     ["Strikethrough" ein:markdown-insert-strike-through]
     ["Keyboard" ein:markdown-insert-kbd]
     "---"
     ["Blockquote" ein:markdown-insert-blockquote]
     ["Preformatted" ein:markdown-insert-pre]
     ["Edit Code Block" ein:markdown-edit-code-block
      :enable (ein:markdown-code-block-at-point-p)]
     "---"
     ["Blockquote Region" ein:markdown-blockquote-region]
     ["Preformatted Region" ein:markdown-pre-region]
     "---"
     ["LaTeX Math Support" ein:markdown-toggle-math
      :style radio
      :selected ein:markdown-enable-math])
    "---"
    ("Markup Completion and Cycling"
     ["Complete Markup" ein:markdown-complete]
     ["Promote Element" ein:markdown-promote
      :keys "C-c C--"]
     ["Demote Element" ein:markdown-demote
      :keys "C-c C-="])
    "---"
    ["Kill Element" ein:markdown-kill-thing-at-point]
    "---"
    ("Documentation"
     ["Version" ein:markdown-show-version]
     ["Homepage" ein:markdown-mode-info]
     ["Describe Mode" (describe-function 'ein:markdown-mode)]
     ["Guide" (browse-url "https://leanpub.com/ein:markdown-mode")])))


;;; imenu =====================================================================

(defun ein:markdown-imenu-create-nested-index ()
  "Create and return a nested imenu index alist for the current buffer.
See `imenu-create-index-function' and `imenu--index-alist' for details."
  (let* ((root '(nil . nil))
         cur-alist
         (cur-level 0)
         (empty-heading "-")
         (self-heading ".")
         hashes pos level heading)
    (save-excursion
      ;; Headings
      (goto-char (point-min))
      (while (re-search-forward ein:markdown-regex-header (point-max) t)
        (unless (ein:markdown-code-block-at-point-p)
          (cond
           ((match-string-no-properties 2) ;; level 1 setext
            (setq heading (match-string-no-properties 1))
            (setq pos (match-beginning 1)
                  level 1))
           ((match-string-no-properties 3) ;; level 2 setext
            (setq heading (match-string-no-properties 1))
            (setq pos (match-beginning 1)
                  level 2))
           ((setq hashes (ein:markdown-trim-whitespace
                          (match-string-no-properties 4)))
            (setq heading (match-string-no-properties 5)
                  pos (match-beginning 4)
                  level (length hashes))))
          (let ((alist (list (cons heading pos))))
            (cond
             ((= cur-level level)       ; new sibling
              (setcdr cur-alist alist)
              (setq cur-alist alist))
             ((< cur-level level)       ; first child
              (dotimes (_ (- level cur-level 1))
                (setq alist (list (cons empty-heading alist))))
              (if cur-alist
                  (let* ((parent (car cur-alist))
                         (self-pos (cdr parent)))
                    (setcdr parent (cons (cons self-heading self-pos) alist)))
                (setcdr root alist))    ; primogenitor
              (setq cur-alist alist)
              (setq cur-level level))
             (t                         ; new sibling of an ancestor
              (let ((sibling-alist (last (cdr root))))
                (dotimes (_ (1- level))
                  (setq sibling-alist (last (cdar sibling-alist))))
                (setcdr sibling-alist alist)
                (setq cur-alist alist))
              (setq cur-level level))))))
      ;; Footnotes
      (let ((fn (ein:markdown-get-defined-footnotes)))
        (if (or (zerop (length fn))
                (null ein:markdown-add-footnotes-to-imenu))
            (cdr root)
          (nconc (cdr root) (list (cons "Footnotes" fn))))))))

(defun ein:markdown-imenu-create-flat-index ()
  "Create and return a flat imenu index alist for the current buffer.
See `imenu-create-index-function' and `imenu--index-alist' for details."
  (let* ((empty-heading "-") index heading pos)
    (save-excursion
      ;; Headings
      (goto-char (point-min))
      (while (re-search-forward ein:markdown-regex-header (point-max) t)
        (when (and (not (ein:markdown-code-block-at-point-p (point-at-bol)))
                   (not (ein:markdown-text-property-at-point 'ein:markdown-yaml-metadata-begin)))
          (cond
           ((setq heading (match-string-no-properties 1))
            (setq pos (match-beginning 1)))
           ((setq heading (match-string-no-properties 5))
            (setq pos (match-beginning 4))))
          (or (> (length heading) 0)
              (setq heading empty-heading))
          (setq index (append index (list (cons heading pos))))))
      ;; Footnotes
      (when ein:markdown-add-footnotes-to-imenu
        (nconc index (ein:markdown-get-defined-footnotes)))
      index)))


;;; References ================================================================

(defun ein:markdown-reference-goto-definition ()
  "Jump to the definition of the reference at point or create it."
  (interactive)
  (when (thing-at-point-looking-at ein:markdown-regex-link-reference)
    (let* ((text (match-string-no-properties 3))
           (reference (match-string-no-properties 6))
           (target (downcase (if (string= reference "") text reference)))
           (loc (cadr (save-match-data (ein:markdown-reference-definition target)))))
      (if loc
          (goto-char loc)
        (goto-char (match-beginning 0))
        (ein:markdown-insert-reference-definition target)))))

(defun ein:markdown-reference-find-links (reference)
  "Return a list of all links for REFERENCE.
REFERENCE should not include the surrounding square brackets.
Elements of the list have the form (text start line), where
text is the link text, start is the location at the beginning of
the link, and line is the line number on which the link appears."
  (let* ((ref-quote (regexp-quote reference))
         (regexp (format "!?\\(?:\\[\\(%s\\)\\][ ]?\\[\\]\\|\\[\\([^]]+?\\)\\][ ]?\\[%s\\]\\)"
                         ref-quote ref-quote))
         links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let* ((text (or (match-string-no-properties 1)
                         (match-string-no-properties 2)))
               (start (match-beginning 0))
               (line (ein:markdown-line-number-at-pos)))
          (cl-pushnew (list text start line) links :test #'equal))))
    links))

(defmacro ein:markdown-for-all-refs (f)
  `(let ((result))
     (save-excursion
       (goto-char (point-min))
       (while
           (re-search-forward ein:markdown-regex-link-reference nil t)
         (let* ((text (match-string-no-properties 3))
                (reference (match-string-no-properties 6))
                (target (downcase (if (string= reference "") text reference))))
          (,f text target result))))
     (reverse result)))

(defmacro ein:markdown-collect-always (_ target result)
  `(cl-pushnew ,target ,result :test #'equal))

(defmacro ein:markdown-collect-undefined (text target result)
  `(unless (ein:markdown-reference-definition target)
     (let ((entry (assoc ,target ,result)))
       (if (not entry)
           (cl-pushnew
            (cons ,target (list (cons ,text (ein:markdown-line-number-at-pos))))
            ,result :test #'equal)
         (setcdr entry
                 (append (cdr entry) (list (cons ,text (ein:markdown-line-number-at-pos)))))))))

(defun ein:markdown-get-all-refs ()
  "Return a list of all ein:markdown references."
  (ein:markdown-for-all-refs ein:markdown-collect-always))

(defun ein:markdown-get-undefined-refs ()
  "Return a list of undefined ein:markdown references.
Result is an alist of pairs (reference . occurrences), where
occurrences is itself another alist of pairs (label . line-number).
For example, an alist corresponding to [Nice editor][Emacs] at line 12,
\[GNU Emacs][Emacs] at line 45 and [manual][elisp] at line 127 is
\((\"emacs\" (\"Nice editor\" . 12) (\"GNU Emacs\" . 45)) (\"elisp\" (\"manual\" . 127)))."
  (ein:markdown-for-all-refs ein:markdown-collect-undefined))

(defun ein:markdown-get-unused-refs ()
  (cl-sort
   (cl-set-difference
    (ein:markdown-get-defined-references) (ein:markdown-get-all-refs)
    :test (lambda (e1 e2) (equal (car e1) e2)))
   #'< :key #'cdr))

(defmacro defun-markdown-buffer (name docstring)
  "Define a function to name and return a buffer.

By convention, NAME must be a name of a string constant with
%buffer% placeholder used to name the buffer, and will also be
used as a name of the function defined.

DOCSTRING will be used as the first part of the docstring."
  `(defun ,name (&optional buffer-name)
     ,(concat docstring "\n\nBUFFER-NAME is the name of the main buffer being visited.")
     (or buffer-name (setq buffer-name (buffer-name)))
     (let ((refbuf (get-buffer-create (ein:markdown-replace-regexp-in-string
                                       "%buffer%" buffer-name
                                       ,name))))
       (with-current-buffer refbuf
         (when view-mode
           (View-exit-and-edit))
         (use-local-map button-buffer-map)
         (erase-buffer))
       refbuf)))

(defconst ein:markdown-reference-check-buffer
  "*Undefined references for %buffer%*"
  "Pattern for name of buffer for listing undefined references.
The string %buffer% will be replaced by the corresponding
`ein:markdown-mode' buffer name.")

(defun-markdown-buffer
  ein:markdown-reference-check-buffer
  "Name and return buffer for reference checking.")

(defconst ein:markdown-unused-references-buffer
  "*Unused references for %buffer%*"
  "Pattern for name of buffer for listing unused references.
The string %buffer% will be replaced by the corresponding
`ein:markdown-mode' buffer name.")

(defun-markdown-buffer
  ein:markdown-unused-references-buffer
  "Name and return buffer for unused reference checking.")

(defconst ein:markdown-reference-links-buffer
  "*Reference links for %buffer%*"
  "Pattern for name of buffer for listing references.
The string %buffer% will be replaced by the corresponding buffer name.")

(defun-markdown-buffer
  ein:markdown-reference-links-buffer
  "Name, setup, and return a buffer for listing links.")

;; Add an empty ein:markdown reference definition to buffer
;; specified in the 'target-buffer property.  The reference name is
;; the button's label.
(define-button-type 'ein:markdown-undefined-reference-button
  'help-echo "mouse-1, RET: create definition for undefined reference"
  'follow-link t
  'face 'bold
  'action (lambda (b)
            (let ((buffer (button-get b 'target-buffer))
                  (line (button-get b 'target-line))
                  (label (button-label b)))
              (switch-to-buffer-other-window buffer)
              (goto-char (point-min))
              (forward-line line)
              (ein:markdown-insert-reference-definition label)
              (ein:markdown-check-refs t))))

;; Jump to line in buffer specified by 'target-buffer property.
;; Line number is button's 'target-line property.
(define-button-type 'ein:markdown-goto-line-button
  'help-echo "mouse-1, RET: go to line"
  'follow-link t
  'face 'italic
  'action (lambda (b)
            (switch-to-buffer-other-window (button-get b 'target-buffer))
            ;; use call-interactively to silence compiler
            (let ((current-prefix-arg (button-get b 'target-line)))
              (call-interactively 'goto-line))))

;; Kill a line in buffer specified by 'target-buffer property.
;; Line number is button's 'target-line property.
(define-button-type 'ein:markdown-kill-line-button
  'help-echo "mouse-1, RET: kill line"
  'follow-link t
  'face 'italic
  'action (lambda (b)
            (switch-to-buffer-other-window (button-get b 'target-buffer))
            ;; use call-interactively to silence compiler
            (let ((current-prefix-arg (button-get b 'target-line)))
              (call-interactively 'goto-line))
            (kill-line 1)
            (ein:markdown-unused-refs t)))

;; Jumps to a particular link at location given by 'target-char
;; property in buffer given by 'target-buffer property.
(define-button-type 'ein:markdown-location-button
  'help-echo "mouse-1, RET: jump to location of link"
  'follow-link t
  'face 'bold
  'action (lambda (b)
            (let ((target (button-get b 'target-buffer))
                  (loc (button-get b 'target-char)))
              (kill-buffer-and-window)
              (switch-to-buffer target)
              (goto-char loc))))

(defun ein:markdown-insert-undefined-reference-button (reference oldbuf)
  "Insert a button for creating REFERENCE in buffer OLDBUF.
REFERENCE should be a list of the form (reference . occurrences),
as returned by `markdown-get-undefined-refs'."
  (let ((label (car reference)))
    ;; Create a reference button
    (insert-button label
                   :type 'ein:markdown-undefined-reference-button
                   'target-buffer oldbuf
                   'target-line (cdr (car (cdr reference))))
    (insert " (")
    (dolist (occurrence (cdr reference))
      (let ((line (cdr occurrence)))
        ;; Create a line number button
        (insert-button (number-to-string line)
                       :type 'ein:markdown-goto-line-button
                       'target-buffer oldbuf
                       'target-line line)
        (insert " ")))
    (delete-char -1)
    (insert ")")
    (newline)))

(defun ein:markdown-insert-unused-reference-button (reference oldbuf)
  "Insert a button for creating REFERENCE in buffer OLDBUF.
REFERENCE must be a pair of (ref . line-number)."
  (let ((label (car reference))
        (line (cdr reference)))
    ;; Create a reference button
    (insert-button label
                   :type 'ein:markdown-goto-line-button
                   'face 'bold
                   'target-buffer oldbuf
                   'target-line line)
    (insert (format " (%d) [" line))
    (insert-button "X"
                   :type 'ein:markdown-kill-line-button
                   'face 'bold
                   'target-buffer oldbuf
                   'target-line line)
    (insert "]")
    (newline)))

(defun ein:markdown-insert-link-button (link oldbuf)
  "Insert a button for jumping to LINK in buffer OLDBUF.
LINK should be a list of the form (text char line) containing
the link text, location, and line number."
  (let ((label (cl-first link))
        (char (cl-second link))
        (line (cl-third link)))
    ;; Create a reference button
    (insert-button label
                   :type 'ein:markdown-location-button
                   'target-buffer oldbuf
                   'target-char char)
    (insert (format " (line %d)\n" line))))

(defun ein:markdown-reference-goto-link (&optional reference)
  "Jump to the location of the first use of REFERENCE."
  (interactive)
  (unless reference
    (if (thing-at-point-looking-at ein:markdown-regex-reference-definition)
        (setq reference (match-string-no-properties 2))
      (user-error "No reference definition at point")))
  (let ((links (ein:markdown-reference-find-links reference)))
    (cond ((= (length links) 1)
           (goto-char (cadr (car links))))
          ((> (length links) 1)
           (let ((oldbuf (current-buffer))
                 (linkbuf (ein:markdown-reference-links-buffer)))
             (with-current-buffer linkbuf
               (insert "Links using reference " reference ":\n\n")
               (dolist (link (reverse links))
                 (ein:markdown-insert-link-button link oldbuf)))
             (view-buffer-other-window linkbuf)
             (goto-char (point-min))
             (forward-line 2)))
          (t
           (error "No links for reference %s" reference)))))

(defmacro defun-markdown-ref-checker
    (name docstring checker-function buffer-function none-message buffer-header insert-reference)
  "Define a function NAME acting on result of CHECKER-FUNCTION.

DOCSTRING is used as a docstring for the defined function.

BUFFER-FUNCTION should name and return an auxiliary buffer to put
results in.

NONE-MESSAGE is used when CHECKER-FUNCTION returns no results.

BUFFER-HEADER is put into the auxiliary buffer first, followed by
calling INSERT-REFERENCE for each element in the list returned by
CHECKER-FUNCTION."
  `(defun ,name (&optional silent)
     ,(concat
       docstring
       "\n\nIf SILENT is non-nil, do not message anything when no
such references found.")
     (interactive "P")
     (unless (memq major-mode '(ein:markdown-mode))
       (user-error "Not available in current mode"))
     (let ((oldbuf (current-buffer))
           (refs (,checker-function))
           (refbuf (,buffer-function)))
       (if (null refs)
           (progn
             (when (not silent)
               (message ,none-message))
             (kill-buffer refbuf))
         (with-current-buffer refbuf
           (insert ,buffer-header)
           (dolist (ref refs)
             (,insert-reference ref oldbuf))
           (view-buffer-other-window refbuf)
           (goto-char (point-min))
           (forward-line 2))))))

(defun-markdown-ref-checker
  ein:markdown-check-refs
  "Show all undefined ein:markdown references in current `ein:markdown-mode' buffer.

Links which have empty reference definitions are considered to be
defined."
  ein:markdown-get-undefined-refs
  ein:markdown-reference-check-buffer
  "No undefined references found"
  "The following references are undefined:\n\n"
  ein:markdown-insert-undefined-reference-button)


(defun-markdown-ref-checker
  ein:markdown-unused-refs
  "Show all unused ein:markdown references in current `ein:markdown-mode' buffer."
  ein:markdown-get-unused-refs
  ein:markdown-unused-references-buffer
  "No unused references found"
  "The following references are unused:\n\n"
  ein:markdown-insert-unused-reference-button)



;;; Lists =====================================================================

(defun ein:markdown-insert-list-item (&optional arg)
  "Insert a new list item.
If the point is inside unordered list, insert a bullet mark.  If
the point is inside ordered list, insert the next number followed
by a period.  Use the previous list item to determine the amount
of whitespace to place before and after list markers.

With a \\[universal-argument] prefix (i.e., when ARG is (4)),
decrease the indentation by one level.

With two \\[universal-argument] prefixes (i.e., when ARG is (16)),
increase the indentation by one level."
  (interactive "p")
  (let (bounds cur-indent marker indent new-indent new-loc)
    (save-match-data
      ;; Look for a list item on current or previous non-blank line
      (save-excursion
        (while (and (not (setq bounds (ein:markdown-cur-list-item-bounds)))
                    (not (bobp))
                    (ein:markdown-cur-line-blank-p))
          (forward-line -1)))
      (when bounds
        (cond ((save-excursion
                 (skip-chars-backward " \t")
                 (looking-at-p ein:markdown-regex-list))
               (beginning-of-line)
               (insert "\n")
               (forward-line -1))
              ((not (ein:markdown-cur-line-blank-p))
               (newline)))
        (setq new-loc (point)))
      ;; Look ahead for a list item on next non-blank line
      (unless bounds
        (save-excursion
          (while (and (null bounds)
                      (not (eobp))
                      (ein:markdown-cur-line-blank-p))
            (forward-line)
            (setq bounds (ein:markdown-cur-list-item-bounds))))
        (when bounds
          (setq new-loc (point))
          (unless (ein:markdown-cur-line-blank-p)
            (newline))))
      (if (not bounds)
          ;; When not in a list, start a new unordered one
          (progn
            (unless (ein:markdown-cur-line-blank-p)
              (insert "\n"))
            (insert ein:markdown-unordered-list-item-prefix))
        ;; Compute indentation and marker for new list item
        (setq cur-indent (nth 2 bounds))
        (setq marker (nth 4 bounds))
        ;; If current item is a GFM checkbox, insert new unchecked checkbox.
        (when (nth 5 bounds)
          (setq marker
                (concat marker
                        (replace-regexp-in-string "[Xx]" " " (nth 5 bounds)))))
        (cond
         ;; Dedent: decrement indentation, find previous marker.
         ((= arg 4)
          (setq indent (max (- cur-indent 4) 0))
          (let ((prev-bounds
                 (save-excursion
                   (goto-char (nth 0 bounds))
                   (when (ein:markdown-up-list)
                     (ein:markdown-cur-list-item-bounds)))))
            (when prev-bounds
              (setq marker (nth 4 prev-bounds)))))
         ;; Indent: increment indentation by 4, use same marker.
         ((= arg 16) (setq indent (+ cur-indent 4)))
         ;; Same level: keep current indentation and marker.
         (t (setq indent cur-indent)))
        (setq new-indent (make-string indent 32))
        (goto-char new-loc)
        (cond
         ;; Ordered list
         ((string-match-p "[0-9]" marker)
          (if (= arg 16) ;; starting a new column indented one more level
              (insert (concat new-indent "1. "))
            ;; Don't use previous match-data
            (set-match-data nil)
            ;; travel up to the last item and pick the correct number.  If
            ;; the argument was nil, "new-indent = cur-indent" is the same,
            ;; so we don't need special treatment. Neat.
            (save-excursion
              (while (and (not (looking-at (concat new-indent "\\([0-9]+\\)\\(\\.[ \t]*\\)")))
                          (>= (forward-line -1) 0))))
            (let* ((old-prefix (match-string 1))
                   (old-spacing (match-string 2))
                   (new-prefix (if old-prefix
                                   (int-to-string (1+ (string-to-number old-prefix)))
                                 "1"))
                   (space-adjust (- (length old-prefix) (length new-prefix)))
                   (new-spacing (if (and (match-string 2)
                                         (not (string-match-p "\t" old-spacing))
                                         (< space-adjust 0)
                                         (> space-adjust (- 1 (length (match-string 2)))))
                                    (substring (match-string 2) 0 space-adjust)
                                  (or old-spacing ". "))))
              (insert (concat new-indent new-prefix new-spacing)))))
         ;; Unordered list, GFM task list, or ordered list with hash mark
         ((string-match-p "[\\*\\+-]\\|#\\." marker)
          (insert new-indent marker))))
      ;; Propertize the newly inserted list item now
      (ein:markdown-syntax-propertize-list-items (point-at-bol) (point-at-eol)))))

(defun ein:markdown-move-list-item-up ()
  "Move the current list item up in the list when possible.
In nested lists, move child items with the parent item."
  (interactive)
  (let (cur prev old)
    (when (setq cur (ein:markdown-cur-list-item-bounds))
      (setq old (point))
      (goto-char (nth 0 cur))
      (if (ein:markdown-prev-list-item (nth 3 cur))
          (progn
            (setq prev (ein:markdown-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (transpose-regions (nth 0 prev) (nth 1 prev)
                                     (nth 0 cur) (nth 1 cur) t)
                  (goto-char (+ (nth 0 prev) (- old (nth 0 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun ein:markdown-move-list-item-down ()
  "Move the current list item down in the list when possible.
In nested lists, move child items with the parent item."
  (interactive)
  (let (cur next old)
    (when (setq cur (ein:markdown-cur-list-item-bounds))
      (setq old (point))
      (if (ein:markdown-next-list-item (nth 3 cur))
          (progn
            (setq next (ein:markdown-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (transpose-regions (nth 0 cur) (nth 1 cur)
                                     (nth 0 next) (nth 1 next) nil)
                  (goto-char (+ old (- (nth 1 next) (nth 1 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun ein:markdown-demote-list-item (&optional bounds)
  "Indent (or demote) the current list item.
Optionally, BOUNDS of the current list item may be provided if available.
In nested lists, demote child items as well."
  (interactive)
  (when (or bounds (setq bounds (ein:markdown-cur-list-item-bounds)))
    (save-excursion
      (let* ((item-start (set-marker (make-marker) (nth 0 bounds)))
             (item-end (set-marker (make-marker) (nth 1 bounds)))
             (list-start (progn (ein:markdown-beginning-of-list)
                                (set-marker (make-marker) (point))))
             (list-end (progn (ein:markdown-end-of-list)
                              (set-marker (make-marker) (point)))))
        (goto-char item-start)
        (while (< (point) item-end)
          (unless (ein:markdown-cur-line-blank-p)
            (insert (make-string ein:markdown-list-indent-width ? )))
          (forward-line))
        (ein:markdown-syntax-propertize-list-items list-start list-end)))))

(defun ein:markdown-promote-list-item (&optional bounds)
  "Unindent (or promote) the current list item.
Optionally, BOUNDS of the current list item may be provided if available.
In nested lists, demote child items as well."
  (interactive)
  (when (or bounds (setq bounds (ein:markdown-cur-list-item-bounds)))
    (save-excursion
      (save-match-data
        (let ((item-start (set-marker (make-marker) (nth 0 bounds)))
              (item-end (set-marker (make-marker) (nth 1 bounds)))
              (list-start (progn (ein:markdown-beginning-of-list)
                                 (set-marker (make-marker) (point))))
              (list-end (progn (ein:markdown-end-of-list)
                               (set-marker (make-marker) (point))))
              num regexp)
          (goto-char item-start)
          (when (looking-at (format "^[ ]\\{1,%d\\}"
                                    ein:markdown-list-indent-width))
            (setq num (- (match-end 0) (match-beginning 0)))
            (setq regexp (format "^[ ]\\{1,%d\\}" num))
            (while (and (< (point) item-end)
                        (re-search-forward regexp item-end t))
              (replace-match "" nil nil)
              (forward-line))
            (ein:markdown-syntax-propertize-list-items list-start list-end)))))))

(defun ein:markdown-cleanup-list-numbers-level (&optional pfx)
  "Update the numbering for level PFX (as a string of spaces).

Assume that the previously found match was for a numbered item in
a list."
  (let ((cpfx pfx)
        (idx 0)
        (continue t)
        (step t)
        (sep nil))
    (while (and continue (not (eobp)))
      (setq step t)
      (cond
       ((looking-at "^\\([\s-]*\\)[0-9]+\\. ")
        (setq cpfx (match-string-no-properties 1))
        (cond
         ((string= cpfx pfx)
          (save-excursion
            (replace-match
             (concat pfx (number-to-string (setq idx (1+ idx))) ". ")))
          (setq sep nil))
         ;; indented a level
         ((string< pfx cpfx)
          (setq sep (ein:markdown-cleanup-list-numbers-level cpfx))
          (setq step nil))
         ;; exit the loop
         (t
          (setq step nil)
          (setq continue nil))))

       ((looking-at "^\\([\s-]*\\)[^ \t\n\r].*$")
        (setq cpfx (match-string-no-properties 1))
        (cond
         ;; reset if separated before
         ((string= cpfx pfx) (when sep (setq idx 0)))
         ((string< cpfx pfx)
          (setq step nil)
          (setq continue nil))))
       (t (setq sep t)))

      (when step
        (beginning-of-line)
        (setq continue (= (forward-line) 0))))
    sep))

(defun ein:markdown-cleanup-list-numbers ()
  "Update the numbering of ordered lists."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ein:markdown-cleanup-list-numbers-level "")))


;;; Movement ==================================================================

(defun ein:markdown-beginning-of-defun (&optional arg)
  "`beginning-of-defun-function' for ein:markdown.
This is used to find the beginning of the defun and should behave
like ‘beginning-of-defun’, returning non-nil if it found the
beginning of a defun.  It moves the point backward, right before a
heading which defines a defun.  When ARG is non-nil, repeat that
many times.  When ARG is negative, move forward to the ARG-th
following section."
  (or arg (setq arg 1))
  (when (< arg 0) (end-of-line))
  ;; Adjust position for setext headings.
  (when (and (thing-at-point-looking-at ein:markdown-regex-header-setext)
             (not (= (point) (match-beginning 0)))
             (not (ein:markdown-code-block-at-point-p)))
    (goto-char (match-end 0)))
  (let (found)
    ;; Move backward with positive argument.
    (while (and (not (bobp)) (> arg 0))
      (setq found nil)
      (while (and (not found)
                  (not (bobp))
                  (re-search-backward ein:markdown-regex-header nil 'move))
        (when (not (ein:markdown-code-block-at-pos (match-beginning 0))))
        (setq found (match-beginning 0)))
      (setq arg (1- arg)))
    ;; Move forward with negative argument.
    (while (and (not (eobp)) (< arg 0))
      (setq found nil)
      (while (and (not found)
                  (not (eobp))
                  (re-search-forward ein:markdown-regex-header nil 'move))
        (when (not (ein:markdown-code-block-at-pos (match-beginning 0))))
        (setq found (match-beginning 0)))
      (setq arg (1+ arg)))
    (when found
      (beginning-of-line)
      t)))

(defun ein:markdown-end-of-defun ()
  "`end-of-defun-function’ for ein:markdown.
This is used to find the end of the defun at point.
It is called with no argument, right after calling ‘beginning-of-defun-raw’,
so it can assume that point is at the beginning of the defun body.
It should move point to the first position after the defun."
  (or (eobp) (forward-char 1))
  (let (found)
    (while (and (not found)
                (not (eobp))
                (re-search-forward ein:markdown-regex-header nil 'move))
      (when (not (ein:markdown-code-block-at-pos (match-beginning 0)))
        (setq found (match-beginning 0))))
    (when found
      (goto-char found)
      (skip-syntax-backward "-"))))

(make-obsolete 'ein:markdown-beginning-of-block 'ein:markdown-beginning-of-text-block "v2.2")

(defun ein:markdown-beginning-of-text-block ()
  "Move backward to previous beginning of a plain text block.
This function simply looks for blank lines without considering
the surrounding context in light of ein:markdown syntax.  For that, see
`markdown-backward-block'."
  (interactive)
  (let ((start (point)))
    (if (re-search-backward ein:markdown-regex-block-separator nil t)
        (goto-char (match-end 0))
      (goto-char (point-min)))
    (when (and (= start (point)) (not (bobp)))
      (forward-line -1)
      (if (re-search-backward ein:markdown-regex-block-separator nil t)
          (goto-char (match-end 0))
        (goto-char (point-min))))))

(make-obsolete 'ein:markdown-end-of-block 'ein:markdown-end-of-text-block "v2.2")

(defun ein:markdown-end-of-text-block ()
  "Move forward to next beginning of a plain text block.
This function simply looks for blank lines without considering
the surrounding context in light of ein:markdown syntax.  For that, see
`markdown-forward-block'."
  (interactive)
  (beginning-of-line)
  (skip-chars-forward " \t\n")
  (when (= (point) (point-min))
    (forward-char))
  (if (re-search-forward ein:markdown-regex-block-separator nil t)
      (goto-char (match-end 0))
    (goto-char (point-max)))
  (skip-chars-backward " \t\n")
  (forward-line))

(defun ein:markdown-backward-paragraph (&optional arg)
  "Move the point to the start of the current paragraph.
With argument ARG, do it ARG times; a negative argument ARG = -N
means move forward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (ein:markdown-forward-paragraph (- arg))
    (dotimes (_ arg)
      ;; Skip over whitespace in between paragraphs when moving backward.
      (skip-chars-backward " \t\n")
      (beginning-of-line)
      ;; Skip over code block endings.
      (when (ein:markdown-range-properties-exist
            (point-at-bol) (point-at-eol)
            '(ein:markdown-tilde-fence-end))
        (forward-line -1))
      ;; Skip over blank lines inside blockquotes.
      (while (and (not (eobp))
                  (looking-at ein:markdown-regex-blockquote)
                  (= (length (match-string 3)) 0))
        (forward-line -1))
      ;; Proceed forward based on the type of block of paragraph.
      (let (bounds skip)
        (cond
         ;; Blockquotes
         ((looking-at ein:markdown-regex-blockquote)
          (while (and (not (bobp))
                      (looking-at ein:markdown-regex-blockquote)
                      (> (length (match-string 3)) 0)) ;; not blank
            (forward-line -1))
          (forward-line))
         ;; List items
         ((setq bounds (ein:markdown-cur-list-item-bounds))
          (goto-char (nth 0 bounds)))
         ;; Other
         (t
          (while (and (not (bobp))
                      (not skip)
                      (not (ein:markdown-cur-line-blank-p))
                      (not (looking-at ein:markdown-regex-blockquote))
                      (not (ein:markdown-range-properties-exist
                            (point-at-bol) (point-at-eol)
                            '(ein:markdown-tilde-fence-end))))
            (setq skip (ein:markdown-range-properties-exist
                            (point-at-bol) (point-at-eol)
                            '(ein:markdown-tilde-fence-begin)))
            (forward-line -1))
          (unless (bobp)
            (forward-line 1))))))))

(defun ein:markdown-forward-paragraph (&optional arg)
  "Move forward to the next end of a paragraph.
With argument ARG, do it ARG times; a negative argument ARG = -N
means move backward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (ein:markdown-backward-paragraph (- arg))
    (dotimes (_ arg)
      ;; Skip whitespace in between paragraphs.
      (when (ein:markdown-cur-line-blank-p)
        (skip-syntax-forward "-")
        (beginning-of-line))
      ;; Proceed forward based on the type of block.
      (let (bounds skip)
        (cond
         ;; Blockquotes
         ((looking-at ein:markdown-regex-blockquote)
          ;; Skip over blank lines inside blockquotes.
          (while (and (not (eobp))
                      (looking-at ein:markdown-regex-blockquote)
                      (= (length (match-string 3)) 0))
            (forward-line))
          ;; Move to end of quoted text block
          (while (and (not (eobp))
                      (looking-at ein:markdown-regex-blockquote)
                      (> (length (match-string 3)) 0)) ;; not blank
            (forward-line)))
         ;; List items
         ((and (ein:markdown-cur-list-item-bounds)
               (setq bounds (ein:markdown-next-list-item-bounds)))
          (goto-char (nth 0 bounds)))
         ;; Other
         (t
          (forward-line)
          (while (and (not (eobp))
                      (not skip)
                      (not (ein:markdown-cur-line-blank-p))
                      (not (looking-at ein:markdown-regex-blockquote))
                      (not (ein:markdown-range-properties-exist
                            (point-at-bol) (point-at-eol)
                            '(ein:markdown-tilde-fence-begin))))
            (setq skip (ein:markdown-range-properties-exist
                        (point-at-bol) (point-at-eol)
                        '(ein:markdown-tilde-fence-end)))
            (forward-line))))))))

(defun ein:markdown-backward-block (&optional arg)
  "Move the point to the start of the current ein:markdown block.
Moves across complete code blocks, list items, and blockquotes,
but otherwise stops at blank lines, headers, and horizontal
rules.  With argument ARG, do it ARG times; a negative argument
ARG = -N means move forward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (ein:markdown-forward-block (- arg))
    (dotimes (_ arg)
      ;; Skip over whitespace in between blocks when moving backward,
      ;; unless at a block boundary with no whitespace.
      (skip-syntax-backward "-")
      (beginning-of-line)
      ;; Proceed forward based on the type of block.
      (cond
       ;; Code blocks
       ((and (ein:markdown-code-block-at-pos (point)) ;; this line
             (ein:markdown-code-block-at-pos (point-at-bol 0))) ;; previous line
        (forward-line -1)
        (while (and (ein:markdown-code-block-at-point-p) (not (bobp)))
          (forward-line -1))
        (forward-line))
       ;; Headings
       ((ein:markdown-heading-at-point)
        (goto-char (match-beginning 0)))
       ;; Horizontal rules
       ((looking-at ein:markdown-regex-hr))
       ;; Blockquotes
       ((looking-at ein:markdown-regex-blockquote)
        (forward-line -1)
        (while (and (looking-at ein:markdown-regex-blockquote)
                    (not (bobp)))
          (forward-line -1))
        (forward-line))
       ;; List items
       ((ein:markdown-cur-list-item-bounds)
        (ein:markdown-beginning-of-list))
       ;; Other
       (t
        ;; Move forward in case it is a one line regular paragraph.
        (unless (ein:markdown-next-line-blank-p)
          (forward-line))
        (unless (ein:markdown-prev-line-blank-p)
          (ein:markdown-backward-paragraph)))))))

(defun ein:markdown-forward-block (&optional arg)
  "Move forward to the next end of a ein:markdown block.
Moves across complete code blocks, list items, and blockquotes,
but otherwise stops at blank lines, headers, and horizontal
rules.  With argument ARG, do it ARG times; a negative argument
ARG = -N means move backward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (ein:markdown-backward-block (- arg))
    (dotimes (_ arg)
      ;; Skip over whitespace in between blocks when moving forward.
      (if (ein:markdown-cur-line-blank-p)
          (skip-syntax-forward "-")
        (beginning-of-line))
      ;; Proceed forward based on the type of block.
      (cond
       ;; Code blocks
       ((ein:markdown-code-block-at-point-p)
        (forward-line)
        (while (and (ein:markdown-code-block-at-point-p) (not (eobp)))
          (forward-line)))
       ;; Headings
       ((looking-at ein:markdown-regex-header)
        (goto-char (or (match-end 4) (match-end 2) (match-end 3)))
        (forward-line))
       ;; Horizontal rules
       ((looking-at ein:markdown-regex-hr)
        (forward-line))
       ;; Blockquotes
       ((looking-at ein:markdown-regex-blockquote)
        (forward-line)
        (while (and (looking-at ein:markdown-regex-blockquote) (not (eobp)))
          (forward-line)))
       ;; List items
       ((ein:markdown-cur-list-item-bounds)
        (ein:markdown-end-of-list)
        (forward-line))
       ;; Other
       (t (ein:markdown-forward-paragraph))))
    (skip-syntax-backward "-")
    (unless (eobp)
      (forward-char 1))))

(defun ein:markdown-backward-page (&optional count)
  "Move backward to boundary of the current toplevel section.
With COUNT, repeat, or go forward if negative."
  (interactive "p")
  (or count (setq count 1))
  (if (< count 0)
      (ein:markdown-forward-page (- count))
    (skip-syntax-backward "-")
    (or (ein:markdown-back-to-heading-over-code-block t t)
        (goto-char (point-min)))
    (when (looking-at ein:markdown-regex-header)
      (let ((level (ein:markdown-outline-level)))
        (when (> level 1) (ein:markdown-up-heading level))
        (when (> count 1)
          (condition-case nil
              (ein:markdown-backward-same-level (1- count))
            (error (goto-char (point-min)))))))))

(defun ein:markdown-forward-page (&optional count)
  "Move forward to boundary of the current toplevel section.
With COUNT, repeat, or go backward if negative."
  (interactive "p")
  (or count (setq count 1))
  (if (< count 0)
      (ein:markdown-backward-page (- count))
    (if (ein:markdown-back-to-heading-over-code-block t t)
        (let ((level (ein:markdown-outline-level)))
          (when (> level 1) (ein:markdown-up-heading level))
          (condition-case nil
              (ein:markdown-forward-same-level count)
            (error (goto-char (point-max)))))
      (ein:markdown-next-visible-heading 1))))

(defun ein:markdown-next-link ()
  "Jump to next inline, reference, or wiki link.
If successful, return point.  Otherwise, return nil.
See `markdown-wiki-link-p' and `markdown-previous-wiki-link'."
  (interactive)
  (let ((opoint (point)))
    (when (ein:markdown-link-p)
      ;; At a link already, move past it.
      (goto-char (+ (match-end 0) 1)))
    (while (and (re-search-forward (ein:markdown-make-regex-link-generic) nil t)
                (ein:markdown-code-block-at-point-p)
                (< (point) (point-max))))
    (if (and (not (eq (point) opoint)) (ein:markdown-link-p))
        ;; Group 1 will move past non-escape character in wiki link regexp.
        ;; Go to beginning of group zero for all other link types.
        (goto-char (or (match-beginning 1) (match-beginning 0)))
      (goto-char opoint)
      nil)))

(defun ein:markdown-previous-link ()
  "Jump to previous wiki link.
If successful, return point.  Otherwise, return nil.
See `markdown-wiki-link-p' and `markdown-next-wiki-link'."
  (interactive)
  (let ((opoint (point)))
    (while (and (re-search-backward (ein:markdown-make-regex-link-generic) nil t)
                (ein:markdown-code-block-at-point-p)
                (> (point) (point-min))))
    (if (and (not (eq (point) opoint)) (ein:markdown-link-p))
        (goto-char (or (match-beginning 1) (match-beginning 0)))
      (goto-char opoint)
      nil)))


;;; Outline ===================================================================

(defun ein:markdown-move-heading-common (move-fn &optional arg adjust)
  "Wrapper for `outline-mode' functions to skip false positives.
MOVE-FN is a function and ARG is its argument. For example,
headings inside preformatted code blocks may match
`outline-regexp' but should not be considered as headings.
When ADJUST is non-nil, adjust the point for interactive calls
to avoid leaving the point at invisible markup.  This adjustment
generally should only be done for interactive calls, since other
functions may expect the point to be at the beginning of the
regular expression."
  (let ((prev -1) (start (point)))
    (if arg (funcall move-fn arg) (funcall move-fn))
    (while (and (/= prev (point)) (ein:markdown-code-block-at-point-p))
      (setq prev (point))
      (if arg (funcall move-fn arg) (funcall move-fn)))
    ;; Adjust point for setext headings and invisible text.
    (save-match-data
      (when (and adjust (thing-at-point-looking-at ein:markdown-regex-header))
        (goto-char (or (match-beginning 1) (match-beginning 4)))))
    (if (= (point) start) nil (point))))

(defun ein:markdown-next-visible-heading (arg)
  "Move to the next visible heading line of any level.
With argument, repeats or can move backward if negative. ARG is
passed to `outline-next-visible-heading'."
  (interactive "p")
  (ein:markdown-move-heading-common #'outline-next-visible-heading arg 'adjust))

(defun ein:markdown-previous-visible-heading (arg)
  "Move to the previous visible heading line of any level.
With argument, repeats or can move backward if negative. ARG is
passed to `outline-previous-visible-heading'."
  (interactive "p")
  (ein:markdown-move-heading-common #'outline-previous-visible-heading arg 'adjust))

(defun ein:markdown-next-heading ()
  "Move to the next heading line of any level."
  (ein:markdown-move-heading-common #'outline-next-heading))

(defun ein:markdown-previous-heading ()
  "Move to the previous heading line of any level."
  (ein:markdown-move-heading-common #'outline-previous-heading))

(defun ein:markdown-back-to-heading-over-code-block (&optional invisible-ok no-error)
  "Move back to the beginning of the previous heading.
Returns t if the point is at a heading, the location if a heading
was found, and nil otherwise.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil.  Throw an error if there is no previous heading unless
NO-ERROR is non-nil.
Leaves match data intact for `markdown-regex-header'."
  (beginning-of-line)
  (or (and (ein:markdown-heading-at-point)
           (not (ein:markdown-code-block-at-point-p)))
      (let (found)
        (save-excursion
          (while (and (not found)
                      (re-search-backward ein:markdown-regex-header nil t))
            (when (and (or invisible-ok (not (outline-invisible-p)))
                       (not (ein:markdown-code-block-at-point-p)))
              (setq found (point))))
          (if (not found)
              (unless no-error (user-error "Before first heading"))
            (setq found (point))))
        (when found (goto-char found)))))

(defun ein:markdown-forward-same-level (arg)
  "Move forward to the ARG'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (ein:markdown-back-to-heading-over-code-block)
  (ein:markdown-move-heading-common #'outline-forward-same-level arg 'adjust))

(defun ein:markdown-backward-same-level (arg)
  "Move backward to the ARG'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (ein:markdown-back-to-heading-over-code-block)
  (while (> arg 0)
    (let ((point-to-move-to
           (save-excursion
             (ein:markdown-move-heading-common #'outline-get-last-sibling nil 'adjust))))
      (if point-to-move-to
          (progn
            (goto-char point-to-move-to)
            (setq arg (1- arg)))
        (user-error "No previous same-level heading")))))

(defun ein:markdown-up-heading (arg)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  (and (called-interactively-p 'any)
       (not (eq last-command 'ein:markdown-up-heading)) (push-mark))
  (ein:markdown-move-heading-common #'outline-up-heading arg 'adjust))

(defun ein:markdown-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (ein:markdown-move-heading-common #'outline-back-to-heading invisible-ok))

(defalias 'ein:markdown-end-of-heading 'outline-end-of-heading)

(defun ein:markdown-on-heading-p ()
  "Return non-nil if point is on a heading line."
  (get-text-property (point-at-bol) 'ein:markdown-heading))

(defun ein:markdown-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil.
Derived from `org-end-of-subtree'."
  (ein:markdown-back-to-heading invisible-OK)
  (let ((first t)
        (level (ein:markdown-outline-level)))
    (while (and (not (eobp))
                (or first (> (ein:markdown-outline-level) level)))
      (setq first nil)
      (ein:markdown-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

(defun ein:markdown-outline-fix-visibility ()
  "Hide any false positive headings that should not be shown.
For example, headings inside preformatted code blocks may match
`outline-regexp' but should not be shown as headings when cycling.
Also, the ending --- line in metadata blocks appears to be a
setext header, but should not be folded."
  (save-excursion
    (goto-char (point-min))
    ;; Unhide any false positives in metadata blocks
    (when (ein:markdown-text-property-at-point 'ein:markdown-yaml-metadata-begin)
      (let ((body (progn (forward-line)
                         (ein:markdown-text-property-at-point
                          'ein:markdown-yaml-metadata-section))))
        (when body
          (let ((end (progn (goto-char (cl-second body))
                            (ein:markdown-text-property-at-point
                             'ein:markdown-yaml-metadata-end))))
            (outline-flag-region (point-min) (1+ (cl-second end)) nil)))))
    ;; Hide any false positives in code blocks
    (unless (outline-on-heading-p)
      (outline-next-visible-heading 1))
    (while (< (point) (point-max))
      (when (ein:markdown-code-block-at-point-p)
        (outline-flag-region (1- (point-at-bol)) (point-at-eol) t))
      (outline-next-visible-heading 1))))

(defvar ein:markdown-cycle-global-status 1)
(defvar ein:markdown-cycle-subtree-status nil)

(defun ein:markdown-next-preface ()
  (let (finish)
    (while (and (not finish) (re-search-forward (concat "\n\\(?:" outline-regexp "\\)")
                                                nil 'move))
      (unless (ein:markdown-code-block-at-point-p)
        (goto-char (match-beginning 0))
        (setq finish t))))
  (when (and (bolp) (or outline-blank-line (eobp)) (not (bobp)))
    (forward-char -1)))

(defun ein:markdown-show-entry ()
  (save-excursion
    (outline-back-to-heading t)
    (outline-flag-region (1- (point))
                         (progn
                           (ein:markdown-next-preface)
                           (if (= 1 (- (point-max) (point)))
                               (point-max)
                             (point)))
                         nil)))

;; This function was originally derived from `org-cycle' from org.el.
(defun ein:markdown-cycle (&optional arg)
  "Visibility cycling for ein:markdown mode.
If ARG is t, perform global visibility cycling.  If the point is
at an atx-style header, cycle visibility of the corresponding
subtree.  Otherwise, indent the current line or insert a tab,
as appropriate, by calling `indent-for-tab-command'."
  (interactive "P")
  (cond

   ;; Global cycling
   ((eq arg t)
    (cond
     ;; Move from overview to contents
     ((and (eq last-command this-command)
           (eq ein:markdown-cycle-global-status 2))
      (ein:markdown-hide-sublevels 1)
      (message "CONTENTS")
      (setq ein:markdown-cycle-global-status 3)
      (ein:markdown-outline-fix-visibility))
     ;; Move from contents to all
     ((and (eq last-command this-command)
           (eq ein:markdown-cycle-global-status 3))
      (ein:markdown-show-all)
      (message "SHOW ALL")
      (setq ein:markdown-cycle-global-status 1))
     ;; Defaults to overview
     (t
      (ein:markdown-hide-body)
      (message "OVERVIEW")
      (setq ein:markdown-cycle-global-status 2)
      (ein:markdown-outline-fix-visibility))))

   ;; At a heading: rotate between three different views
   ((save-excursion (beginning-of-line 1) (ein:markdown-on-heading-p))
    (ein:markdown-back-to-heading)
    (let ((goal-column 0) eoh eol eos)
      ;; Determine boundaries
      (save-excursion
        (ein:markdown-back-to-heading)
        (save-excursion
          (beginning-of-line 2)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2)) (setq eol (point)))
        (ein:markdown-end-of-heading)   (setq eoh (point))
        (ein:markdown-end-of-subtree t)
        (skip-chars-forward " \t\n")
        (beginning-of-line 1) ; in case this is an item
        (setq eos (1- (point))))
      ;; Find out what to do next and set `this-command'
      (cond
       ;; Nothing is hidden behind this heading
       ((= eos eoh)
        (message "EMPTY ENTRY")
        (setq ein:markdown-cycle-subtree-status nil))
       ;; Entire subtree is hidden in one line: open it
       ((>= eol eos)
        (ein:markdown-show-entry)
        (ein:markdown-show-children)
        (message "CHILDREN")
        (setq ein:markdown-cycle-subtree-status 'children))
       ;; We just showed the children, now show everything.
       ((and (eq last-command this-command)
             (eq ein:markdown-cycle-subtree-status 'children))
        (ein:markdown-show-subtree)
        (message "SUBTREE")
        (setq ein:markdown-cycle-subtree-status 'subtree))
       ;; Default action: hide the subtree.
       (t
        (ein:markdown-hide-subtree)
        (message "FOLDED")
        (setq ein:markdown-cycle-subtree-status 'folded)))))

   ;; In a table, move forward by one cell
   ((ein:markdown-table-at-point-p)
    (call-interactively #'ein:markdown-table-forward-cell))

   ;; Otherwise, indent as appropriate
   (t
    (indent-for-tab-command))))

(defun ein:markdown-shifttab ()
  "Handle S-TAB keybinding based on context.
When in a table, move backward one cell.
Otherwise, cycle global heading visibility by calling
`markdown-cycle' with argument t."
  (interactive)
  (cond ((ein:markdown-table-at-point-p)
         (call-interactively #'ein:markdown-table-backward-cell))
        (t (ein:markdown-cycle t))))

(defun ein:markdown-outline-level ()
  "Return the depth to which a statement is nested in the outline."
  (cond
   ((and (match-beginning 0)
         (ein:markdown-code-block-at-pos (match-beginning 0)))
    7) ;; Only 6 header levels are defined.
   ((match-end 2) 1)
   ((match-end 3) 2)
   ((match-end 4)
    (length (ein:markdown-trim-whitespace (match-string-no-properties 4))))))

(defun ein:markdown-promote-subtree (&optional arg)
  "Promote the current subtree of ATX headings.
Note that ein:markdown does not support heading levels higher than
six and therefore level-six headings will not be promoted
further. If ARG is non-nil promote the heading, otherwise
demote."
  (interactive "*P")
  (save-excursion
    (when (and (or (thing-at-point-looking-at ein:markdown-regex-header-atx)
                   (re-search-backward ein:markdown-regex-header-atx nil t))
               (not (ein:markdown-code-block-at-point-p)))
      (let ((level (length (match-string 1)))
            (promote-or-demote (if arg 1 -1))
            (remove 't))
        (ein:markdown-cycle-atx promote-or-demote remove)
        (catch 'end-of-subtree
          (while (and (ein:markdown-next-heading)
                      (looking-at ein:markdown-regex-header-atx))
            ;; Exit if this not a higher level heading; promote otherwise.
            (if (and (looking-at ein:markdown-regex-header-atx)
                     (<= (length (match-string-no-properties 1)) level))
                (throw 'end-of-subtree nil)
              (ein:markdown-cycle-atx promote-or-demote remove))))))))

(defun ein:markdown-demote-subtree ()
  "Demote the current subtree of ATX headings."
  (interactive)
  (ein:markdown-promote-subtree t))

(defun ein:markdown-move-subtree-up ()
  "Move the current subtree of ATX headings up."
  (interactive)
  (outline-move-subtree-up 1))

(defun ein:markdown-move-subtree-down ()
  "Move the current subtree of ATX headings down."
  (interactive)
  (outline-move-subtree-down 1))

(defun ein:markdown-outline-next ()
  "Move to next list item, when in a list, or next visible heading."
  (interactive)
  (let ((bounds (ein:markdown-next-list-item-bounds)))
    (if bounds
        (goto-char (nth 0 bounds))
      (ein:markdown-next-visible-heading 1))))

(defun ein:markdown-outline-previous ()
  "Move to previous list item, when in a list, or previous visible heading."
  (interactive)
  (let ((bounds (ein:markdown-prev-list-item-bounds)))
    (if bounds
        (goto-char (nth 0 bounds))
      (ein:markdown-previous-visible-heading 1))))

(defun ein:markdown-outline-next-same-level ()
  "Move to next list item or heading of same level."
  (interactive)
  (let ((bounds (ein:markdown-cur-list-item-bounds)))
    (if bounds
        (ein:markdown-next-list-item (nth 3 bounds))
      (ein:markdown-forward-same-level 1))))

(defun ein:markdown-outline-previous-same-level ()
  "Move to previous list item or heading of same level."
  (interactive)
  (let ((bounds (ein:markdown-cur-list-item-bounds)))
    (if bounds
        (ein:markdown-prev-list-item (nth 3 bounds))
      (ein:markdown-backward-same-level 1))))

(defun ein:markdown-outline-up ()
  "Move to previous list item, when in a list, or next heading."
  (interactive)
  (unless (ein:markdown-up-list)
    (ein:markdown-up-heading 1)))


;;; Marking and Narrowing =====================================================

(defun ein:markdown-mark-paragraph ()
  "Put mark at end of this block, point at beginning.
The block marked is the one that contains point or follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next block after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (ein:markdown-forward-paragraph)
         (point)))
    (let ((beginning-of-defun-function 'ein:markdown-backward-paragraph)
          (end-of-defun-function 'ein:markdown-forward-paragraph))
      (mark-defun))))

(defun ein:markdown-mark-block ()
  "Put mark at end of this block, point at beginning.
The block marked is the one that contains point or follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next block after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (ein:markdown-forward-block)
         (point)))
    (let ((beginning-of-defun-function 'ein:markdown-backward-block)
          (end-of-defun-function 'ein:markdown-forward-block))
      (mark-defun))))

(defun ein:markdown-narrow-to-block ()
  "Make text outside current block invisible.
The current block is the one that contains point or follows point."
  (interactive)
  (let ((beginning-of-defun-function 'ein:markdown-backward-block)
        (end-of-defun-function 'ein:markdown-forward-block))
    (narrow-to-defun)))

(defun ein:markdown-mark-text-block ()
  "Put mark at end of this plain text block, point at beginning.
The block marked is the one that contains point or follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next block after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (ein:markdown-end-of-text-block)
         (point)))
    (let ((beginning-of-defun-function 'ein:markdown-beginning-of-text-block)
          (end-of-defun-function 'ein:markdown-end-of-text-block))
      (mark-defun))))

(defun ein:markdown-mark-page ()
  "Put mark at end of this top level section, point at beginning.
The top level section marked is the one that contains point or
follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next page after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (ein:markdown-forward-page)
         (point)))
    (let ((beginning-of-defun-function 'ein:markdown-backward-page)
          (end-of-defun-function 'ein:markdown-forward-page))
      (mark-defun))))

(defun ein:markdown-narrow-to-page ()
  "Make text outside current top level section invisible.
The current section is the one that contains point or follows point."
  (interactive)
  (let ((beginning-of-defun-function 'ein:markdown-backward-page)
        (end-of-defun-function 'ein:markdown-forward-page))
    (narrow-to-defun)))

(defun ein:markdown-mark-subtree ()
  "Mark the current subtree.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (let ((beg))
    (if (ein:markdown-heading-at-point)
        (beginning-of-line)
      (ein:markdown-previous-visible-heading 1))
    (setq beg (point))
    (ein:markdown-end-of-subtree)
    (push-mark (point) nil t)
    (goto-char beg)))

(defun ein:markdown-narrow-to-subtree ()
  "Narrow buffer to the current subtree."
  (interactive)
  (save-excursion
    (save-match-data
      (narrow-to-region
       (progn (ein:markdown-back-to-heading-over-code-block t) (point))
       (progn (ein:markdown-end-of-subtree)
          (if (and (ein:markdown-heading-at-point) (not (eobp)))
          (backward-char 1))
          (point))))))


;;; Generic Structure Editing, Completion, and Cycling Commands ===============

(defun ein:markdown-move-up ()
  "Move thing at point up.
When in a list item, call `markdown-move-list-item-up'.
When in a table, call `markdown-table-move-row-up'.
Otherwise, move the current heading subtree up with
`markdown-move-subtree-up'."
  (interactive)
  (cond
   ((ein:markdown-list-item-at-point-p)
    (call-interactively #'ein:markdown-move-list-item-up))
   ((ein:markdown-table-at-point-p)
    (call-interactively #'ein:markdown-table-move-row-up))
   (t
    (call-interactively #'ein:markdown-move-subtree-up))))

(defun ein:markdown-move-down ()
  "Move thing at point down.
When in a list item, call `markdown-move-list-item-down'.
Otherwise, move the current heading subtree up with
`markdown-move-subtree-down'."
  (interactive)
  (cond
   ((ein:markdown-list-item-at-point-p)
    (call-interactively #'ein:markdown-move-list-item-down))
   ((ein:markdown-table-at-point-p)
    (call-interactively #'ein:markdown-table-move-row-down))
   (t
    (call-interactively #'ein:markdown-move-subtree-down))))

(defun ein:markdown-promote ()
  "Promote or move element at point to the left.
Depending on the context, this function will promote a heading or
list item at the point, move a table column to the left, or cycle
markup."
  (interactive)
  (let (bounds)
    (cond
     ;; Promote atx heading subtree
     ((thing-at-point-looking-at ein:markdown-regex-header-atx)
      (ein:markdown-promote-subtree))
     ;; Promote setext heading
     ((thing-at-point-looking-at ein:markdown-regex-header-setext)
      (ein:markdown-cycle-setext -1))
     ;; Promote horizonal rule
     ((thing-at-point-looking-at ein:markdown-regex-hr)
      (ein:markdown-cycle-hr -1))
     ;; Promote list item
     ((setq bounds (ein:markdown-cur-list-item-bounds))
      (ein:markdown-promote-list-item bounds))
     ;; Move table column to the left
     ((ein:markdown-table-at-point-p)
      (call-interactively #'ein:markdown-table-move-column-left))
     ;; Promote bold
     ((thing-at-point-looking-at ein:markdown-regex-bold)
      (ein:markdown-cycle-bold))
     ;; Promote italic
     ((thing-at-point-looking-at ein:markdown-regex-italic)
      (ein:markdown-cycle-italic))
     (t
      (user-error "Nothing to promote at point")))))

(defun ein:markdown-demote ()
  "Demote or move element at point to the right.
Depending on the context, this function will demote a heading or
list item at the point, move a table column to the right, or cycle
or remove markup."
  (interactive)
  (let (bounds)
    (cond
     ;; Demote atx heading subtree
     ((thing-at-point-looking-at ein:markdown-regex-header-atx)
      (ein:markdown-demote-subtree))
     ;; Demote setext heading
     ((thing-at-point-looking-at ein:markdown-regex-header-setext)
      (ein:markdown-cycle-setext 1))
     ;; Demote horizonal rule
     ((thing-at-point-looking-at ein:markdown-regex-hr)
      (ein:markdown-cycle-hr 1))
     ;; Demote list item
     ((setq bounds (ein:markdown-cur-list-item-bounds))
      (ein:markdown-demote-list-item bounds))
     ;; Move table column to the right
     ((ein:markdown-table-at-point-p)
      (call-interactively #'ein:markdown-table-move-column-right))
     ;; Demote bold
     ((thing-at-point-looking-at ein:markdown-regex-bold)
      (ein:markdown-cycle-bold))
     ;; Demote italic
     ((thing-at-point-looking-at ein:markdown-regex-italic)
      (ein:markdown-cycle-italic))
     (t
      (user-error "Nothing to demote at point")))))


;;; Commands ==================================================================

(defun ein:markdown (&optional output-buffer-name)
  "Run `markdown-command' on buffer, sending output to OUTPUT-BUFFER-NAME.
The output buffer name defaults to `markdown-output-buffer-name'.
Return the name of the output buffer used."
  (interactive)
  (save-window-excursion
    (let ((begin-region)
          (end-region))
      (if (ein:markdown-use-region-p)
          (setq begin-region (region-beginning)
                end-region (region-end))
        (setq begin-region (point-min)
              end-region (point-max)))

      (unless output-buffer-name
        (setq output-buffer-name ein:markdown-output-buffer-name))
      (let ((exit-code
             (cond
              ;; Handle case when `markdown-command' does not read from stdin
              ((and (stringp ein:markdown-command) ein:markdown-command-needs-filename)
               (if (not buffer-file-name)
                   (user-error "Must be visiting a file")
                 ;; Don’t use ‘shell-command’ because it’s not guaranteed to
                 ;; return the exit code of the process.
                 (shell-command-on-region
                  ;; Pass an empty region so that stdin is empty.
                  (point) (point)
                  (concat ein:markdown-command " "
                          (shell-quote-argument buffer-file-name))
                  output-buffer-name)))
              ;; Pass region to `markdown-command' via stdin
              (t
               (let ((buf (get-buffer-create output-buffer-name)))
                 (with-current-buffer buf
                   (setq buffer-read-only nil)
                   (erase-buffer))
                 (if (stringp ein:markdown-command)
                     (call-process-region begin-region end-region
                                          shell-file-name nil buf nil
                                          shell-command-switch ein:markdown-command)
                   (funcall ein:markdown-command begin-region end-region buf)
                   ;; If the ‘markdown-command’ function didn’t signal an
                   ;; error, assume it succeeded by binding ‘exit-code’ to 0.
                   0))))))
        ;; The exit code can be a signal description string, so don’t use ‘=’
        ;; or ‘zerop’.
        (unless (eq exit-code 0)
          (user-error "%s failed with exit code %s"
                      ein:markdown-command exit-code))))
    output-buffer-name))

(defun ein:markdown-standalone (&optional output-buffer-name)
  "Special function to provide standalone HTML output.
Insert the output in the buffer named OUTPUT-BUFFER-NAME."
  (interactive)
  (setq output-buffer-name (ein:markdown output-buffer-name))
  (with-current-buffer output-buffer-name
    (set-buffer output-buffer-name)
    (unless (ein:markdown-output-standalone-p)
      (ein:markdown-add-xhtml-header-and-footer output-buffer-name))
    (goto-char (point-min))
    (html-mode))
  output-buffer-name)

(defun ein:markdown-other-window (&optional output-buffer-name)
  "Run `markdown-command' on current buffer and display in other window.
When OUTPUT-BUFFER-NAME is given, insert the output in the buffer with
that name."
  (interactive)
  (ein:markdown-display-buffer-other-window
   (ein:markdown-standalone output-buffer-name)))

(defun ein:markdown-output-standalone-p ()
  "Determine whether `markdown-command' output is standalone XHTML.
Standalone XHTML output is identified by an occurrence of
`markdown-xhtml-standalone-regexp' in the first five lines of output."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (re-search-forward
       ein:markdown-xhtml-standalone-regexp
       (save-excursion (goto-char (point-min)) (forward-line 4) (point))
       t))))

(defun ein:markdown-stylesheet-link-string (stylesheet-path)
  (concat "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
          stylesheet-path
          "\"  />"))

(defun ein:markdown-add-xhtml-header-and-footer (title)
  "Wrap XHTML header and footer with given TITLE around current buffer."
  (goto-char (point-min))
  (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
          "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
          "\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\n"
          "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\n"
          "<head>\n<title>")
  (insert title)
  (insert "</title>\n")
  (unless (= (length ein:markdown-content-type) 0)
    (insert
     (format
      "<meta http-equiv=\"Content-Type\" content=\"%s;charset=%s\"/>\n"
      ein:markdown-content-type
      (or (and ein:markdown-coding-system
               (fboundp 'coding-system-get)
               (coding-system-get ein:markdown-coding-system
                                  'mime-charset))
          (and (fboundp 'coding-system-get)
               (coding-system-get buffer-file-coding-system
                                  'mime-charset))
          "utf-8"))))
  (if (> (length ein:markdown-css-paths) 0)
      (insert (mapconcat #'ein:markdown-stylesheet-link-string
                         ein:markdown-css-paths "\n")))
  (when (> (length ein:markdown-xhtml-header-content) 0)
    (insert ein:markdown-xhtml-header-content))
  (insert "\n</head>\n\n"
          "<body>\n\n")
  (when (> (length ein:markdown-xhtml-body-preamble) 0)
    (insert ein:markdown-xhtml-body-preamble "\n"))
  (goto-char (point-max))
  (when (> (length ein:markdown-xhtml-body-epilogue) 0)
    (insert "\n" ein:markdown-xhtml-body-epilogue))
  (insert "\n"
          "</body>\n"
          "</html>\n"))

(defun ein:markdown-visual-lines-between-points (beg end)
  (save-excursion
    (goto-char beg)
    (cl-loop with count = 0
             while (progn (end-of-visual-line)
                          (and (< (point) end) (line-move-visual 1 t)))
             do (cl-incf count)
             finally return count)))

(defun ein:markdown-get-point-back-lines (pt num-lines)
  (save-excursion
    (goto-char pt)
    (line-move-visual (- num-lines) t)
    ;; in testing, can occasionally overshoot the number of lines to traverse
    (let ((actual-num-lines (ein:markdown-visual-lines-between-points (point) pt)))
      (when (> actual-num-lines num-lines)
        (line-move-visual (- actual-num-lines num-lines) t)))
    (point)))

(defun ein:markdown-get-other-window ()
  "Find another window to display preview or output content."
  (cond
   ((memq ein:markdown-split-window-direction '(vertical below))
    (or (window-in-direction 'below) (split-window-vertically)))
   ((memq ein:markdown-split-window-direction '(horizontal right))
    (or (window-in-direction 'right) (split-window-horizontally)))
   (t (split-window-sensibly (get-buffer-window)))))

(defun ein:markdown-display-buffer-other-window (buf)
  "Display preview or output buffer BUF in another window."
  (let ((cur-buf (current-buffer))
        (window (ein:markdown-get-other-window)))
    (set-window-buffer window buf)
    (set-buffer cur-buf)))

(defun ein:markdown-open ()
  "Open file for the current buffer with `markdown-open-command'."
  (interactive)
  (unless ein:markdown-open-command
    (user-error "Variable `markdown-open-command' must be set"))
  (if (stringp ein:markdown-open-command)
      (if (not buffer-file-name)
          (user-error "Must be visiting a file")
        (save-buffer)
        (let ((exit-code (call-process ein:markdown-open-command nil nil nil
                                       buffer-file-name)))
          ;; The exit code can be a signal description string, so don’t use ‘=’
          ;; or ‘zerop’.
          (unless (eq exit-code 0)
            (user-error "%s failed with exit code %s"
                        ein:markdown-open-command exit-code))))
    (funcall ein:markdown-open-command))
  nil)

(defun ein:markdown-kill-ring-save ()
  "Run ein:markdown on file and store output in the kill ring."
  (interactive)
  (save-window-excursion
    (ein:markdown)
    (with-current-buffer ein:markdown-output-buffer-name
      (kill-ring-save (point-min) (point-max)))))


;;; Links =====================================================================

(defun ein:markdown-link-p ()
  "Return non-nil when `point' is at a non-wiki link.
See `markdown-wiki-link-p' for more information."
  (let ((case-fold-search nil))
    (and (not (ein:markdown-code-block-at-point-p))
         (or (thing-at-point-looking-at ein:markdown-regex-link-inline)
             (thing-at-point-looking-at ein:markdown-regex-link-reference)
             (thing-at-point-looking-at ein:markdown-regex-uri)
             (thing-at-point-looking-at ein:markdown-regex-angle-uri)))))

(make-obsolete 'ein:markdown-link-link 'ein:markdown-link-url "v2.3")

(defun ein:markdown-link-at-pos (pos)
  "Return properties of link or image at position POS.
Value is a list of elements describing the link:
 0. beginning position
 1. end position
 2. link text
 3. URL
 4. reference label
 5. title text
 6. bang (nil or \"!\")"
  (save-excursion
    (goto-char pos)
    (let (begin end text url reference title bang)
      (cond
       ;; Inline or reference image or link at point.
       ((or (thing-at-point-looking-at ein:markdown-regex-link-inline)
            (thing-at-point-looking-at ein:markdown-regex-link-reference))
        (setq bang (match-string-no-properties 1)
              begin (match-beginning 0)
              end (match-end 0)
              text (match-string-no-properties 3))
        (if (char-equal (char-after (match-beginning 5)) ?\[)
            ;; Reference link
            (setq reference (match-string-no-properties 6))
          ;; Inline link
          (setq url (match-string-no-properties 6))
          (when (match-end 7)
            (setq title (substring (match-string-no-properties 7) 1 -1)))))
       ;; Angle bracket URI at point.
       ((thing-at-point-looking-at ein:markdown-regex-angle-uri)
        (setq begin (match-beginning 0)
              end (match-end 0)
              url (match-string-no-properties 2)))
       ;; Plain URI at point.
       ((thing-at-point-looking-at ein:markdown-regex-uri)
        (setq begin (match-beginning 0)
              end (match-end 0)
              url (match-string-no-properties 1))))
      (list begin end text url reference title bang))))

(defun ein:markdown-link-url ()
  "Return the URL part of the regular (non-wiki) link at point.
Works with both inline and reference style links, and with images.
If point is not at a link or the link reference is not defined
returns nil."
  (let* ((values (ein:markdown-link-at-pos (point)))
         (text (nth 2 values))
         (url (nth 3 values))
         (ref (nth 4 values)))
    (or url (and ref (car (ein:markdown-reference-definition
                           (downcase (if (string= ref "") text ref))))))))

(defun ein:markdown-follow-link-at-point ()
  "Open the current non-wiki link.
If the link is a complete URL, open in browser with `browse-url'.
Otherwise, open with `find-file' after stripping anchor and/or query string.
Translate filenames using `markdown-filename-translate-function'."
  (interactive)
  (if (ein:markdown-link-p)
      (let* ((url (ein:markdown-link-url))
             (struct (url-generic-parse-url url))
             (full (url-fullness struct))
             (file url))
        ;; Parse URL, determine fullness, strip query string
        (if (fboundp 'url-path-and-query)
            (setq file (car (url-path-and-query struct)))
          (when (and (setq file (url-filename struct))
                     (string-match "\\?" file))
            (setq file (substring file 0 (match-beginning 0)))))
        ;; Open full URLs in browser, files in Emacs
        (if full
            (browse-url url)
          (when (and file (> (length file) 0))
            (find-file (funcall ein:markdown-translate-filename-function file)))))
    (user-error "Point is not at a ein:markdown link or URL")))

(defun ein:markdown-fontify-inline-links (last)
  "Add text properties to next inline link from point to LAST."
  (when (ein:markdown-match-generic-links last nil)
    (let* ((link-start (match-beginning 3))
           (link-end (match-end 3))
           (url-start (match-beginning 6))
           (url-end (match-end 6))
           (url (match-string-no-properties 6))
           (title-start (match-beginning 7))
           (title-end (match-end 7))
           (title (match-string-no-properties 7))
           ;; Markup part
           (mp (list 'face 'ein:markdown-markup-face
                     'rear-nonsticky t
                     'font-lock-multiline t))
           ;; Link part (without face)
           (lp (list 'keymap ein:markdown-mode-mouse-map
                     'mouse-face 'ein:markdown-highlight-face
                     'font-lock-multiline t
                     'help-echo (if title (concat title "\n" url) url)))
           ;; URL part
           (up (list 'keymap ein:markdown-mode-mouse-map
                     'face 'ein:markdown-url-face
                     'mouse-face 'ein:markdown-highlight-face
                     'font-lock-multiline t))
           ;; Title part
           (tp (list 'face 'ein:markdown-link-title-face
                     'font-lock-multiline t)))
      (dolist (g '(1 2 4 5 8))
        (when (match-end g)
          (add-text-properties (match-beginning g) (match-end g) mp)))
      ;; Preserve existing faces applied to link part (e.g., inline code)
      (when link-start
        (add-text-properties link-start link-end lp)
        (add-face-text-property link-start link-end
                                'ein:markdown-link-face 'append))
      (when url-start (add-text-properties url-start url-end up))
      (when title-start (add-text-properties url-end title-end tp))
      t)))

(defun ein:markdown-fontify-reference-links (last)
  "Add text properties to next reference link from point to LAST."
  (when (ein:markdown-match-generic-links last t)
    (let* ((link-start (match-beginning 3))
           (link-end (match-end 3))
           (ref-start (match-beginning 6))
           (ref-end (match-end 6))
           ;; Markup part
           (mp (list 'face 'ein:markdown-markup-face
                     'rear-nonsticky t
                     'font-lock-multiline t))
           ;; Link part
           (lp (list 'keymap ein:markdown-mode-mouse-map
                     'face 'ein:markdown-link-face
                     'mouse-face 'ein:markdown-highlight-face
                     'font-lock-multiline t
                     'help-echo (lambda (_ __ pos)
                                  (save-match-data
                                    (save-excursion
                                      (goto-char pos)
                                      (or (ein:markdown-link-url)
                                          "Undefined reference"))))))
           ;; Reference part
           (rp (list 'face 'ein:markdown-reference-face
                     'font-lock-multiline t)))
      (dolist (g '(1 2 4 5 8))
        (when (match-end g)
          (add-text-properties (match-beginning g) (match-end g) mp)))
      (when link-start (add-text-properties link-start link-end lp))
      (when ref-start (add-text-properties ref-start ref-end rp))
      t)))

(defun ein:markdown-fontify-angle-uris (last)
  "Add text properties to angle URIs from point to LAST."
  (when (ein:markdown-match-angle-uris last)
    (let* ((url-start (match-beginning 2))
           (url-end (match-end 2))
           ;; Markup part
           (mp (list 'face 'ein:markdown-markup-face
                     'rear-nonsticky t
                     'font-lock-multiline t))
           ;; URI part
           (up (list 'keymap ein:markdown-mode-mouse-map
                     'face 'ein:markdown-plain-url-face
                     'mouse-face 'ein:markdown-highlight-face
                     'font-lock-multiline t)))
      (dolist (g '(1 3))
        (add-text-properties (match-beginning g) (match-end g) mp))
      (add-text-properties url-start url-end up)
      t)))

(defun ein:markdown-fontify-plain-uris (last)
  "Add text properties to plain URLs from point to LAST."
  (when (ein:markdown-match-plain-uris last)
    (let* ((start (match-beginning 0))
           (end (match-end 0))
           (props (list 'keymap ein:markdown-mode-mouse-map
                        'face 'ein:markdown-plain-url-face
                        'mouse-face 'ein:markdown-highlight-face
                        'rear-nonsticky t
                        'font-lock-multiline t)))
      (add-text-properties start end props)
      t)))

;;; Following & Doing =========================================================

(defun ein:markdown-follow-thing-at-point (_arg)
  "Follow thing at point if possible, such as a reference link or wiki link.
Opens inline and reference links in a browser.  Opens wiki links
to other files in the current window, or the another window if
ARG is non-nil.
See `markdown-follow-link-at-point' and
`markdown-follow-wiki-link-at-point'."
  (interactive "P")
  (cond ((ein:markdown-link-p)
         (ein:markdown-follow-link-at-point))
        (t
         (user-error "Nothing to follow at point"))))

(make-obsolete 'ein:markdown-jump 'ein:markdown-do "v2.3")

(defun ein:markdown-do ()
  "Do something sensible based on context at point.
Jumps between reference links and definitions; between footnote
markers and footnote text."
  (interactive)
  (cond
   ;; Footnote definition
   ((ein:markdown-footnote-text-positions)
    (ein:markdown-footnote-return))
   ;; Footnote marker
   ((ein:markdown-footnote-marker-positions)
    (ein:markdown-footnote-goto-text))
   ;; Reference link
   ((thing-at-point-looking-at ein:markdown-regex-link-reference)
    (ein:markdown-reference-goto-definition))
   ;; Reference definition
   ((thing-at-point-looking-at ein:markdown-regex-reference-definition)
    (ein:markdown-reference-goto-link (match-string-no-properties 2)))
   ;; Align table
   ((ein:markdown-table-at-point-p)
    (call-interactively #'ein:markdown-table-align))
   ;; Otherwise
   (t
    (error "ein:markdown-do: don't know what to do"))))


;;; Miscellaneous =============================================================

(defun ein:markdown-compress-whitespace-string (str)
  "Compress whitespace in STR and return result.
Leading and trailing whitespace is removed.  Sequences of multiple
spaces, tabs, and newlines are replaced with single spaces."
  (ein:markdown-replace-regexp-in-string "\\(^[ \t\n]+\\|[ \t\n]+$\\)" ""
                            (ein:markdown-replace-regexp-in-string "[ \t\n]+" " " str)))

(defun ein:markdown--substitute-command-keys (string)
  "Like `substitute-command-keys' but, but prefers control characters.
First pass STRING to `substitute-command-keys' and then
substitute `C-i` for `TAB` and `C-m` for `RET`."
  (replace-regexp-in-string
   "\\<TAB\\>" "C-i"
   (replace-regexp-in-string
    "\\<RET\\>" "C-m" (substitute-command-keys string) t) t))

(defun ein:markdown-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
This is an exact copy of `line-number-at-pos' for use in emacs21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun ein:markdown-inside-link-p ()
  "Return t if point is within a link."
  (save-match-data
    (thing-at-point-looking-at (ein:markdown-make-regex-link-generic))))

(defun ein:markdown-line-is-reference-definition-p ()
  "Return whether the current line is a (non-footnote) reference defition."
  (save-excursion
    (move-beginning-of-line 1)
    (and (looking-at-p ein:markdown-regex-reference-definition)
         (not (looking-at-p "[ \t]*\\[^")))))

(defun ein:markdown-adaptive-fill-function ()
  "Return prefix for filling paragraph or nil if not determined."
  (cond
   ;; List item inside blockquote
   ((looking-at "^[ \t]*>[ \t]*\\(\\(?:[0-9]+\\|#\\)\\.\\|[*+:-]\\)[ \t]+")
    (ein:markdown-replace-regexp-in-string
     "[0-9\\.*+-]" " " (match-string-no-properties 0)))
   ;; Blockquote
   ((looking-at ein:markdown-regex-blockquote)
    (buffer-substring-no-properties (match-beginning 0) (match-end 2)))
   ;; List items
   ((looking-at ein:markdown-regex-list)
    (match-string-no-properties 0))
   ;; Footnote definition
   ((looking-at-p ein:markdown-regex-footnote-definition)
    "    ") ; four spaces
   ;; No match
   (t nil)))

(defun ein:markdown-fill-paragraph (&optional justify)
  "Fill paragraph at or after point.
This function is like \\[fill-paragraph], but it skips ein:markdown
code blocks.  If the point is in a code block, or just before one,
do not fill.  Otherwise, call `fill-paragraph' as usual. If
JUSTIFY is non-nil, justify text as well.  Since this function
handles filling itself, it always returns t so that
`fill-paragraph' doesn't run."
  (interactive "P")
  (unless (or (ein:markdown-code-block-at-point-p)
              (save-excursion
                (back-to-indentation)
                (skip-syntax-forward "-")
                (ein:markdown-code-block-at-point-p)))
    (fill-paragraph justify))
  t)

(make-obsolete 'ein:markdown-fill-forward-paragraph-function
               'ein:markdown-fill-forward-paragraph "v2.3")

(defun ein:markdown-fill-forward-paragraph (&optional arg)
  "Function used by `fill-paragraph' to move over ARG paragraphs.
This is a `fill-forward-paragraph-function' for `ein:markdown-mode'.
It is called with a single argument specifying the number of
paragraphs to move.  Just like `forward-paragraph', it should
return the number of paragraphs left to move."
  (or arg (setq arg 1))
  (if (> arg 0)
      ;; With positive ARG, move across ARG non-code-block paragraphs,
      ;; one at a time.  When passing a code block, don't decrement ARG.
      (while (and (not (eobp))
                  (> arg 0)
                  (= (forward-paragraph 1) 0)
                  (or (ein:markdown-code-block-at-pos (point-at-bol 0))
                      (setq arg (1- arg)))))
    ;; Move backward by one paragraph with negative ARG (always -1).
    (let ((start (point)))
      (setq arg (forward-paragraph arg))
      (while (and (not (eobp))
                  (progn (move-to-left-margin) (not (eobp)))
                  (looking-at-p paragraph-separate))
        (forward-line 1))
      (cond
       ;; Move point past whitespace following list marker.
       ((looking-at ein:markdown-regex-list)
        (goto-char (match-end 0)))
       ;; Move point past whitespace following pipe at beginning of line
       ;; to handle Pandoc line blocks.
       ((looking-at "^|\\s-*")
        (goto-char (match-end 0)))
       ;; Return point if the paragraph passed was a code block.
       ((ein:markdown-code-block-at-pos (point-at-bol 2))
        (goto-char start)))))
  arg)

(defun ein:markdown--inhibit-electric-quote ()
  "Function added to `electric-quote-inhibit-functions'.
Return non-nil if the quote has been inserted inside a code block
or span."
  (let ((pos (1- (point))))
    (or (ein:markdown-inline-code-at-pos pos)
        (ein:markdown-code-block-at-pos pos))))


;;; Extension Framework =======================================================

(defun ein:markdown-reload-extensions ()
  "Check settings, update font-lock keywords and hooks, and re-fontify buffer."
  (interactive)
  (when (member major-mode '(ein:markdown-mode))
    ;; Refontify buffer
    (if (eval-when-compile (fboundp 'font-lock-flush))
        ;; Use font-lock-flush in Emacs >= 25.1
        (font-lock-flush)
      ;; Backwards compatibility for Emacs 24.3-24.5
      (when (and font-lock-mode (fboundp 'font-lock-refresh-defaults))
        (font-lock-refresh-defaults)))
    ))

(defun ein:markdown-handle-local-variables ()
  "Run in `hack-local-variables-hook' to update font lock rules.
Checks to see if there is actually a ‘ein:markdown-mode’ file local variable
before regenerating font-lock rules for extensions."
  (when (and (boundp 'file-local-variables-alist)
             (assoc 'ein:markdown-enable-math file-local-variables-alist))
    (when (assoc 'ein:markdown-enable-math file-local-variables-alist)
      (ein:markdown-toggle-math ein:markdown-enable-math))
    (ein:markdown-reload-extensions)))


;;; Math Support ==============================================================

(make-obsolete 'ein:markdown-enable-math 'ein:markdown-toggle-math "v2.1")

(defconst ein:markdown-mode-font-lock-keywords-math
  (list
   ;; Equation reference (eq:foo)
   '("\\((eq:\\)\\([[:alnum:]:_]+\\)\\()\\)" . ((1 ein:markdown-markup-face)
                                                (2 ein:markdown-reference-face)
                                                (3 ein:markdown-markup-face)))
   ;; Equation reference \eqref{foo}
   '("\\(\\\\eqref{\\)\\([[:alnum:]:_]+\\)\\(}\\)" . ((1 ein:markdown-markup-face)
                                                      (2 ein:markdown-reference-face)
                                                      (3 ein:markdown-markup-face))))
  "Font lock keywords to add and remove when toggling math support.")

(defun ein:markdown-toggle-math (&optional arg)
  "Toggle support for inline and display LaTeX math expressions.
With a prefix argument ARG, enable math mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq ein:markdown-enable-math
        (if (eq arg 'toggle)
            (not ein:markdown-enable-math)
          (> (prefix-numeric-value arg) 0)))
  (if ein:markdown-enable-math
      (progn
        (font-lock-add-keywords
         'ein:markdown-mode ein:markdown-mode-font-lock-keywords-math)
        (message "ein:markdown-mode math support enabled"))
    (font-lock-remove-keywords
     'ein:markdown-mode ein:markdown-mode-font-lock-keywords-math)
    (message "ein:markdown-mode math support disabled"))
  (ein:markdown-reload-extensions))

;;; Display inline image ======================================================

(defcustom ein:markdown-fontify-code-block-default-mode nil
  "Default mode to use to fontify code blocks.
This mode is used when automatic detection fails, such as for GFM
code blocks with no language specified."
  :group 'ein:markdown
  :type '(choice function (const :tag "None" nil))
  :package-version '(ein:markdown-mode . "2.4"))

;; This is based on `org-src-lang-modes' from org-src.el
(defcustom ein:markdown-code-lang-modes
  '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode) ("ditaa" . artist-mode)
    ("asymptote" . asy-mode) ("dot" . fundamental-mode) ("sqlite" . sql-mode)
    ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
    ("C++" . c++-mode) ("screen" . shell-script-mode) ("shell" . sh-mode)
    ("bash" . sh-mode))
  "Alist mapping languages to their major mode.
The key is the language name, the value is the major mode.  For
many languages this is simple, but for language where this is not
the case, this variable provides a way to simplify things on the
user side.  For example, there is no ocaml-mode in Emacs, but the
mode to use is `tuareg-mode'."
  :group 'ein:markdown
  :type '(repeat
          (cons
           (string "Language name")
           (symbol "Major mode")))
  :package-version '(ein:markdown-mode . "2.3"))

(defun ein:markdown-get-lang-mode (lang)
  "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
  (cl-find-if
   'fboundp
   (list (cdr (assoc lang ein:markdown-code-lang-modes))
         (cdr (assoc (downcase lang) ein:markdown-code-lang-modes))
         (intern (concat lang "-mode"))
         (intern (concat (downcase lang) "-mode")))))

(defun ein:markdown-fontify-code-blocks-generic (matcher last)
  "Add text properties to next code block from point to LAST.
Use matching function MATCHER."
  (when (funcall matcher last)
    (save-excursion
      (save-match-data
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               ;; Find positions outside opening and closing backquotes.
               (bol-prev (progn (goto-char start)
                                (if (bolp) (point-at-bol 0) (point-at-bol))))
               (eol-next (progn (goto-char end)
                                (if (bolp) (point-at-bol 2) (point-at-bol 3)))))
          (add-text-properties start end '(face ein:markdown-pre-face))
          ;; Set background for block as well as opening and closing lines.
          (font-lock-append-text-property
           bol-prev eol-next 'face 'ein:markdown-code-face))))
    t))

(defun ein:markdown-fontify-fenced-code-blocks (last)
  "Add text properties to next tilde fenced code block from point to LAST."
  (ein:markdown-fontify-code-blocks-generic 'ein:markdown-match-fenced-code-blocks last))

;;; Table Editing =============================================================

;; These functions were originally adapted from `org-table.el'.

;; General helper functions

(defmacro ein:markdown--with-gensyms (symbols &rest body)
  (declare (debug (sexp body)) (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (make-symbol (concat "--" (symbol-name ',s)))))
                 symbols)
     ,@body))

(defun ein:markdown--split-string (string &optional separators)
  "Splits STRING into substrings at SEPARATORS.
SEPARATORS is a regular expression. If nil it defaults to
`split-string-default-separators'. This version returns no empty
strings if there are matches at the beginning and end of string."
  (let ((start 0) notfirst list)
    (while (and (string-match
                 (or separators split-string-default-separators)
                 string
                 (if (and notfirst
                          (= start (match-beginning 0))
                          (< start (length string)))
                     (1+ start) start))
                (< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
          (and (eq (match-beginning 0) (match-end 0))
               (eq (match-beginning 0) start))
          (push (substring string start (match-beginning 0)) list))
      (setq start (match-end 0)))
    (or (eq start (length string))
        (push (substring string start) list))
    (nreverse list)))

(defun ein:markdown--string-width (s)
  "Return width of string S.
This version ignores characters with invisibility property
`markdown-markup'."
  (let (b)
    (when (or (eq t buffer-invisibility-spec)
              (member 'ein:markdown-markup buffer-invisibility-spec))
      (while (setq b (text-property-any
                      0 (length s)
                      'invisible 'ein:markdown-markup s))
        (setq s (concat
                 (substring s 0 b)
                 (substring s (or (next-single-property-change
                                   b 'invisible s)
                                  (length s))))))))
  (string-width s))

(defun ein:markdown--remove-invisible-markup (s)
  "Remove ein:markdown markup from string S.
This version removes characters with invisibility property
`markdown-markup'."
  (let (b)
    (while (setq b (text-property-any
                    0 (length s)
                    'invisible 'ein:markdown-markup s))
      (setq s (concat
               (substring s 0 b)
               (substring s (or (next-single-property-change
                                 b 'invisible s)
                                (length s)))))))
  s)

;; Functions for maintaining tables

(defvar ein:markdown-table-at-point-p-function nil
  "Function to decide if point is inside a table.

The indirection serves to differentiate between standard ein:markdown
tables and gfm tables which are less strict about the markup.")

(defconst ein:markdown-table-line-regexp "^[ \t]*|"
  "Regexp matching any line inside a table.")

(defconst ein:markdown-table-hline-regexp "^[ \t]*|[-:]"
  "Regexp matching hline inside a table.")

(defconst ein:markdown-table-dline-regexp "^[ \t]*|[^-:]"
  "Regexp matching dline inside a table.")

(defun ein:markdown-table-at-point-p ()
  "Return non-nil when point is inside a table."
  (if (functionp ein:markdown-table-at-point-p-function)
      (funcall ein:markdown-table-at-point-p-function)
    (ein:markdown--table-at-point-p)))

(defun ein:markdown--table-at-point-p ()
  "Return non-nil when point is inside a table."
  (save-excursion
    (beginning-of-line)
    (and (looking-at-p ein:markdown-table-line-regexp)
         (not (ein:markdown-code-block-at-point-p)))))

(defun ein:markdown-table-hline-at-point-p ()
  "Return non-nil when point is on a hline in a table.
This function assumes point is on a table."
  (save-excursion
    (beginning-of-line)
    (looking-at-p ein:markdown-table-hline-regexp)))

(defun ein:markdown-table-begin ()
  "Find the beginning of the table and return its position.
This function assumes point is on a table."
  (save-excursion
    (while (and (not (bobp))
                (ein:markdown-table-at-point-p))
      (forward-line -1))
    (unless (or (eobp)
                (ein:markdown-table-at-point-p))
      (forward-line 1))
    (point)))

(defun ein:markdown-table-end ()
  "Find the end of the table and return its position.
This function assumes point is on a table."
  (save-excursion
    (while (and (not (eobp))
                (ein:markdown-table-at-point-p))
      (forward-line 1))
    (point)))

(defun ein:markdown-table-get-dline ()
  "Return index of the table data line at point.
This function assumes point is on a table."
  (let ((pos (point)) (end (ein:markdown-table-end)) (cnt 0))
    (save-excursion
      (goto-char (ein:markdown-table-begin))
      (while (and (re-search-forward
                   ein:markdown-table-dline-regexp end t)
                  (setq cnt (1+ cnt))
                  (< (point-at-eol) pos))))
    cnt))

(defun ein:markdown-table-get-column ()
  "Return table column at point.
This function assumes point is on a table."
  (let ((pos (point)) (cnt 0))
    (save-excursion
      (beginning-of-line)
      (while (search-forward "|" pos t) (setq cnt (1+ cnt))))
    cnt))

(defun ein:markdown-table-get-cell (&optional n)
  "Return the content of the cell in column N of current row.
N defaults to column at point. This function assumes point is on
a table."
  (and n (ein:markdown-table-goto-column n))
  (skip-chars-backward "^|\n") (backward-char 1)
  (if (looking-at "|[^|\r\n]*")
      (let* ((pos (match-beginning 0))
             (val (buffer-substring (1+ pos) (match-end 0))))
        (goto-char (min (point-at-eol) (+ 2 pos)))
        ;; Trim whitespaces
        (setq val (replace-regexp-in-string "\\`[ \t]+" "" val)
              val (replace-regexp-in-string "[ \t]+\\'" "" val)))
    (forward-char 1) ""))

(defun ein:markdown-table-goto-dline (n)
  "Go to the Nth data line in the table at point.
Return t when the line exists, nil otherwise. This function
assumes point is on a table."
  (goto-char (ein:markdown-table-begin))
  (let ((end (ein:markdown-table-end)) (cnt 0))
    (while (and (re-search-forward
                 ein:markdown-table-dline-regexp end t)
                (< (setq cnt (1+ cnt)) n)))
    (= cnt n)))

(defun ein:markdown-table-goto-column (n &optional on-delim)
  "Go to the Nth column in the table line at point.
With optional argument ON-DELIM, stop with point before the left
delimiter of the cell. If there are less than N cells, just go
beyond the last delimiter. This function assumes point is on a
table."
  (beginning-of-line 1)
  (when (> n 0)
    (while (and (> (setq n (1- n)) -1)
                (search-forward "|" (point-at-eol) t)))
    (if on-delim
        (backward-char 1)
      (when (looking-at " ") (forward-char 1)))))

(defmacro ein:markdown-table-save-cell (&rest body)
  "Save cell at point, execute BODY and restore cell.
This function assumes point is on a table."
  (declare (debug (body)))
  (ein:markdown--with-gensyms (line column)
    `(let ((,line (copy-marker (line-beginning-position)))
           (,column (ein:markdown-table-get-column)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,line)
         (ein:markdown-table-goto-column ,column)
         (set-marker ,line nil)))))

(defun ein:markdown-table-blank-line (s)
  "Convert a table line S into a line with blank cells."
  (if (string-match "^[ \t]*|-" s)
      (setq s (mapconcat
               (lambda (x) (if (member x '(?| ?+)) "|" " "))
               s ""))
    (while (string-match "|\\([ \t]*?[^ \t\r\n|][^\r\n|]*\\)|" s)
      (setq s (replace-match
               (concat "|" (make-string (length (match-string 1 s)) ?\ ) "|")
               t t s)))
    s))

(defun ein:markdown-table-colfmt (fmtspec)
  "Process column alignment specifier FMTSPEC for tables."
  (when (stringp fmtspec)
    (mapcar (lambda (x)
              (cond ((string-match-p "^:.*:$" x) 'c)
                    ((string-match-p "^:"     x) 'l)
                    ((string-match-p ":$"     x) 'r)
                    (t 'd)))
            (ein:markdown--split-string fmtspec "\\s-*|\\s-*"))))

(defun ein:markdown-table-align ()
  "Align table at point.
This function assumes point is on a table."
  (interactive)
  (let ((begin (ein:markdown-table-begin))
        (end (copy-marker (ein:markdown-table-end))))
    (ein:markdown-table-save-cell
     (goto-char begin)
     (let* (fmtspec
            ;; Store table indent
            (indent (progn (looking-at "[ \t]*") (match-string 0)))
            ;; Split table in lines and save column format specifier
            (lines (mapcar (lambda (l)
                             (if (string-match-p "\\`[ \t]*|[-:]" l)
                                 (progn (setq fmtspec (or fmtspec l)) nil) l))
                           (ein:markdown--split-string (buffer-substring begin end) "\n")))
            ;; Split lines in cells
            (cells (mapcar (lambda (l) (ein:markdown--split-string l "\\s-*|\\s-*"))
                           (remq nil lines)))
            ;; Calculate maximum number of cells in a line
            (maxcells (if cells
                          (apply #'max (mapcar #'length cells))
                        (user-error "Empty table")))
            ;; Empty cells to fill short lines
            (emptycells (make-list maxcells "")) maxwidths)
       ;; Calculate maximum width for each column
       (dotimes (i maxcells)
         (let ((column (mapcar (lambda (x) (or (nth i x) "")) cells)))
           (push (apply #'max 1 (mapcar #'ein:markdown--string-width column))
                 maxwidths)))
       (setq maxwidths (nreverse maxwidths))
       ;; Process column format specifier
       (setq fmtspec (ein:markdown-table-colfmt fmtspec))
       ;; Compute formats needed for output of table lines
       (let ((hfmt (concat indent "|"))
             (rfmt (concat indent "|"))
             hfmt1 rfmt1 fmt)
         (dolist (width maxwidths (setq hfmt (concat (substring hfmt 0 -1) "|")))
           (setq fmt (pop fmtspec))
           (cond ((equal fmt 'l) (setq hfmt1 ":%s-|" rfmt1 " %%-%ds |"))
                 ((equal fmt 'r) (setq hfmt1 "-%s:|" rfmt1  " %%%ds |"))
                 ((equal fmt 'c) (setq hfmt1 ":%s:|" rfmt1 " %%-%ds |"))
                 (t              (setq hfmt1 "-%s-|" rfmt1 " %%-%ds |")))
           (setq rfmt (concat rfmt (format rfmt1 width)))
           (setq hfmt (concat hfmt (format hfmt1 (make-string width ?-)))))
         ;; Replace modified lines only
         (dolist (line lines)
           (let ((line (if line
                           (apply #'format rfmt (append (pop cells) emptycells))
                         hfmt))
                 (previous (buffer-substring (point) (line-end-position))))
             (if (equal previous line)
                 (forward-line)
               (insert line "\n")
               (delete-region (point) (line-beginning-position 2))))))
       (set-marker end nil)))))

(defun ein:markdown-table-insert-row (&optional arg)
  "Insert a new row above the row at point into the table.
With optional argument ARG, insert below the current row."
  (interactive "P")
  (unless (ein:markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((line (buffer-substring
                (line-beginning-position) (line-end-position)))
         (new (ein:markdown-table-blank-line line)))
    (beginning-of-line (if arg 2 1))
    (unless (bolp) (insert "\n"))
    (insert-before-markers new "\n")
    (beginning-of-line 0)
    (re-search-forward "| ?" (line-end-position) t)))

(defun ein:markdown-table-delete-row ()
  "Delete row or horizontal line at point from the table."
  (interactive)
  (unless (ein:markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let ((col (current-column)))
    (kill-region (point-at-bol)
                 (min (1+ (point-at-eol)) (point-max)))
    (unless (ein:markdown-table-at-point-p) (beginning-of-line 0))
    (move-to-column col)))

(defun ein:markdown-table-move-row (&optional up)
  "Move table line at point down.
With optional argument UP, move it up."
  (interactive "P")
  (unless (ein:markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((col (current-column)) (pos (point))
         (tonew (if up 0 2)) txt)
    (beginning-of-line tonew)
    (unless (ein:markdown-table-at-point-p)
      (goto-char pos) (user-error "Cannot move row further"))
    (goto-char pos) (beginning-of-line 1) (setq pos (point))
    (setq txt (buffer-substring (point) (1+ (point-at-eol))))
    (delete-region (point) (1+ (point-at-eol)))
    (beginning-of-line tonew)
    (insert txt) (beginning-of-line 0)
    (move-to-column col)))

(defun ein:markdown-table-move-row-up ()
  "Move table row at point up."
  (interactive)
  (ein:markdown-table-move-row 'up))

(defun ein:markdown-table-move-row-down ()
  "Move table row at point down."
  (interactive)
  (ein:markdown-table-move-row nil))

(defun ein:markdown-table-insert-column ()
  "Insert a new table column."
  (interactive)
  (unless (ein:markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((col (max 1 (ein:markdown-table-get-column)))
         (begin (ein:markdown-table-begin))
         (end (copy-marker (ein:markdown-table-end))))
    (ein:markdown-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (ein:markdown-table-goto-column col t)
       (if (ein:markdown-table-hline-at-point-p)
           (insert "|---")
         (insert "|   "))
       (forward-line)))
    (set-marker end nil)
    (ein:markdown-table-align)))

(defun ein:markdown-table-delete-column ()
  "Delete column at point from table."
  (interactive)
  (unless (ein:markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let ((col (ein:markdown-table-get-column))
        (begin (ein:markdown-table-begin))
        (end (copy-marker (ein:markdown-table-end))))
    (ein:markdown-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (ein:markdown-table-goto-column col t)
       (and (looking-at "|[^|\n]+|")
            (replace-match "|"))
       (forward-line)))
    (set-marker end nil)
    (ein:markdown-table-goto-column (max 1 (1- col)))
    (ein:markdown-table-align)))

(defun ein:markdown-table-move-column (&optional left)
  "Move table column at point to the right.
With optional argument LEFT, move it to the left."
  (interactive "P")
  (unless (ein:markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((col (ein:markdown-table-get-column))
         (col1 (if left (1- col) col))
         (colpos (if left (1- col) (1+ col)))
         (begin (ein:markdown-table-begin))
         (end (copy-marker (ein:markdown-table-end))))
    (when (and left (= col 1))
      (user-error "Cannot move column further left"))
    (when (and (not left) (looking-at "[^|\n]*|[^|\n]*$"))
      (user-error "Cannot move column further right"))
    (ein:markdown-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (ein:markdown-table-goto-column col1 t)
       (when (looking-at "|\\([^|\n]+\\)|\\([^|\n]+\\)|")
         (replace-match "|\\2|\\1|"))
       (forward-line)))
    (set-marker end nil)
    (ein:markdown-table-goto-column colpos)
    (ein:markdown-table-align)))

(defun ein:markdown-table-move-column-left ()
  "Move table column at point to the left."
  (interactive)
  (ein:markdown-table-move-column 'left))

(defun ein:markdown-table-move-column-right ()
  "Move table column at point to the right."
  (interactive)
  (ein:markdown-table-move-column nil))

(defun ein:markdown-table-next-row ()
  "Go to the next row (same column) in the table.
Create new table lines if required."
  (interactive)
  (unless (ein:markdown-table-at-point-p)
    (user-error "Not at a table"))
  (if (or (looking-at "[ \t]*$")
          (save-excursion (skip-chars-backward " \t") (bolp)))
      (newline)
    (ein:markdown-table-align)
    (let ((col (ein:markdown-table-get-column)))
      (beginning-of-line 2)
      (if (or (not (ein:markdown-table-at-point-p))
              (ein:markdown-table-hline-at-point-p))
          (progn
            (beginning-of-line 0)
            (ein:markdown-table-insert-row 'below)))
      (ein:markdown-table-goto-column col)
      (skip-chars-backward "^|\n\r")
      (when (looking-at " ") (forward-char 1)))))

(defun ein:markdown-table-forward-cell ()
  "Go to the next cell in the table.
Create new table lines if required."
  (interactive)
  (unless (ein:markdown-table-at-point-p)
    (user-error "Not at a table"))
  (ein:markdown-table-align)
  (let ((end (ein:markdown-table-end)))
    (when (ein:markdown-table-hline-at-point-p) (end-of-line 1))
    (condition-case nil
        (progn
          (re-search-forward "|" end)
          (if (looking-at "[ \t]*$")
              (re-search-forward "|" end))
          (if (and (looking-at "[-:]")
                   (re-search-forward "^[ \t]*|\\([^-:]\\)" end t))
              (goto-char (match-beginning 1)))
          (if (looking-at "[-:]")
              (progn
                (beginning-of-line 0)
                (ein:markdown-table-insert-row 'below))
            (when (looking-at " ") (forward-char 1))))
      (error (ein:markdown-table-insert-row 'below)))))

(defun ein:markdown-table-backward-cell ()
  "Go to the previous cell in the table."
  (interactive)
  (unless (ein:markdown-table-at-point-p)
    (user-error "Not at a table"))
  (ein:markdown-table-align)
  (when (ein:markdown-table-hline-at-point-p) (end-of-line 1))
  (condition-case nil
      (progn
        (re-search-backward "|" (ein:markdown-table-begin))
        (re-search-backward "|" (ein:markdown-table-begin)))
    (error (user-error "Cannot move to previous table cell")))
  (while (looking-at "|\\([-:]\\|[ \t]*$\\)")
    (re-search-backward "|" (ein:markdown-table-begin)))
  (when (looking-at "| ?") (goto-char (match-end 0))))

(defun ein:markdown-table-transpose ()
  "Transpose table at point.
Horizontal separator lines will be eliminated."
  (interactive)
  (unless (ein:markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((table (buffer-substring-no-properties
                 (ein:markdown-table-begin) (ein:markdown-table-end)))
         ;; Convert table to a Lisp structure
         (table (delq nil
                      (mapcar
                       (lambda (x)
                         (unless (string-match-p
                                  ein:markdown-table-hline-regexp x)
                           (ein:markdown--split-string x "\\s-*|\\s-*")))
                       (ein:markdown--split-string table "[ \t]*\n[ \t]*"))))
         (dline_old (ein:markdown-table-get-dline))
         (col_old (ein:markdown-table-get-column))
         (contents (mapcar (lambda (_)
                             (let ((tp table))
                               (mapcar
                                (lambda (_)
                                  (prog1
                                      (pop (car tp))
                                    (setq tp (cdr tp))))
                                table)))
                           (car table))))
    (goto-char (ein:markdown-table-begin))
    (re-search-forward "|") (backward-char)
    (delete-region (point) (ein:markdown-table-end))
    (insert (mapconcat
             (lambda(x)
               (concat "| " (mapconcat 'identity x " | " ) "  |\n"))
             contents ""))
    (ein:markdown-table-goto-dline col_old)
    (ein:markdown-table-goto-column dline_old))
  (ein:markdown-table-align))

(defun ein:markdown-table-sort-lines (&optional sorting-type)
  "Sort table lines according to the column at point.

The position of point indicates the column to be used for
sorting, and the range of lines is the range between the nearest
horizontal separator lines, or the entire table of no such lines
exist. If point is before the first column, user will be prompted
for the sorting column. If there is an active region, the mark
specifies the first line and the sorting column, while point
should be in the last line to be included into the sorting.

The command then prompts for the sorting type which can be
alphabetically or numerically. Sorting in reverse order is also
possible.

If SORTING-TYPE is specified when this function is called from a
Lisp program, no prompting will take place. SORTING-TYPE must be
a character, any of (?a ?A ?n ?N) where the capital letters
indicate that sorting should be done in reverse order."
  (interactive)
  (unless (ein:markdown-table-at-point-p)
    (user-error "Not at a table"))
  ;; Set sorting type and column used for sorting
  (let ((column (let ((c (ein:markdown-table-get-column)))
                  (cond ((> c 0) c)
                        ((called-interactively-p 'any)
                         (read-number "Use column N for sorting: "))
                        (t 1))))
        (sorting-type
         (or sorting-type
             (read-char-exclusive
              "Sort type: [a]lpha [n]umeric (A/N means reversed): "))))
    (save-restriction
      ;; Narrow buffer to appropriate sorting area
      (if (region-active-p)
          (narrow-to-region
           (save-excursion
             (progn
               (goto-char (region-beginning)) (line-beginning-position)))
           (save-excursion
             (progn
               (goto-char (region-end)) (line-end-position))))
        (let ((start (ein:markdown-table-begin))
              (end (ein:markdown-table-end)))
          (narrow-to-region
           (save-excursion
             (if (re-search-backward
                  ein:markdown-table-hline-regexp start t)
                 (line-beginning-position 2)
               start))
           (if (save-excursion (re-search-forward
                                ein:markdown-table-hline-regexp end t))
               (match-beginning 0)
             end))))
      ;; Determine arguments for `sort-subr'
      (let* ((extract-key-from-cell
              (cl-case sorting-type
                ((?a ?A) #'ein:markdown--remove-invisible-markup) ;; #'identity)
                ((?n ?N) #'string-to-number)
                (t (user-error "Invalid sorting type: %c" sorting-type))))
             (predicate
              (cl-case sorting-type
                ((?n ?N) #'<)
                ((?a ?A) #'string<))))
        ;; Sort selected area
        (goto-char (point-min))
        (sort-subr (memq sorting-type '(?A ?N))
                   (lambda ()
                     (forward-line)
                     (while (and (not (eobp))
                                 (not (looking-at
                                       ein:markdown-table-dline-regexp)))
                       (forward-line)))
                   #'end-of-line
                   (lambda ()
                     (funcall extract-key-from-cell
                              (ein:markdown-table-get-cell column)))
                   nil
                   predicate)
        (goto-char (point-min))))))

(defun ein:markdown-table-convert-region (begin end &optional separator)
  "Convert region from BEGIN to END to table with SEPARATOR.

If every line contains at least one TAB character, the function
assumes that the material is tab separated (TSV). If every line
contains a comma, comma-separated values (CSV) are assumed. If
not, lines are split at whitespace into cells.

You can use a prefix argument to force a specific separator:
\\[universal-argument] once forces CSV, \\[universal-argument]
twice forces TAB, and \\[universal-argument] three times will
prompt for a regular expression to match the separator, and a
numeric argument N indicates that at least N consecutive
spaces, or alternatively a TAB should be used as the separator."

  (interactive "r\nP")
  (let* ((begin (min begin end)) (end (max begin end)) re)
    (goto-char begin) (beginning-of-line 1)
    (setq begin (point-marker))
    (goto-char end)
    (if (bolp) (backward-char 1) (end-of-line 1))
    (setq end (point-marker))
    (when (equal separator '(64))
      (setq separator (read-regexp "Regexp for cell separator: ")))
    (unless separator
      ;; Get the right cell separator
      (goto-char begin)
      (setq separator
            (cond
             ((not (re-search-forward "^[^\n\t]+$" end t)) '(16))
             ((not (re-search-forward "^[^\n,]+$" end t)) '(4))
             (t 1))))
    (goto-char begin)
    (if (equal separator '(4))
        ;; Parse CSV
        (while (< (point) end)
          (cond
           ((looking-at "^") (insert "| "))
           ((looking-at "[ \t]*$") (replace-match " |") (beginning-of-line 2))
           ((looking-at "[ \t]*\"\\([^\"\n]*\\)\"")
            (replace-match "\\1") (if (looking-at "\"") (insert "\"")))
           ((looking-at "[^,\n]+") (goto-char (match-end 0)))
           ((looking-at "[ \t]*,") (replace-match " | "))
           (t (beginning-of-line 2))))
      (setq re
            (cond
             ((equal separator '(4))  "^\\|\"?[ \t]*,[ \t]*\"?")
             ((equal separator '(16)) "^\\|\t")
             ((integerp separator)
              (if (< separator 1)
                  (user-error "Cell separator must contain one or more spaces")
                (format "^ *\\| *\t *\\| \\{%d,\\}" separator)))
             ((stringp separator) (format "^ *\\|%s" separator))
             (t (error "Invalid cell separator"))))
      (while (re-search-forward re end t) (replace-match "| " t t)))
    (goto-char begin)
    (ein:markdown-table-align)))

(defun ein:markdown-insert-table (&optional rows columns align)
  "Insert an empty pipe table.
Optional arguments ROWS, COLUMNS, and ALIGN specify number of
rows and columns and the column alignment."
  (interactive)
  (let* ((rows (or rows (string-to-number (read-string "Row size: "))))
         (columns (or columns (string-to-number (read-string "Column size: "))))
         (align (or align (read-string "Alignment ([l]eft, [r]ight, [c]enter, or RET for default): ")))
         (align (cond ((equal align "l") ":--")
                      ((equal align "r") "--:")
                      ((equal align "c") ":-:")
                      (t "---")))
         (pos (point))
         (indent (make-string (current-column) ?\ ))
         (line (concat
                (apply 'concat indent "|"
                       (make-list columns "   |")) "\n"))
         (hline (apply 'concat indent "|"
                       (make-list columns (concat align "|")))))
    (if (string-match
         "^[ \t]*$" (buffer-substring-no-properties
                     (point-at-bol) (point)))
        (beginning-of-line 1)
      (newline))
    (dotimes (_ rows) (insert line))
    (goto-char pos)
    (if (> rows 1)
        (progn
          (end-of-line 1) (insert (concat "\n" hline)) (goto-char pos)))
    (ein:markdown-table-forward-cell)))


;;; ElDoc Support =============================================================

(defun ein:markdown-eldoc-function ()
  "Return a helpful string when appropriate based on context.
* Report URL when point is at a hidden URL.
* Report language name when point is a code block with hidden markup."
  (cond
   ;; Hidden URL or reference for inline link
   ((or (thing-at-point-looking-at ein:markdown-regex-link-inline)
        (thing-at-point-looking-at ein:markdown-regex-link-reference))
    (let* ((imagep (string-equal (match-string 1) "!"))
           (edit-keys (ein:markdown--substitute-command-keys
                       (if imagep
                           "\\[ein:markdown-insert-image]"
                         "\\[ein:markdown-insert-link]")))
           (edit-str (propertize edit-keys 'face 'font-lock-constant-face))
           (referencep (string-equal (match-string 5) "["))
           (object (if referencep "reference" "URL")))
      (format "%s (%s to edit): %s" object edit-str
              (if referencep
                  (concat
                   (propertize "[" 'face 'ein:markdown-markup-face)
                   (propertize (match-string-no-properties 6)
                               'face 'ein:markdown-reference-face)
                   (propertize "]" 'face 'ein:markdown-markup-face))
                (propertize (match-string-no-properties 6)
                            'face 'ein:markdown-url-face)))))
   ;; Hidden language name for fenced code blocks
   ((and (ein:markdown-code-block-at-point-p)
         (not (get-text-property (point) 'ein:markdown-pre)))
    (let ((lang (save-excursion (ein:markdown-code-block-lang))))
      (unless lang (setq lang "[unspecified]"))
      (format "Code block language: %s"
              (propertize lang 'face 'ein:markdown-language-keyword-face))))))


;;; Mode Definition  ==========================================================

(defun ein:markdown-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "ein:markdown-mode, version %s" ein:markdown-mode-version))

(defun ein:markdown-mode-info ()
  "Open the `ein:markdown-mode' homepage."
  (interactive)
  (browse-url "https://jblevins.org/projects/ein:markdown-mode/"))

;;;###autoload
(define-derived-mode ein:markdown-mode text-mode "ein:markdown"
  "Major mode for editing ein:markdown files."
  ;; Natural ein:markdown tab width
  (setq tab-width 4)
  ;; Comments
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")
  (setq-local comment-start-skip "<!--[ \t]*")
  (setq-local comment-column 0)
  (setq-local comment-auto-fill-only-comments nil)
  (setq-local comment-use-syntax t)
  (setq-local syntax-propertize-function #'ein:markdown-syntax-propertize)
  (syntax-propertize (point-max)) ;; Propertize before hooks run, etc.
  ;; Font lock.
  (setq font-lock-defaults
        '(ein:markdown-mode-font-lock-keywords
          nil nil nil nil
          (font-lock-multiline . t)
          (font-lock-syntactic-face-function . ein:markdown-syntactic-face)
          (font-lock-extra-managed-props
           . (composition display invisible rear-nonsticky
                          keymap help-echo mouse-face))))
  ;; Math mode
  (when ein:markdown-enable-math (ein:markdown-toggle-math t))
  ;; Add a buffer-local hook to reload after file-local variables are read
  (add-hook 'hack-local-variables-hook #'ein:markdown-handle-local-variables nil t)
  ;; For imenu support
  (setq imenu-create-index-function
        (if ein:markdown-nested-imenu-heading-index
            #'ein:markdown-imenu-create-nested-index
          #'ein:markdown-imenu-create-flat-index))
  ;; For menu support in XEmacs
  (easy-menu-add ein:markdown-mode-menu ein:markdown-mode-map)
  ;; Defun movement
  (setq-local beginning-of-defun-function #'ein:markdown-beginning-of-defun)
  (setq-local end-of-defun-function #'ein:markdown-end-of-defun)
  ;; Paragraph filling
  (setq-local fill-paragraph-function #'ein:markdown-fill-paragraph)
  (setq-local paragraph-start
              ;; Should match start of lines that start or separate paragraphs
              (mapconcat #'identity
                         '(
                           "\f" ; starts with a literal line-feed
                           "[ \t\f]*$" ; space-only line
                           "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                           "[ \t]*[*+-][ \t]+" ; unordered list item
                           "[ \t]*\\(?:[0-9]+\\|#\\)\\.[ \t]+" ; ordered list item
                           "[ \t]*\\[\\S-*\\]:[ \t]+" ; link ref def
                           "[ \t]*:[ \t]+" ; definition
                           "^|" ; table or Pandoc line block
                           )
                         "\\|"))
  (setq-local paragraph-separate
              ;; Should match lines that separate paragraphs without being
              ;; part of any paragraph:
              (mapconcat #'identity
                         '("[ \t\f]*$" ; space-only line
                           "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                           ;; The following is not ideal, but the Fill customization
                           ;; options really only handle paragraph-starting prefixes,
                           ;; not paragraph-ending suffixes:
                           ".*  $" ; line ending in two spaces
                           "^#+"
                           "[ \t]*\\[\\^\\S-*\\]:[ \t]*$") ; just the start of a footnote def
                         "\\|"))
  (setq-local adaptive-fill-first-line-regexp "\\`[ \t]*[A-Z]?>[ \t]*?\\'")
  (setq-local adaptive-fill-regexp "\\s-*")
  (setq-local adaptive-fill-function #'ein:markdown-adaptive-fill-function)
  (setq-local fill-forward-paragraph-function #'ein:markdown-fill-forward-paragraph)
  ;; Outline mode
  (setq-local outline-regexp ein:markdown-regex-header)
  (setq-local outline-level #'ein:markdown-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  ;; ElDoc support
  (if (eval-when-compile (fboundp 'add-function))
      (add-function :before-until (local 'eldoc-documentation-function)
                    #'ein:markdown-eldoc-function)
    (setq-local eldoc-documentation-function #'ein:markdown-eldoc-function))
  ;; Inhibiting line-breaking:
  ;; Separating out each condition into a separate function so that users can
  ;; override if desired (with remove-hook)
  (add-hook 'fill-nobreak-predicate
            #'ein:markdown-line-is-reference-definition-p nil t)
  (add-hook 'fill-nobreak-predicate
            #'ein:markdown-pipe-at-bol-p nil t)

  ;; Indentation
  (setq-local indent-line-function ein:markdown-indent-function)

  ;; Flyspell
  (setq-local flyspell-generic-check-word-predicate
              #'ein:markdown-flyspell-check-word-p)

  ;; Electric quoting
  (add-hook 'electric-quote-inhibit-functions
            #'ein:markdown--inhibit-electric-quote nil :local)

  ;; Backwards compatibility with ein:markdown-css-path
  (when (boundp 'ein:markdown-css-path)
    (warn "ein:markdown-css-path is deprecated, see ein:markdown-css-paths.")
    (add-to-list 'ein:markdown-css-paths ein:markdown-css-path)))

(ein:markdown-update-header-faces)
(provide 'ein-markdown-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:
;;; ein:markdown-mode.el ends here
