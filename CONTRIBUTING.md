# Contribution guide line

Please use `M-x ein:dev-bug-report-template` to write a bug report.
It pops up a buffer containing some system information and instruction
for bug report.


## Avoid standard Emacs traps

### It is Emacs not all

Do you try the same thing in normal IPython notebook (i.e., browser)?
If not, please try.  There can be problem in your IPython installation.


### Your Emacs configuration

There can be some configuration EIN does not work well with.  To check
if the problem is in the configuration or not, the best way is to start
EIN in a clean Emacs (i.e., without your configuration).

You can use zeroein.el to start EIN in a clean Emacs.
See the Quick try section in the manual:
http://tkf.github.com/emacs-ipython-notebook/#quick-try


### Badly compiled file

Remove all `*.elc` files from EIN source directory and its
dependencies.  Then restart Emacs and try the procedure to reproduce
the problem again.

You will have problem with compiled files if they are older than the
source files and/or the files are compiled with different Emacs
versions.


### Make sure that the right library is loaded.

Sometimes you accidentally load libraries from unexpected location,
if you installed it in different places in the past or another
Emacs lisp libraries you are using bundles the old version of the
libraries.

To make sure that all EIN dependencies are loaded from the intended
place, use `M-x locate-library`.  Also, `M-x ein:dev-bug-report-template`
does it for you for all EIN dependencies.


## Log and backtrace

Please consider putting log and backtrace in your bug report.
Follow the instruction in the manual:
http://tkf.github.com/emacs-ipython-notebook/#reporting-issue
