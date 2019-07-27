import os
import shutil

from invoke import task

try:
    from pathlib import Path
except ImportError:
    from pathlib2 import Path


def find_file(start, name):
    for relpath, dirs, files in os.walk(start):
        if name in files:
            full_path = os.path.join(start, relpath, name)
            return Path(full_path)
    return None


def generate_cask_env(ctx, curl_path):
    current_path = os.environ['PATH']
    execpath = ctx.run("cask exec-path")
    loadpath = ctx.run("cask load-path")
    cask_env = {'PATH': '{};{}'.format(execpath.stdout, current_path),
                'EMACSLOADPATH': '{}'.format(loadpath.stdout)}
    if curl_path and Path(curl_path).exists():
        cask_env['PATH'] = "{};{}".format(curl_path, cask_env['PATH'])
    return cask_env


def prepare_ecukes_args(with_doc, with_file, verbose, quiet, help, debug,
                        tags, script, no_win, win, reporter, timeout,
                        patterns, anti_patterns, only_failing,
                        error_log):
    sarg = ""
    if with_doc:
        sarg += " --with-doc"
    if with_file:
        sarg += " --with-file"
    if verbose:
        sarg += " --verbose"
    if quiet:
        sarg += " --quiet"
    if help:
        sarg += " --help"
    if debug:
        sarg += " --debug"
    if tags:
        sarg += f" --tags {tags}"
    if script:
        sarg += " --script"
    if no_win:
        sarg += " --no-win"
    if win:
        sarg += " --win"
    if reporter:
        sarg += f" --reporter {reporter}"
    if timeout:
        sarg += f" --timeout {timeout}"
    if patterns:
        sarg += f" --patterns {patterns}"
    if anti_patterns:
        sarg += f" --anti-patterns {anti_patterns}"
    if only_failing:
        sarg += " --only-failing"
    if error_log:
        sarg += f" --error-log {error_log}"
    return sarg.strip()

@task
def ecukes(ctx, curl_path=None, with_doc=False, with_file=False, verbose=False,
           quiet=False, help=False, debug=False, script=False,
           no_win=False, win=False, reporter="magnars", timeout=None,
           patterns=None, anti_patterns=None, tags=None, only_failing=False,
           error_log=None):
    # import pdb
    # pdb.set_trace()
    cask_env = generate_cask_env(ctx, curl_path)
    cask_env["ECUKES_ARGS"] = prepare_ecukes_args(with_doc, with_file, verbose,
                                                  quiet, help, debug, tags,
                                                  script, no_win, win,
                                                  reporter, timeout, patterns,
                                                  anti_patterns, only_failing,
                                                  error_log)
    #f"--reporter {reporter} --tags ~@julia,~@ir,~@memory,~@switch"
    ecukes_bindir = find_file(os.getcwd(), "ecukes")
    ecukes_cli = ecukes_bindir.parent.parent.joinpath("ecukes-cli.el")
    if win:
        ctx.run("emacs --load {} -Q".format(ecukes_cli), env=cask_env)
    elif no_win:
        ctx.run("emacs -nw --load {} -Q".format(ecukes_cli), env=cask_env)
    else:
        ctx.run("emacs --script {} -Q".format(ecukes_cli), env=cask_env)


@task
def autoloads(ctx):
    ctx.run("""emacs -Q --batch --eval "(package-initialize)" --eval "(package-generate-autoloads \\"ein\\" \\"./lisp\\")" """)


@task
def clean_elc(ctx):
    ctx.run("""cask clean-elc""")


@task
def clean(c):
    clean_elc(c)
    cwd = Path('.')
    if Path("test/test-install").exists():
        Path("test/test-install").unlink()
    [f.unlink() for f in cwd.glob('log/*websocket*')]
    [f.unlink() for f in cwd.glob('features/Untitled*.ipynb')]
    [f.unlink() for f in cwd.glob('test/Untitled*.ipynb')]


@task
def dist_clean(c):
    clean(c)
    cwd = Path('.')
    if Path('dist/').exists():
        [f.unlink() for f in cwd.glob('dist/*')]
        Path('dist').rmdir()


@task
def test_compile(c):
    c.run("cask install")


@task
def test_jupyterhub(c):
    c.run("cask exec ecukes --tags @jupyterhub --reporter magnars")


@task
def test_poly(ctx, curl_path):
    updated_env = generate_cask_env(ctx, curl_path)
    ctx.run("cask exec ert-runner -L ./lisp -L ./test -l test/testfunc.el test/test-poly.el test/test-func.el", env=updated_env)
    shutil.copy('test/test-poly.el', 'features/support/test-poly.el')
    ecukes(ctx, curl_path=curl_path, reporter="magnars")
    Path('features/support/test-poly.el').unlink()


@task
def test_func(ctx, curl_path=None):
    updated_env = generate_cask_env(ctx, curl_path)
    ctx.run("cask exec ert-runner -L ./lisp -L ./test -l test/testfunc.el test/test-func.el",
            env=updated_env)

@task
def test_int(ctx, curl_path=None):
    updated_env = generate_cask_env(ctx, curl_path)
    ecukes(ctx, curl_path=curl_path, reporter="magnars")


@task
def test_unit(c):
    c.run("cask exec ert-runner -L ./lisp -L ./test -l test/testein-loader.el")


@task
def check_var(c):
    var = c.run("cask exec-path")
    print("The cask exec path is = {}".format(var.stdout))


@task
def windowed_testfunc(c):
    c.run("cask exec ert-runner --win -L ./lisp -L ./test -l test/testfunc.el test/test-func.el")
