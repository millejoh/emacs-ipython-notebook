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


@task
def ecukes(ctx, curl_path=None, reporter="magnars"):
    cask_env = generate_cask_env(ctx, curl_path)
    cask_env["ECUKES_ARGS"] = f"--reporter {reporter}"
    ecukes_bindir = find_file(os.getcwd(), "ecukes")
    ecukes_cli = ecukes_bindir.parent.parent.joinpath("ecukes-cli.el")
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
def test_poly(c):
    c.run("cask exec ert-runner -L ./lisp -L ./test -l test/testfunc.el test/test-poly.el test/test-func.el")
    shutil.copy('test/test-poly.el', 'features/support/test-poly.el')
    c.run("cask exec ecukes --report magnars")
    Path('features/support/test-poly.el').unlink()



@task
def test_int(ctx, curl_path=None):
    updated_env = generate_cask_env(ctx, curl_path)
    ctx.run("cask exec ert-runner -L ./lisp -L ./test -l test/testfunc.el test/test-func.el",
            env=updated_env)
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
