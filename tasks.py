import os
import shutil
from pathlib import Path

from invoke import task


@task
def autoloads(c):
    c.run("""emacs -Q --batch --eval "(package-initialize)" --eval "(package-generate-autoloads \\"ein\\" \\"./lisp\\")" """)


@task
def clean_elc(c):
    c.run("""cask clean-elc""")


@task
def clean(c):
    clean_elc(c)
    p = Path('.')
    if Path("test/test-install").exists():
        Path("test/test-install").unlink()
    [f.unlink() for f in p.glob('log/*websocket*')]
    [f.unlink() for f in p.glob('features/Untitled*.ipynb')]
    [f.unlink() for f in p.glob('test/Untitled*.ipynb')]


@task
def dist_clean(c):
    clean(c)
    p = Path('.')
    if Path('dist/').exists():
        [f.unlink() for f in p.glob('dist/*')]
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
def test_int(c, curl_path=None):
    current_path = os.environ['PATH']
    execpath = c.run("cask exec-path")
    loadpath = c.run("cask load-path")
    updated_env = {'PATH': '{};{};{}'.format(curl_path, execpath.stdout, current_path),
                   'EMACSLOADPATH': '{}'.format(loadpath.stdout)}
    c.run("cask exec ert-runner -L ./lisp -L ./test -l test/testfunc.el test/test-func.el",
          env=updated_env)
    c.run("cask exec ecukes --reporter magnars", env=updated_env)


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
