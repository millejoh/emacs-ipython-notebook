import os

from flask import (
    Flask, request, session, redirect, abort, jsonify)
from werkzeug.http import HTTP_STATUS_CODES

app = Flask(__name__)
app.secret_key = 'SECRET-KEY-FOR-EMACS-REQUEST-DEVELOPMENT'

all_methods = ['GET', 'POST', 'PUT', 'DELETE']


# View functions


@app.route('/report/<path:path>', methods=all_methods)
def page_report(path):
    """
    Report back path, input data, parameter, etc. as JSON.
    """
    # see: http://flask.pocoo.org/docs/api/#incoming-request-data
    return jsonify(dict(
        path=path,
        data=request.data,
        form=request.form,
        files=[dict(name=k, filename=f.filename, data=f.read())
               for (k, f) in request.files.items()],
        args=request.args,
        cookies=request.cookies,
        method=request.method,
        json=request.json,
        username=session.get('username'),
    ))


@app.route('/redirect/<path:path>', methods=all_methods)
def page_redirect(path):
    return redirect(path)


@app.route('/broken_redirect/<path:path>', methods=all_methods)
def page_broken_redirect(path):
    """
    A pathological redirection.  Location does not contain the scheme part.
    """
    response = redirect(path)
    response.headers['Location'] = '/' + path  # URL w/o scheme part
    response.autocorrect_location_header = False
    return response


@app.route('/code/<int:code>')
def page_code(code):
    try:
        return abort(code)
    except LookupError:
        return HTTP_STATUS_CODES[code], code


@app.route('/sleep/<float:sleep>')
def page_sleep(sleep):
    import time
    time.sleep(sleep)
    return redirect('report/from-sleep')


@app.route('/login', methods=['GET', 'POST'])
def page_login():
    error = 'Not logged-in'
    if request.method == 'POST':
        username = request.form['username']
        if 'invalid' in username:
            error = 'Invalid username'
        elif 'invalid' in request.form['password']:
            error = 'Invalid password'
        else:
            session['username'] = username
            return redirect('report/from-login')
    return error


@app.route('/logout')
def page_logout():
    session.pop('username', None)
    return redirect('report/from-logout')


@app.route('/cookies/set')
def page_set_cookies():
    # see: http://flask.pocoo.org/docs/quickstart/#cookies
    resp = redirect('report/from-cookies')
    for (name, value) in request.args.items():
        resp.set_cookie(name, value)
    return resp


# Runner


def get_open_port():
    import socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind(("", 0))
    s.listen(1)
    port = s.getsockname()[1]
    s.close()
    return port


def run(port, server, **kwds):
    import sys
    port = port or get_open_port()
    # Pass port number to child process via envvar.  This is required
    # when using Flask's reloader.
    os.environ['EL_REQUEST_TEST_PORT'] = str(port)
    print port
    sys.stdout.flush()

    if server == 'flask':
        app.run(port=port, **kwds)
    else:
        app.debug = True
        from tornado.wsgi import WSGIContainer
        from tornado.httpserver import HTTPServer
        from tornado.ioloop import IOLoop
        http_server = HTTPServer(WSGIContainer(app))
        http_server.listen(port)
        print " * Running on", port
        IOLoop.instance().start()


def main(args=None):
    import argparse
    default_port = int(os.environ.get('EL_REQUEST_TEST_PORT', '0'))
    default_server = os.environ.get('EL_REQUEST_TEST_SERVER') or 'flask'
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default=default_port, type=int)
    parser.add_argument('--use-reloader', default=False, action='store_true')
    parser.add_argument('--server', default=default_server,
                        choices=['flask', 'tornado'])
    ns = parser.parse_args(args)
    run(**vars(ns))


if __name__ == '__main__':
    main()
