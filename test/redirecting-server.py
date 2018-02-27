from flask import Flask, redirect

app = Flask(__name__)

@app.route('/')
def jupyter_redirect():
    return redirect("http://127.0.0.1:8888/", code=302)

@app.route('/api')
def api_check():
    return redirect("http://127.0.0.1:8888/api", code=302)

if __name__=='__main__':
    port = int(8000)
    app.run(host='127.0.0.1', port=port)


