import logging

from flask import Flask, render_template, request
from flask.json import jsonify

FLASK_HOST = '0.0.0.0'
FLASK_PORT = 4000
FLASK_DEBUG = True

app = Flask(__name__,
            template_folder='.',
            static_url_path='',
            static_folder='dist')


@app.route('/api/mixes')
def mixes():
    return jsonify([
      {
        "id":1,
        "name": "first mix"
      },
      {
        "id":2,
        "name": "second mix"
      },
      {
        "id":3,
        "name": "third mix"
      },
      {
        "id":4,
        "name": "forth mix"
      },
      {
        "id":5,
        "name": "fifth mix"
      },
      {
        "id":6,
        "name": "sixth mix"
      }
    ])

    
@app.route('/api/mixes/<id>')
def mix(id):
  return jsonify([
    {
      "id":1,
      "name":"Ch1",
      "on":False,
      "image":"https://api.adorable.io/avatars/285/channel-1.png"
    },
    {
      "id":2,
      "name":"Ch2",
      "on":False,
      "image":"https://api.adorable.io/avatars/285/channel-2.png"
    },
    {
      "id":3,
      "name":"Ch3",
      "on":False,
      "image":"https://api.adorable.io/avatars/285/channel-3.png"
    },
    {
      "id":4,
      "name":"Ch4",
      "on":False,
      "image":"https://api.adorable.io/avatars/285/channel-4.png"
    },
    {
      "id":5,
      "name":"Ch5",
      "on":False,
      "image":"https://api.adorable.io/avatars/285/channel-5.png"
    },
    {
      "id":6,
      "name":"Ch6",
      "on":False,
      "image":"https://api.adorable.io/avatars/285/channel-6.png"
    }
  ])


if __name__ == '__main__':
    logging.basicConfig(
        format='%(levelname)s|%(asctime)s|%(name)s|%(message)s',
        level=logging.INFO
    )

    app.run(host=FLASK_HOST, port=FLASK_PORT, debug=FLASK_DEBUG)