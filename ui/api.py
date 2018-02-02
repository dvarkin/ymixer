import logging
import random
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
    return jsonify([1, 2, 3, 4, 5, 6])

    
@app.route('/api/mix/<id>')
def mix(id):
  def chan(id):
    return {
      "id": id,
      "on":random.choice([True, False]),
      "image": "https://api.adorable.io.crap/avatars/285/channel-{}.png".format(id)
    }
  
  return jsonify([chan(id) for id in range(0, 10)])


if __name__ == '__main__':
    logging.basicConfig(
        format='%(levelname)s|%(asctime)s|%(name)s|%(message)s',
        level=logging.INFO
    )

    app.run(host=FLASK_HOST, port=FLASK_PORT, debug=FLASK_DEBUG)
