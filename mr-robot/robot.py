from flask import Flask, render_template
from flask_socketio import SocketIO, send, emit
import base64
import gpiozero
from picamera import PiCamera
import time
import io

port = 5000

app = Flask(__name__)
app.config['SECRET_KEY'] = 'secret!'
socketio = SocketIO(app, cors_allowed_origins="*")

camera = PiCamera()
camera.resolution = (640, 480)

@socketio.on('connected')
def handle_connect(data):
    '''Initialize robot.'''
    print('connected')
    pass

@socketio.on('keypress')
def handle_keypress(direction):
    '''Move robot according to direction.'''
    print(direction)
    pass

def send_image():
    image = io.BytesIO()
    camera.capture(image, 'jpeg')
    emit('image', {buffer: base64.b64encode(image)})

if __name__ == '__main__':
    print(f'Running on port {port}.')
    socketio.run(app, port=port)
