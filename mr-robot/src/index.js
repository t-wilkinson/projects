import React from 'react'
import ReactDOM from 'react-dom'
// import reportWebVitals from './reportWebVitals'
import { io } from 'socket.io-client'
import './index.css'

const socket = io('192.168.0.20:5000')

ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById('root')
)

function App() {
  const refs = {}

  React.useEffect(() => {
    socket.on('connect', () => {
      socket.emit('connected', {})
    })
  }, [])

  React.useEffect(() => {
    if (refs.canvas === null) return
    socket.on('image', ({buffer}) => {
      const img = new Image()
      img.src = 'data:image/jpeg;base64,' + buffer
      const ctx = refs.canvas.getContext('2d')
      ctx.drawImage(img, 0, 0)
    })
  }, [refs.canvas])

  return <div className='App'>
    <KeyPress />
    <canvas ref={(ref)=>refs.canvas = ref} />
  </div>

}

const keyPresses = {
  'ArrowLeft': 'left', 'h': 'left', 'a': 'left',
  'ArrowRight': 'right', 'l': 'right', 'd': 'right',
  'ArrowUp': 'up', 'k': 'up', 'w': 'up',
  'ArrowDown': 'down', 'j': 'down', 's': 'down',
}

function KeyPress() {
  const refs = {}

  React.useEffect(() => {
    if (refs.keypress === null) return
    function sendKeyPress(event) {
      const direction = keyPresses[event.key]
      if (direction === undefined) return
      socket.emit('keypress', direction)
    }
    refs.keypress.addEventListener("keydown", sendKeyPress)
    return () => refs.keypress.removeEventListener('keydown', sendKeyPress)
  }, [refs.keypress])

  const borderTransparent = '2rem solid transparent'
  const borderSolid = '2rem solid black'

  return <div tabIndex={-1} className='keypress' ref={(ref) => refs.keypress = ref}>
    <ControlButton direction='up' style={{transform: 'translateY(-10em)', borderLeft: borderTransparent, borderRight: borderTransparent, borderBottom: borderSolid}} />
    <ControlButton direction='down' style={{transform: 'translateY(10em)', borderLeft: borderTransparent, borderRight: borderTransparent, borderTop: borderSolid}} />
    <ControlButton direction='left' style={{transform: 'translateX(-10em)', borderTop: borderTransparent, borderBottom: borderTransparent, borderRight: borderSolid}} />
    <ControlButton direction='right' style={{transform: 'translateX(10em)', borderTop: borderTransparent, borderBottom: borderTransparent, borderLeft: borderSolid}} />
  </div>
}

function ControlButton({direction, ...props}) {
  return <button
    className='control-button'
    onClick={() => {
      socket.emit('keypress', direction)
    }}
    {...props}
  >
  </button>
}

// reportWebVitals()
