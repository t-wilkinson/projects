const config = require('./config')
const fetch = require('node-fetch')

function encrypt() {
}

function decrypt() {
}

let connectToRemoteServerTimeout

// We want to call this imediately and on a timeout
function _connectToRemoteServer() {
  fetch(`${config.remoteServer}`, {
    method: 'POST',
    body: JSON.stringify({
      id: config.id,
      name: config.serverName,
      key: config.key,
    }),
    headers: {'Content-Type': 'application/json'}
  })
}

function connectToRemoteServer() {
  if (connectToRemoteServerTimeout) {
    clearInterval(connectToRemoteServerTimeout)
  }
  _connectToRemoteServer()
  connectToRemoteServerTimeout = setInterval(_connectToRemoteServer, 30 * 60 * 1000)
}

module.exports = {
  encrypt,
  decrypt,
  connectToRemoteServer
}
