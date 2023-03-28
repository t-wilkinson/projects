require('dotenv').config()
const fs = require('fs')
const os = require('os')
const path = require('path')

function expandPath(filepath) {
    if (filepath[0] === '~') {
        return path.join(os.homedir(), filepath.slice(1));
    }
    return filepath;
}

const notesDir = expandPath(process.env.NOTES_DIR) || './test/notes'
if (!fs.existsSync(notesDir)) {
  fs.mkdirSync(notesDir)
}

const staticFileDir = expandPath(process.env.STATIC_FILE_DIR) || './test/static'
if (!fs.existsSync(staticFileDir)) {
  fs.mkdirSync(staticFileDir)
}

module.exports = {
  notesDir, // Default notes directory
  staticFileDir,
  remoteServer: 'https://zettelkasten.treywilkinson.com/api',
  id: 'my-unique-id',
  serverName: 'Local Server',
  key: '<key>',
}
