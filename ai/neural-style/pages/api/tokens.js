const sqlite3 = require('sqlite3')
const { open } = require('sqlite')
const fs = require('fs')

export default async function handler(req, res) {
  const body = JSON.parse(req.body)

  let tokens = await open({
    filename: 'db.sqlite',
    driver: sqlite3.Database,
  })
    .then((db) => db.all('SELECT token FROM tokens WHERE email = ?', body.email))
  tokens = tokens.map((t) => t.token)

  res.status = 200
  res.json({tokens: tokens})
}

