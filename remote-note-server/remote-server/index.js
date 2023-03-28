const crypto = require('crypto')
const Koa = require('koa')
const http = require('koa-router')()
const bodyParser = require('koa-bodyparser')

// LocalServers := { name: string; id: string; key: string }
let localServers = {}

function isValidServer(server) {
  return server.name && server.id && server.key
}

function hash(message) {
  return crypto.createHash('sha256').update(message).digest('hex')
}

app.on('error', err => {
  console.error('server error', err)
})

// Connect the client to local server
http.post('/servers/connect', async ctx => {
  const { id, password } = ctx.request.body
  if (!localServers[id]) {
    return ctx.throw(404, 'Server could not be found')
  } else if (localServers[id]?.key === hash(password)) {
    ctx.status = 200
    ctx.set('Content-Type', 'application/json')
    ctx.body = localServers[id]
    return
  } else {
    return ctx.throw(401, 'Invalid credentials')
  }
})

// Add server
http.post('/servers', async ctx => {
  const server = ctx.request.body
  if (!isValidServer(server)) {
    return ctx.throw(400, 'Server is invalid')
  }

  localServers[id] = server

  ctx.status = 200
  ctx.body = null
})

// Get a list of servers
http.get('/servers', async ctx => {
  const { name } = ctx.request.query
  ctx.body = Object.values(localServers).filter(server => server.name === name)
  ctx.set('Content-Type', 'application/json')
})

const app = new Koa()
// prettier-ignore
app
  .use(bodyParser({ enableTypes: ['text', 'plain', 'json']}))
  .use(http.routes())
  .use(http.allowedMethods())

const port = 4001
console.info(`Listening on port ${port}`)
app.listen(port)
