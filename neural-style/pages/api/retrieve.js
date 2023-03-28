const fs = require('fs')

export default async function handler(req, res) {
  const body = JSON.parse(req.body)
  const token = body.token

  try {
    const data = fs.readFileSync(`${process.env.UPLOADS}/output-${token}.jpg`)
    res.setHeader('Content-Type', 'image/jpeg')
    res.status = 200
    res.end(data)

  } catch (err) {
    res.status = 404
    res.end()

  } finally {
    res.end()
  }

}
