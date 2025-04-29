const multer = require('multer')
const crypto = require('crypto')
const { exec } = require('child_process')
const { open } = require('sqlite')
const sqlite3 = require('sqlite3')

export const config = {
  api: {
    bodyParser: false,
  }
}

const uniqueSuffix = () => Date.now() + '-' + Math.round(Math.random() * 1E9)

var storage = multer.diskStorage({
  destination: process.env.UPLOADS,
  filename: function (req, file, cb) {
    const ext = file.mimetype.split(/\//)[1]
    cb(null, `${file.fieldname}-${uniqueSuffix()}.${ext}`)
  }
})

var upload = multer({ storage: storage })

function runMiddleware(req, res, fn) {
  return new Promise((resolve, reject) => {
    fn(req, res, (result) => {
      if (result instanceof Error) {
        return reject(result)
      }
      return resolve(result)
    })
  })
}

export default async function handler(req, res) {
  await runMiddleware(req, res, upload.fields([{name: 'content-image'}, {name: 'style-image'}]))
  const token = crypto.createHash('md5').update(uniqueSuffix()).digest('hex')

  const content_image = req.files['content-image'][0].path
  const style_image = req.files['style-image'][0].path
  exec(`python run_neural_style.py -c ${content_image} -s ${style_image} -o ${process.env.UPLOADS}/output-${token}.jpg --token ${token}`,
    (err, stdout, stderr) => {
    })

  open({
    filename: 'db.sqlite',
    driver: sqlite3.Database,
  }).then((db) => {
    db.run('INSERT OR IGNORE INTO emails VALUES (?)', req.body.email)
    db.run('INSERT INTO tokens VALUES (?, ?)', token, req.body.email)
  })

  res.statusCode = 200
  res.end()
}
