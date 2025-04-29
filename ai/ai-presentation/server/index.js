const express = require('express');
const bodyParser = require('body-parser');
const multer = require('multer');
const cors = require('cors')
const app = express();
const fileupload = require('express-fileupload')
const { spawnSync } = require('child_process')
const process = require('process')
const path = require('path')

const tmpFolder = '/tmp/style-transfer-uploads'
const tmpSuffix = () => Date.now() + '-' + Math.round(Math.random() * 1E9)

const storage = multer.diskStorage({
   destination: tmpFolder,
   filename: (_req, file, cb) => {
    cb(null, file.fieldname + '-' + tmpSuffix())
  },
})
const upload = multer({storage});

/** Run style transfer and returns the path of the generated image. */
async function runStyleTransfer(styleImg, contentImg) {
   const outputImg = `${tmpFolder}/${tmpSuffix()}.jpg`
   const args = ['-s', styleImg, '-c', contentImg, '-o', outputImg, '--image-size', 300, '--style-weight', 100000]
   const cwd = path.normalize(path.join(process.cwd(), '../style-transfer'))
   await spawnSync('./run_neural_style', args , { cwd });

   return outputImg
}

const corsOptions = {
   origin: ['http://localhost:3000', 'http://localhost:3008']
}

app.post('/style-transfer', cors(corsOptions), upload.any(), async function(req, res){
   const styleImg = req.files.find(o => o.fieldname === 'style')
   const contentImg = req.files.find(o => o.fieldname === 'content')

   try {
     const outputImg = await runStyleTransfer(styleImg.path, contentImg.path)
     res.sendFile(outputImg)
   } catch (e) {
     console.error(e)
   }
});

app.use(
   cors(),
   fileupload(),
   bodyParser.json()
);

// for parsing application/x-www-form-urlencoded
app.use(bodyParser.urlencoded({ extended: true }));

// for parsing multipart/form-data
app.use(upload.array());
app.use(express.static('public'));

app.listen(3008, () => {
   console.log(`Listening on port ${3008}`)
});
