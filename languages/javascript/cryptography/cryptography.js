import crypto from 'crypto'
import fs from 'fs'
import CryptoJS from 'crypto-js'
import bs58 from 'bs58'
import Base64 from 'crypto-js/enc-base64.js';

const secretKey = 'secret key'

function fileShaSum() {
  var filename = process.argv[2]
  var shasum = crypto.createHash('sha1')

  var s = fs.ReadStream(filename)
  s.on('data', function (d) {
    shasum.update(d)
  })

  s.on('end', function () {
    var d = shasum.digest('hex')
    console.log(d + '  ' + filename)
  })
}

function generateCouponCodeHash(clientSecret) {
  const hash = crypto.createHash('sha256')
  hash.update(clientSecret)
  return hash.digest('base64')
}

function generateCouponCode(clientSecret, key) {
  const hmac = crypto.createHmac('sha256', key)
  hmac.update(clientSecret)
  const d = hmac.digest()
  return bs58.encode(d).slice(0, 6).length
}

function generateKey() {
  return crypto.randomBytes(64).toString('base64')
}

function encryption(data) {
  const hex = CryptoJS.AES.encrypt(JSON.stringify(data), secretKey).toString(CryptoJS.format.Hex)
  return parseInt(hex, 16)
}

function main() {
  // const key = generateKey()
  // console.log('key', key)
  // console.log('code', generateCouponCode('client_secret', key))
  console.log(encryption(1))
}

main()
