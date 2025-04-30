// const crypto = require('crypto')
// const fs = require('fs')

// const secret = 'secret'

// const hash = crypto
//   .createHmac('sha256', secret)
//   .update('this is some text')
//   .digest('hex')

// console.log(hash)

// const algorithm  = 'aes-192-cbc'
// const password = 'my password'
// const key = crypto.scryptSync(password, 'salt', 24)
// const iv = Buffer.alloc(16, 0)
// const cipher = crypto.createCipheriv(algorithm, key, iv)

// let encrypted = cipher.update('some text', 'utf8', 'hex')
// encrypted += cipher.final('hex')
// console.log(encrypted)
const crypto = require('crypto');
const fs = require('fs');

const algorithm = 'aes-192-cbc';
const password = 'Password used to generate key';
// Use the async `crypto.scrypt()` instead.
const key = crypto.scryptSync(password, 'salt', 24);
// Use `crypto.randomBytes()` to generate a random iv instead of the static iv
// shown here.
const iv = Buffer.alloc(16, 0); // Initialization vector.

const cipher = crypto.createCipheriv(algorithm, key, iv);

const input = fs.createReadStream('test.js');
const output = fs.createWriteStream('test.enc');

input.pipe(cipher).pipe(output);
