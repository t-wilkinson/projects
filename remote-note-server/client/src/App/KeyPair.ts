import * as openpgp from 'openpgp'

export const createKeyPair = async () => {
  const { privateKey, publicKey } = await openpgp.generateKey({
    type: 'rsa',
    rsaBits: 4096,
    userIDs: [{ name: 'Jon Smith', email: 'jon@example.com' }], // you can pass multiple user IDs
    // passphrase: 'super long and hard to guess secret' // protects the private key
  })
  return {privateKey, publicKey}
}

