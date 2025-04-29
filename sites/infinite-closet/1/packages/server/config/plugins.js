module.exports = ({ env }) => ({
  email: {
    provider: 'sendmail',
    providerOptions: {
      logger: {
        debug: console.log,
        info: console.info,
        warn: console.warn,
        error: console.error
      },
      silent: false,
      // dkim: { // Default: False
      //   privateKey: fs.readFileSync('./dkim-private.pem', 'utf8'),
      //   keySelector: 'mydomainkey'
      // },
      devHost: 'smtp.gmail.com',
      smtpHost: 'smtp.gmail.com',
      // devPort: 1025, // Default: False
      // devHost: 'localhost', // Default: localhost
      smtpPort: 587,
      // smtpPort: 2525, // Default: 25
      // smtpHost: 'localhost' // Default: -1 - extra smtp host after resolveMX
    },
    settings: {
      defaultFrom: 'nationistrey@gmail.com',
      defaultReplyTo: 'nationistrey@gmail.com',
    },
  },
});
