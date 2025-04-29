import { NextApiRequest, NextApiResponse } from 'next';
import formData from 'form-data'
import Mailgun from 'mailgun.js'
const mailgun = new Mailgun(formData);

const DOMAIN = "email.treywilkinson.com";
const mg = mailgun.client({username: 'api', key: process.env.MAILGUN_API!});

export default async (req: NextApiRequest, res: NextApiResponse) => {
  const { email, message, name } = req.body
  const msg = {
    to: 'winston.trey.wilkinson@gmail.com',
    from: `${name} <${email}>`,
    subject: `[Portfolio] Message`,
    text: message,
  };

  try {
    await mg.messages.create(DOMAIN, msg)
    res.json({ message: `Email has been sent` })
  } catch (error) {
    res.status(500).json({ error: 'Error sending email' })
  }
}
