const express = require("express")
const stripe = require("stripe")("TEST_KEY")
const cookieParser = require("cookie-parser")
const http = require("http")
const bodyParser = require("body-parser")
const nodemailer = require("nodemailer")

const app = express()
app.use(cookieParser())
// app.use(express.json())

const webhookSecret = "WEBHOOK_SECRET"
app.post("/stripe/webhook", bodyParser.raw({type: 'application/json'}), (req, res) => {
  let event, object
  const sig = req.get("stripe-signature")

  try {
    event = stripe.webhooks.constructEvent(req.body, sig, webhookSecret)
    object = event.data.object
  } catch (e) {
    res.status(400).send(`Webhook Error: ${e.message}`).end()
    return
  }

  switch(event.type) {
    case "payment_intent.created":
      break
    case "payment_intent.succeeded":
      paymentIntentSucceeded(object.metadata.cookie)
      .then(() => {
        console.log(`You just got ${object.amount/100}!!!`)
        return sendMail(object.metadata.email)
      })
      .then(() => {
        console.log(`sent email to ${object.metadata.email}`)
      })
      .catch(() => {
        res.status(500).end()
      })
      break
    default: break
  }
  res.end()
})

const paymentIntentSucceeded = cookie => new Promise((resolve, reject) => {
  const req = http.request("http://localhost/api/booth/user/make-payment", {
    method: "PUT", headers: { "cookie": cookie, }
  }, (res) => { resolve() })
  req.on("error", (e) => reject(e.message))
  req.end()
})

app.post("/stripe/create-payment-intent", bodyParser.json({type: "*/*"}), async (req, res) => {
  const { items } = req.body

  // getCharge from backend
  .then(charge => {
    if (charge < 50) {
      throw new Error("Charge must be larger than $0.50")
    }
    return stripe.paymentIntents.create({
      amount: charge,
      currency: "usd",
      metadata: {
        cookie: req.get("cookie"),
        email: req.cookies.user,
      },
    })
  })
  .then(paymentIntent => {
    res.send({
      charge: paymentIntent.amount,
      customerId: paymentIntent.client_secret,
    }).end()
  })
  .catch(e => {
    res.status(404).end()
  })

})

app.listen(4242, () => console.log('Node server listening on port 4242!'))

// const sendMail = (email) => new Promise((resolve, reject) => {
//   // no-reply@
//   // hello@
//   let transporter = nodemailer.createTransport({
//     service: "gmail",
//     auth: {
//       user: "test@gmail.com",
//       pass: "test",
//     },
//   })
//   transporter.sendMail({
//     from: "nationistrey@gmail.com", // sender address
//     to:email, // list of receivers
//     subject: "Hello ✔", // Subject line
//     // text: "Hello world?", // plain text body
//     html: "<b>Hello world?</b>", // html body
//   }, (e, data) => {
//     if (e) reject(e.message)
//     else resolve(data)
//   })
// })
