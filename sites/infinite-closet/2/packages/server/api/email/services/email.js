"use strict";
const cp = require("child_process");
const path = require("path");
const { google } = require("googleapis");

function sendMessage(auth, to, from, subject, message) {
  var raw = makeBody(to, from, subject, message);

  const gmail = google.gmail({ version: "v1", auth });
  gmail.users.messages
    .send({
      auth: auth,
      userId: "me",
      resource: {
        raw: raw,
      },
    })
    .catch(() => {});
}

function makeBody(to, from, subject, message) {
  var str = [
    'Content-Type: text/html; charset="UTF-8"\n',
    "MIME-Version: 1.0\n",
    "Content-Transfer-Encoding: 7bit\n",
    "to: ",
    to,
    "\n",
    "from: ",
    from,
    "\n",
    "subject: ",
    subject,
    "\n\n",
    message,
  ].join("");

  var encodedMail = Buffer.from(str)
    .toString("base64")
    .replace(/\+/g, "-")
    .replace(/\//g, "_");

  return encodedMail;
}

module.exports = {
  send: async ({ name, to, subject, html }) => {
    const JWT = google.auth.JWT;
    const authClient = new JWT({
      keyFile: path.resolve(__dirname, "credentials.json"),
      scopes: [
        "https://mail.google.com",
        "https://www.googleapis.com/auth/gmail.compose",
        "https://www.googleapis.com/auth/gmail.modify",
        "https://www.googleapis.com/auth/gmail.readonly",
      ],
      subject: "info@infinitecloset.co.uk",
    });

    const from = `${name} <info@infinitecloset.co.uk>`;
    console.log("sending from JWT");

    authClient
      .authorize()
      .then(() => sendMessage(authClient, to, from, subject, html))
      .catch(() => {});
  },
};
