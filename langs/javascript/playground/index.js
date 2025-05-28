const nodemailer = require('nodemailer');
const { google } = require("googleapis")
const OAuth2 = google.auth.OAuth2;

const oauth2Client = new OAuth2(
     "914245121301-cknfi48roprdskegl03enajj4bu9o68i.apps.googleusercontent.com", // ClientID
     "-0AjGzv2vewbTF_ERvgJu9dx", // Client Secret
     "https://developers.google.com/oauthplayground" // Redirect URL
);

// let mailTransporter = nodemailer.createTransport({
//     service: 'gmail',
//     auth: {
//         user: 'info@infiniteclosetuk.com',
//         pass: 'Kingston2020',
//     }
// });

// let mailDetails = {
//     from: 'info@infiniteclosetuk.com',
//     to: 'trey.wilkinson@infiniteclosetuk.com',
//     subject: 'Test mail',
//     text: 'Node.js testing mail for GeeksforGeeks'
// };

// mailTransporter.sendMail(mailDetails, function(err, data) {
//     if(err) {
//       console.log(err)
//         console.log('Error Occurs');
//     } else {
//         console.log('Email sent successfully');
//     }
// });
