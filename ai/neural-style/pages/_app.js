import '../styles/globals.css'
import Head from 'next/head'

import { theme, Header } from '../components'

function MyApp({ Component, pageProps }) {
  return <div className='main'>
    <Head>
      <link rel="preconnect" href="https://fonts.gstatic.com" />
      <link href="https://fonts.googleapis.com/css2?family=Raleway&family=Sarpanch:wght@900&display=swap" rel="stylesheet" />
      <link rel="shortcut icon" href="/assets/images/logo.svg" />
      <title>Style Transfer</title>
    </Head>

    <style global jsx>{`
      * {
        color: black;
        font-family: inherit;
        box-sizing: border-box;
      }

      html, body { overflow-x: hidden; }
      body {
        font-family: ${theme.fontSec};
        background: url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAYAAABytg0kAAAAFElEQVQIW2NkYGAwZmBgOMvIAAUACvkBAyxEG4oAAAAASUVORK5CYII=") repeat;
      }

      .main {
        display: grid;
        grid-template-columns: 1fr 1000px 1fr;
        grid-template-rows: 1fr;
        height: 100vh;
      }

      main { grid-column: 2; align-self: center; }

      li { list-style: none; }

      .box {
        border: 2px solid black;
      }

      .box::after {
        z-index: -1;
        top: 0; right: 0; left: 0; bottom: 0;
        position: absolute;
        content: '';
        transform: translate(10px, 10px);
        transition: all ease-in-out 0.25s;
        background: black;
        border: 2px solid black;
      }
  `}</style>
    <Header />
    <Component {...pageProps} />
  </div>
}

export default MyApp
