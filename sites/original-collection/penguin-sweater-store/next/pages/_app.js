import '../styles.css'
import React from 'react'

import '@fortawesome/fontawesome-svg-core/styles.css' // Import the CSS
import { library, config } from '@fortawesome/fontawesome-svg-core'
import { faBars, faTimes, faAngleDown, faDollarSign, faShoppingCart, faQuoteLeft } from '@fortawesome/free-solid-svg-icons'

library.add( faTimes, faAngleDown, faDollarSign, faShoppingCart, faQuoteLeft, faBars )
config.autoAddCss = false // Tell Font Awesome to skip adding the CSS automatically since it's being imported above

export default function MyApp({ Component, pageProps }) {
  return <Component {...pageProps} />
}
