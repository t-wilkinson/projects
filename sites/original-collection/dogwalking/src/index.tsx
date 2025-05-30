import 'react-app-polyfill/stable';
import React from "react"
import ReactDOM from "react-dom"
import {subscribe, Provider} from "./store"
import App from "./components/App"

subscribe(st => {
  ReactDOM.render(
    <React.StrictMode>
      <Provider value={st}>
        <App />
      </Provider>
    </React.StrictMode>,
    document.getElementById("root")
  )
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
/* serviceWorker.unregister() */
