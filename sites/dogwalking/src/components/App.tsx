import React from "react"
import {BrowserRouter as Router, Switch, Route, useLocation} from "react-router-dom"
import Home from "../routes/Home"
import Calendar from "../routes/Calendar"
import Login from "../routes/Login"
import Register from "../routes/Register"
import Header from "./Header"
import Footer from "./Footer"
import {dispatch} from "../store"

function App() {
  React.useEffect(() => {
    document.title = "Paw Pals"
    dispatch({filter: "root", type: "getUser", onError: ()=>{}})
  }, [])

  return <div
    className="App"
  >
    <Router>
      <OnPageChange />
      <Header />
      <Switch>
        <Route path="/login" component={Login} />
        <Route path="/register" component={Register} />
        <Route path="/calendar" component={Calendar} />
        <Route path="/" component={Home} />
      </Switch>
    <Footer />
    </Router>
  </div>
}

const OnPageChange = () => {
  let {pathname} = useLocation()
  React.useEffect(() => {
    window.scrollTo(0,0)
  }, [pathname])
  return null
}

export default App

