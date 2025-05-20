import React from "react"
import {withRouter, BrowserRouter as Router, Switch, Route} from "react-router-dom"
import gsap from "gsap"
import Header from "./Header"
import Footer from "./Footer"
import "./App.css"
import "./index.css"
import {ScrollTrigger} from "gsap/ScrollTrigger"
import {dispatch, useStore} from "../store"
gsap.registerPlugin(ScrollTrigger)

const Home = React.lazy(() => import("../routes/Home"))
const Dashboard = React.lazy(() => import("../routes/Dashboard"))
const Login = React.lazy(() => import("../routes/Login"))
const Register = React.lazy(() => import("../routes/Register"))
const Projects = React.lazy(() => import("../routes/Projects"))
const Admin = React.lazy(() => import("../routes/Admin"))
const ConfirmPayment = React.lazy(() => import("../routes/Dashboard/Confirm"))
const Lost = React.lazy(() => import("../routes/Lost"))

export default () => {
  React.useEffect(() => {
    document.title = "Klean Studios"
    dispatch({filter: "root", type: "getUser"})
  }, [])

  return <div id="app">
    <Router>
      <OnPageChange />
      <Header />
      <React.Suspense
        fallback={<div className="fixed inset-0 bg-pri grid" style={{placeItems: "center"}}></div>}
      >
        <Switch>
          <Route path="/projects" component={Projects} />
          <Route path="/dashboard/confirm" component={ConfirmPayment} />
          <Route path="/dashboard" component={Dashboard} />
          <Route path="/login" component={Login} />
          <Route path="/register" component={Register} />
          <Route path="/admin" component={Admin} />
          <Route exact path="/" component={Home} />
          <Route component={Lost} />
        </Switch>
        <Footer />
      </React.Suspense>
    </Router>
  </div>
}

const OnPageChange = withRouter(({history}) => {
  let root = useStore(s=>s.root)
  React.useEffect(() => {
    const unlisten = history.listen(() => window.scrollTo(0,0))
    return () => unlisten()
  }, [history])

  if (root.userAgent('mobile')) {
    return null
  } else {
    return <ScrollBar key={history} />
  }
})

const ScrollBar = (props) => {
  const refs = {
    scrollbar: React.useRef(null)
  }

  React.useEffect(() => {
    let scrollbar = refs.scrollbar.current
    if (scrollbar === null) return

    gsap.fromTo(
      scrollbar,
      {y: 0},
      { y: window.innerHeight - scrollbar.clientHeight,
        ease: "linear",
        scrollTrigger: {
          id: "scrollbar",
          trigger: document.body,
          scrub: 0,
          start: "top top",
          end: "bottom bottom",
        }
      }
    )

  }, [props, refs.scrollbar])

  return <div
    ref={refs.scrollbar}
    className="w-2 h-48 bg-black fixed top-0 right-0 z-30"
  />
}
