import React from 'react'
import { BrowserRouter as Router, Switch, Route } from 'react-router-dom'
const Home = React.lazy(() => import('./routes/Home'))
const Resume = React.lazy(() => import('./routes/Resume'))
const Learn = React.lazy(() => import('./routes/Learn'))

export default function App() {
  return (
    <div>
      <Router>
        <React.Suspense fallback={<Loading />}>
          <Switch>
            <Route exact path="/" component={Home} />
            <Route exact path="/resume" component={Resume} />
            <Route exact path="/blog" component={Learn} />
          </Switch>
        </React.Suspense>
      </Router>
    </div>
  )
}

function Loading() {
  return (
    <div
      style={{
        background: '#1c2124',
        width: '100vw',
        height: '100vh',
        position: 'absolute',
        top: '0',
        left: '0',
      }}
    ></div>
  )
}
