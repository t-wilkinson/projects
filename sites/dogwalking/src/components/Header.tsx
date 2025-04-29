import {NavLink, Link, useLocation, useHistory} from "react-router-dom"
import React from "react"
import {
  SocialMedia, Logo
} from "./"
import gsap from "gsap"
import {useStore, actions, dispatch} from "../store"

const {header: {updateTl, toggle}} = actions

export default () => {
  const state = useStore(s => s.header)
  const rootState = useStore(s => s.root)

  React.useEffect(() => {
    updateTl({user: rootState.user})
    toggle({payload: false})
  }, [rootState.user])

  React.useEffect(() => {
    function resize() {
      if (window.innerWidth >= 640 && state.visible)
      toggle({payload: false})
    }
    window.addEventListener("resize", resize)
    return () => window.removeEventListener("resize", resize)
  }, [state.visible])

  return  <div>
    <Aside user={rootState.user} form={state.form} />
    <Header user={rootState.user} from={state.form} />
    <Menu
      id="header-menu"
      active={state.visible}
      height="33px" width="51px"
      onClick={(e) => toggle()}
    />
  </div>
}

export const Menu = ({active=false, duration=0.7, bg="#fff", onClick=(e)=>{}, ...props}) => {
  // 20c9c6
  let [state,] = React.useState({tl: gsap.timeline()})
  let ref = React.useRef(null)
  React.useEffect(() => {
    state.tl.add(gsap.timeline({defaults: {duration: duration, ease: "power3.out"}})
      .fromTo(ref.current.children, {background: bg}, {background: "#000"}, 0)
      .fromTo(ref.current.children[0], {rotate: "0", y: 0}, {rotate: "45deg", y: 8.0}, 0)
      .fromTo(ref.current.children[1], {opacity: 1}, {opacity: 0, }, 0)
      .fromTo(ref.current.children[2], {rotate: "0", y: 0}, {rotate: "-45deg", y: -8.0}, 0)
    )
    state.tl.pause()
  }, [state.tl, bg, duration])

  React.useEffect(() => {
    active ? state.tl.play() : state.tl.reverse()
  }, [active, state.tl])

  return <div
    className="fixed top-0 right-0 z-20 py-2 px-4"
  >
    <ul
      {...props}
      ref={ref}
      onClick={(e) => {
        onClick(e)
      }}
      className="w-12 h-10 col py-2 justify-around sm:hidden outline-none cursor-pointer bg-pri rounded-lg"
      tabIndex={-1}
    >
      <li className="h-1 w-8 rounded-sm bg-sec"></li>
      <li className="h-1 w-8 rounded-sm bg-sec"></li>
      <li className="h-1 w-8 rounded-sm bg-sec"></li>
    </ul>
  </div>
}

const Aside = (props) => {
  let history = useHistory()
  return <aside
    id="header-aside"
    className="fixed inset-0 text-black z-20 hidden"
  >
    <div
      id="header-aside-bg"
      className="absolute inset-0 transform -translate-y-full bg-pri-2"
    />
    <div
      id="header-aside-content"
      className="relative w-full h-full flex flex-col"
    >
      <span
        className="absolute top-0 left-0 right-0 mt-16 flex justify-center"
      >{window.innerWidth < 400
        ? props.user
        : ""
        }
      </span>
      <ul
        className="text-5xl space-y-8 flex-auto col justify-center"
      >
        <HeaderLink to="/">Home</HeaderLink>
        {props.user
          ? <button
            className={`${props.form === "login" ? "text-gray" : ""}
              font-pri hover:text-pri`}
            onClick={() => {dispatch({filter: "root", type: "logout", history: history})}}
          >Logout</button>
          : <>
            <HeaderLink to="/login">Login</HeaderLink>
            <HeaderLink to="/register">Register</HeaderLink>
          </>
        }
        <HeaderLink to="/calendar">Calendar</HeaderLink>
      </ul>
      <ul className="text-3xl pl-16 mb-16">
        <div className="flex flex-wrap-reverse">
          <SocialMedia className="w-10 h-10 mr-4 text-black" />
        </div>
        <li><span className="text-black">pawpals@gmail.com</span></li>
        <li><span className="text-black">(301) 385 - 8537</span></li>
      </ul>
    </div>
  </aside>
}

const HeaderLink = ({to, children}) =>
  <NavLink
    exact
    onClick={() => toggle({payload: false})}
    to={(l) => ({...l, pathname: to, state: l.pathname})}
    className="hover:text-pri font-pri"
    activeClassName="text-gray"
  >{children}</NavLink>

const Header = (props) => {
  const location = useLocation()
  const history = useHistory()
  return <header
    className={`flex items-center justify-between h-16 w-full px-4 top-0 z-20
      ${location.pathname === "/" ?
      "absolute text-pri"
      : "relative bg-pri-2 text-black border-b-2"
      }
    `}
    /* style={{boxShadow: "0px 2px 2px 2px rgb(0 0 0 / 10%), 0px 25px 25px 25px rgb(0 0 0 / 5%)"}} */
  >
    <Link to="/" className={`
      ${location.pathname === "/" ?
      "text-pri"
      : "text-black hover:text-pri"}
      `}>
      <div className="flex items-center">
        <Logo className="h-6" />
        <h1 className={`text-3xl font-pri
          ${location.pathname === "/" ?  "text-pri" : "text-black"}
        `}>Paw Pals</h1>
      </div>
    </Link>
    <span
      className={`
        ${location.pathname === "/" ? "text-white" : "text-black"}
      `}
    >{props.user === null
      ? null
        :  window.innerWidth < 400
          ? ""
          :  window.innerWidth < 786 && props.user.length > 20
        ? props.user.slice(0,8) + "..." + props.user.slice(-10)
        : props.user }</span>
    <div className="cursor-pointer right-0 mr-4 fill-current">
      <div className="flex hidden sm:block space-x-6">
        {props.user ?
          <button
            className={props.form === "login" ? "text-gray" : ""}
            onClick={() => {
              toggle({payload: false})
              dispatch({filter: "root", type: "logout", history: history})
            }}
          >Logout</button>
          : <>
            <NavLink
              to={(l) => ({...l, pathname: "/login", state: l.pathname})}
              activeClassName="text-gray">Login</NavLink>
            <NavLink
              to={(l) => ({...l, pathname: "/register", state: l.pathname})}
              activeClassName="text-gray">Register</NavLink>
          </>
        }
        <Link to="/calendar">Calendar</Link>
      </div>
    </div>
  </header>
}
