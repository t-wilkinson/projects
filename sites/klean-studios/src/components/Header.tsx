// @ts-nocheck
import React from 'react'
import gsap from 'gsap'
import { useHistory } from 'react-router-dom'
import { isAdmin, Logo } from './'
import { dispatch, useStore, actions } from '../store'
import { A, social } from './'

const {
  header: { updateHeader },
} = actions

export const Header = (props) => {
  const state = useStore((s) => s.header)
  const root = useStore((s) => s.root)
  const history = useHistory()
  const refs = {
    aside: React.useRef(null),
    menu: React.useRef(null),
    menuParent: React.useRef(null),
    links: React.useRef(null),
    contact: React.useRef(null),
    logo: React.useRef(null),
  }
  const [tl, setTl] = React.useState(null)

  React.useEffect(() => {
    if (Object.values(refs).some((v) => v.current === null)) return
    let r: any = {}
    for (const [k, v] of Object.entries(refs)) r[k] = v.current
    let timeline = gsap
      .timeline({ defaults: { ease: 'power3.inOut' }, paused: true })
      .fromTo(
        r.aside,
        { display: 'none' },
        { display: 'flex', duration: 0.01 },
        0
      )
      .fromTo(
        r.aside,
        { opacity: 0 },
        { opacity: 1, duration: 1, ease: 'power3.out' },
        0
      )
      .fromTo(
        r.menuParent.children[0],
        { height: '1.25rem' },
        { height: '2.5rem' },
        0
      )
      /* .fromTo(r.menu.children[1], {width: "2rem"}, {width: 0}, 0.0) */
      .fromTo(
        r.menu.children[0],
        { rotate: '0', y: 0 },
        { rotate: '45deg', y: 2.5 },
        0.1
      )
      .fromTo(
        r.menu.children[1],
        { rotate: '0', y: 0 },
        { rotate: '-45deg', y: -2.5 },
        0.1
      )
      .fromTo(
        r.links.children,
        {
          opacity: 0,
          x: 80,
          ease: 'power3.out',
          stagger: { from: 'end', each: 0.08 },
        },
        {
          opacity: 1,
          x: 0,
          ease: 'power1.out',
          stagger: 0.1,
        },
        0.5
      )
      .fromTo(
        r.contact.children,
        {
          y: 20,
          opacity: 0,
        },
        {
          y: 0,
          opacity: 1,
          stagger: 0.04,
        },
        0.8
      )
    setTl(timeline)

    // eslint-disable-next-line
  }, [root.user, root.userAgent])

  React.useEffect(() => {
    if (tl === null) return
    else if (state.active) tl.play()
    else tl.reverse()
  }, [tl, state.active])

  return (
    <header className="relative z-30">
      <aside
        ref={refs.aside}
        className="fixed w-screen opacity-0
    p-8 inset-y-0 right-0 bg-sec text-center text-2xl space-y-6 z-30 text-4xl
    col justify-between"
      >
        <A to="/" onClick={() => updateHeader({ active: false })}>
          <div ref={refs.logo} className="flex flex-col items-center">
            <Logo
              alt="logo"
              height="60px"
              fill="#000"
              style={{
                ...(window.innerWidth > 700 && window.innerHeight < 400
                  ? { position: 'absolute', left: '0' }
                  : {}),
              }}
            />
          </div>
        </A>
        <ul
          ref={refs.links}
          className="space-y-3 w-full h-full col justify-center
      max-w-screen-sm"
        >
          {Links(root, history)}
        </ul>
        <div
          ref={refs.contact}
          className={`flex flex-col items-start w-full px-4
        ${window.innerHeight < 900 ? 'text-base' : 'text-lg '}
      `}
          style={{
            ...(window.innerWidth > 700 && window.innerHeight < 400
              ? { position: 'absolute', left: '0', bottom: '0' }
              : {}),
          }}
        >
          <span>(540) 816-2061</span>
          <span>studio.klean@gmail.com</span>
          <span>Roanoke - Staunton VA</span>
          <nav className="flex space-x-2">
            <span>follow me on</span>
            {/* <Href target="_blank" to="https://twitter.com/mtklim20">twitter</Href> */}
            <social.Spotify />
            <social.Instagram />
          </nav>
        </div>
      </aside>
      <div
        ref={refs.menuParent}
        className="col justify-center fixed h-screen right-0 z-30 mr-3"
      >
        <button
          className="w-10 h-6 text-md bg-pri py-0
      flex items-center cursor-pointer relative z-40
      "
          onClick={() => {
            updateHeader()
          }}
        >
          <div className="absolute bg-pri inset-0" />
          <ul
            ref={refs.menu}
            className="w-full h-2 col justify-around outline-none"
          >
            <li className="h-2px w-8 rounded-sm bg-sec"></li>
            <li className="h-2px w-8 rounded-sm bg-sec"></li>
          </ul>
        </button>
      </div>
    </header>
  )
}

const Links = (root, history) =>
  [
    { to: '/', txt: 'home' },
    { to: '/dashboard', txt: 'dashboard' },
    { to: '/projects', txt: 'projects' },
    ...(root.user
      ? [
        {
          to: '/',
          txt: 'logout',
          onClick() {
            dispatch({ filter: 'root', type: 'logout', history: history })
          },
        },
      ]
      : [
        { to: '/login', txt: 'login' },
        { to: '/register', txt: 'register' },
      ]),
    isAdmin(root.user) ? { to: '/admin', txt: 'admin' } : {},
  ].map(({ to, txt, onClick = () => {} }) => (
    <A
      key={txt}
      to={to}
      onClick={() => {
        updateHeader({ active: false })
        onClick()
      }}
      className="w-full no-underline"
    >
      <li
        className={`
        ${window.innerHeight < 900 ? 'text-2xl' : 'text-5xl md:text-6xl'}
        ${history.location.pathname === to ? 'text-black' : ''}
        block w-full font-pri no-underline
        transform hover:-skew-y-3 hover:-skew-x-12 hover:text-black`}
      >
        {txt}
      </li>
    </A>
  ))

export default Header
