import React from 'react'
import { Link } from 'react-router-dom'
import gsap from 'gsap'
import './index.css'

type Time = {
  hour: number
  minute: number
  ispm: boolean
}

export const reload = () => window.location.reload()
export const isEmpty = (obj) => Object.keys(obj).length === 0
export const upperChar = (s: string, n = 0) =>
  s.slice(0, n) + s.charAt(n).toUpperCase() + s.slice(n + 1)
export const spaceCase = (s: string) => s.replace(/([a-z0-9])([A-Z])/g, '$1 $2')
export const parseCents = (money) => (parseInt(money) / 100).toFixed(2)
export const validateCents = (parsedCents: string) =>
  Number.parseFloat(parsedCents) * 100 > 50
export const get = (o, props, def = null) =>
  props.reduce((xs, x) => (xs && xs[x] ? xs[x] : def), o)
export const padInt = (n, size = 2, char = '0') =>
  typeof n !== 'number' ? n : n.toString().padStart(size, char)
export const hours12To24 = (hours, ispm) =>
  hours + (ispm && hours !== 12 ? 12 : 0)
export const toyyyymmdd = (date) =>
  `${padInt(date.year)}-${padInt(date.month + 1)}-${padInt(date.day)}`
export const dateToSeconds = (date: Date) =>
  (date.getHours() * 60 + date.getMinutes()) * 60
export const dateToISO = (date: Date) => date.toISOString().split(/T/)[0]
export const dateToUTC = (date: Date) =>
  new Date(date.getTime() + date.getTimezoneOffset() * 60 * 1000)
export const dateFromUTC = (utc: Date) =>
  new Date(utc.getTime() - utc.getTimezoneOffset() * 60 * 1000)
export const dateToClock = (date: Date) =>
  date.toLocaleTimeString('en-US').replace(/:.. /, ' ').replace(/EDT/, '')
export const timeToClock = (time: Time) =>
  padInt(time.hour) +
  ':' +
  padInt(time.minute) +
  ' ' +
  (time.ispm ? 'PM' : 'AM')
export const timeToSeconds = (time: Time) => timeToMinutes(time) * 60
export const timeDifference = (start: Time, end: Time) =>
  timeToSeconds(end) - timeToSeconds(start)
export const isAdmin = (email) => email === 'admin@kleanstudio.com'
const timeToMinutes = (time: Time) =>
  hours12To24(time.hour, time.ispm) * 60 + time.minute

export const monthYear = (month, year) => {
  let date = new Date(year, month).toLocaleString('default', {
    month: 'long',
    year: 'numeric',
  })
  if (/EDT/.test(date)) {
    return `${month}/${year}`
  } else {
    return date
  }
}

export const isFuture = (date) => {
  let curDate = new Date()
  let calDate = new Date()
  calDate.setFullYear(date.year)
  calDate.setMonth(date.month)
  calDate.setDate(date.day)
  return calDate.getTime() >= curDate.getTime()
}

export function rotateNum(n, i, max, min = 0) {
  let res = n + (i || 0)
  return res > max ? min : res < min ? max : res
}

export function getCookies(): any {
  return document.cookie.split(';').reduce((cs, kv) => {
    let [k, v] = kv.split('=')
    cs[k.trim()] = v
    return cs
  }, {})
}

export function dateToTime(date: Date): Time {
  return {
    minute: Math.floor(date.getMinutes() / 5) * 5,
    hour: date.getHours() % 12 || 12,
    ispm: date.getHours() >= 12,
  }
}

export function getServiceDate(
  dateString: string,
  startSeconds: number,
  endSeconds: number
): { date: string; start: Date; end: Date } {
  let date = new Date(dateString)

  let utc = dateToUTC(date)
  let start = new Date(utc.getTime())
  start.setSeconds(startSeconds)
  let end = new Date(utc.getTime())
  end.setSeconds(endSeconds)

  return {
    date: dateToISO(date),
    start: dateFromUTC(start),
    end: dateFromUTC(end),
  }
}

export function ymdToUTC(
  date: string,
  start: Time,
  end: Time
): { date: string; start: number; end: number } {
  if (!date.match(/\d{4}-\d{2}-\d{2}/)) throw new Error('Not valid date.')
  let [year, month, day] = date.split(/-/).map((v) => parseInt(v))
  month = month - 1
  let startDate = dateToUTC(
    new Date(
      year,
      month,
      day,
      hours12To24(start.hour, start.ispm),
      start.minute
    )
  )
  let endDate = dateToUTC(
    new Date(year, month, day, hours12To24(end.hour, end.ispm), end.minute)
  )
  // use startDate in case date rolls over
  let utcDate = dateToUTC(new Date(year, month, startDate.getDate()))
  return {
    date: dateToISO(utcDate),
    start: dateToSeconds(startDate),
    end: dateToSeconds(endDate),
  }
}

export function basicAuth(root) {
  if (root.session) {
    let auth = btoa(`${root.user}:${root.session}`)
    return `Basic ${auth}`
  } else {
    return null
  }
}

/*
Components
*/

export const AnimPlane = ({ className, ...props }) => (
  <div className={`sk-plane ${className}`} {...props} />
)

export const Strong = (props) => (
  <strong style={{ fontFamily: '\'Abril Fatface\'' }} {...props} />
)

export const Logo = (props) => (
  <svg id="header-logo" viewBox="0 0 120 110" {...props}>
    <g transform="translate(-643.09811,-542.28051)">
      <g transform="matrix(2.2042757,0,0,2.2042757,-1033.6644,2091.3051)">
        <path d="m 789.42802,-702.73578 a 1.033648,1.033648 0 0 0 -0.0196,0 1.033648,1.033648 0 0 0 -0.77152,0.37312 c -3.99627,4.82539 -10.00967,12.01623 -15.0146,14.29731 -1.25124,0.57026 -2.41142,0.83307 -3.44215,0.75653 -1.03077,-0.0765 -1.95892,-0.46109 -2.88616,-1.34927 -1.85441,-1.77634 -3.58621,-5.74207 -4.55062,-12.82298 a 1.033648,1.033648 0 0 0 -2.05515,0.20722 c 0.54348,8.28707 1.55866,14.15328 3.12021,17.99426 0.78078,1.92048 1.70251,3.35539 2.86546,4.29276 1.16295,0.93736 2.60146,1.30439 4.0323,1.06193 2.86171,-0.4849 5.69505,-2.9699 8.91781,-6.93134 3.22278,-3.96148 6.789,-9.48809 10.70581,-16.3339 a 1.033648,1.033648 0 0 0 -0.90175,-1.54617 z m 5.11236,0.55502 v 0 c -1.23822,0.003 -2.37979,0.60137 -3.00707,1.72598 -0.64749,1.16089 -0.84553,2.7646 -0.68109,4.91855 0.32885,4.30792 2.17792,10.90168 6.06629,20.53725 a 0.76813483,0.76813483 0 0 0 1.45986,-0.46405 c -1.80996,-7.66583 -2.3545,-12.60137 -2.03192,-15.39698 0.16129,-1.39782 0.53589,-2.22171 0.94777,-2.63758 0.41185,-0.41584 0.88786,-0.54978 1.6588,-0.45527 1.54186,0.18905 3.97809,1.57152 6.66057,3.49488 2.68248,1.92337 5.65968,4.35161 8.59586,6.52984 a 0.76813483,0.76813483 0 0 0 1.05627,-1.09812 c -3.82278,-4.75891 -7.77938,-8.97361 -11.35123,-12.03699 -3.57188,-3.06339 -6.68592,-5.04383 -9.25423,-5.11598 -0.0401,0 -0.0799,-0.003 -0.11988,-0.003 z m 7.53494,15.44505 a 0.63675045,0.63675045 0 0 0 -0.62013,0.76121 c 1.50005,7.49138 2.51346,12.72278 2.90266,16.12201 0.19463,1.6996 0.22738,2.94468 0.11266,3.73309 -0.0574,0.3942 -0.15219,0.66841 -0.24598,0.82838 -0.0938,0.15997 -0.16436,0.21074 -0.26922,0.24958 -0.20976,0.0777 -0.7458,0.0294 -1.57202,-0.35189 -0.82619,-0.38138 -1.90399,-1.04278 -3.21064,-1.9172 -2.61331,-1.7489 -6.1545,-4.34491 -10.73009,-7.28792 a 0.63675045,0.63675045 0 0 0 -0.87075,0.89347 c 3.80365,5.58781 7.73764,10.00418 11.24945,12.91341 1.75588,1.45463 3.40548,2.53394 4.90564,3.19207 1.50013,0.65815 2.86983,0.91215 4.054,0.586 1.18419,-0.32613 2.08177,-1.28662 2.50425,-2.67994 0.42246,-1.39332 0.45024,-3.23334 0.078,-5.6043 -0.74446,-4.74192 -3.12362,-11.62595 -7.71115,-21.07933 a 0.63675045,0.63675045 0 0 0 -0.57671,-0.35864 z m -31.25237,12.95736 c -2.63266,0.0306 -4.84082,1.64224 -6.35826,4.57078 -1.61861,3.12375 -2.66266,7.7687 -3.30832,14.14849 a 0.86531089,0.86531089 0 0 0 1.6924,0.32554 c 1.5648,-5.46888 3.52034,-8.59858 5.58726,-10.13063 2.0669,-1.53204 4.29874,-1.62779 6.91687,-0.82269 5.23629,1.6102 11.54671,7.09276 17.68213,11.32591 a 0.86531089,0.86531089 0 0 0 1.13843,-1.28569 c -4.97266,-5.60925 -9.28095,-10.00691 -13.02658,-13.07415 -3.74563,-3.06723 -6.92438,-4.84918 -9.79165,-5.04259 -0.17922,-0.0122 -0.35676,-0.0172 -0.53228,-0.0151 z" />
      </g>
    </g>
  </svg>
)

export const Menu = ({
  active = false,
  duration = 0.5,
  bg = '#20c9c6',
  onClick = (e) => {},
  ...props
}) => {
  let [state] = React.useState({ tl: gsap.timeline() })
  let ref = React.useRef(null)
  React.useEffect(() => {
    state.tl.add(
      gsap
        .timeline({ defaults: { duration: duration, ease: 'power.out' } })
        .fromTo(
          ref.current.children,
          { background: bg },
          { background: '#000' },
          0
        )
        .fromTo(
          ref.current.children[0],
          { rotate: '0', y: 0 },
          { rotate: '45deg', y: 12.5 },
          0
        )
        .fromTo(ref.current.children[1], { opacity: 1 }, { opacity: 0 }, 0)
        .fromTo(
          ref.current.children[2],
          { rotate: '0', y: 0 },
          { rotate: '-45deg', y: -12.5 },
          0
        )
    )
    state.tl.pause()
  }, [state.tl, bg, duration])

  React.useEffect(() => {
    active ? state.tl.play() : state.tl.reverse()
  }, [active, state.tl])

  return (
    <ul
      {...props}
      ref={ref}
      onClick={(e) => {
        onClick(e)
      }}
      className={'space-y-2 cursor-pointer ' + props.className}
      tabIndex={-1}
    >
      <li className="h-px w-10 rounded-sm bg-sec"></li>
      <li className="h-px w-10 rounded-sm bg-sec"></li>
      <li className="h-px w-10 rounded-sm bg-sec"></li>
    </ul>
  )
}

export const BlurryImg = ({ className, src, alt, ...props }) => {
  return (
    <img
      className={`blurry-load ${className}`}
      src={'/assets/images/' + src}
      data-large={'/assets/images/' + src}
      alt={alt}
      {...props}
    />
  )
}

export const X = (props) => {
  return (
    <svg viewBox="-170 -170 320 320" {...props}>
      <path d="m 0,0 104,104 35,-35 -104,-104 104,-104 -35,-35 -104,104 -104,-104 -35,35 104,104 -104,104 35,35 z" />
    </svg>
  )
}

export const A = ({ to, children, className = '', ...props }) => (
  <Link to={to} className={`font-pri underline ${className}`} {...props}>
    {children}{' '}
  </Link>
)

export const Href = ({ to, children, className = '', ...props }) => (
  <a
    href={to}
    className={`font-pri underline ${className}`}
    rel="noopener noreferrer"
    {...props}
  >
    {children}{' '}
  </a>
)

export const social = {
  Spotify: () => (
    <Href
      target="_blank"
      to="https://open.spotify.com/artist/79JLKjgnXraJkLfdDX2ILF?si=uxh_YlQ9S4eh4gy0WPaKUg&dl_branch=1"
    >
      spotify
    </Href>
  ),
  Instagram: () => (
    <Href target="_blank" to="https://www.instagram.com/klean_studios/">
      instagram
    </Href>
  ),
}

export const Arrow = ({ style = null, className = '', ...props }) => {
  return (
    <svg
      className={`inline-block origin-center transform stroke-current text-pri
      ${className}
      `}
      viewBox="-10 -5 120 80"
      style={style ?? {}}
      {...props}
    >
      <polygon
        points="0,0 100,0 50,70"
        className="fill-current"
        style={{ strokeWidth: '0' }}
      />
    </svg>
  )
}

/* Forms */
export const Button = ({ className = '', children = null, ...props }) => (
  <button
    type="button"
    className={`
    text-md bg-pri text-white border border-pri py-3 px-5
    hover:bg-white hover:text-black ${className}`}
    {...props}
  >
    {children}
  </button>
)

export const Required = ({ label, required }) => (
  <span>
    {label} <span className="text-xs">{required ? '*' : ''}</span>
  </span>
)

export const Checkbox = (props) => (
  <label className="checkbox">
    <input type="checkbox" {...props} />
  </label>
)

export function Input({
  state,
  dispatch,
  field,
  onBlur = (a, b, c) => {},
  onChange = (a, b, c) => {},
}) {
  let f = state.form[field]
  return (
    <label className="flex flex-col w-full relative my-4">
      <Required label={f.label} required={f.required} />
      <input
        name={field}
        value={f.value}
        placeholder={f.placeHolder}
        type={f.visible ? 'text' : 'password'}
        className={`relative border-b-2 py-1 px-2 focus:border-pri w-full outline-none
          ${f.errors.length === 0 ? 'border-gray-3' : 'border-err'}
          `}
        onChange={(e) => {
          onChange(state, dispatch, e)
          dispatch({ type: 'update-field', target: e.target })
        }}
        onBlur={(e) => onBlur(state, dispatch, e.target)}
      />
      {f.type !== 'password' ? null : (
        <FormEye
          visible={f.visible}
          toggleVisibility={(e) =>
            dispatch({ type: 'toggle-visibility', target: { name: field } })
          }
        />
      )}
      <ul className="text-err">
        {f.errors.map((e) => (
          <li key={e}>{e}</li>
        ))}
      </ul>
    </label>
  )
}

const FormEye = ({ visible, toggleVisibility }) => (
  <svg
    onClick={toggleVisibility}
    className="absolute top-0 right-0 h-8 w-8 mt-6 mr-1 cursor-pointer"
    viewBox="2170 1350 800 800"
  >
    {visible ? (
      <>
        <path
          style={{ stroke: 'black', strokeWidth: '30', fill: 'none' }}
          d="m 2574.5273,1559.0176 c -232.2327,-1e-4 -305.9512,155.8886 -305.9512,155.8886 0,0 72.6316,155.8887 305.9512,155.8887 233.3196,0 305.9492,-155.8887 305.9492,-155.8887 0,0 -73.7165,-155.8887 -305.9492,-155.8886 z"
        />
        ,
        <circle
          r="150"
          cx="2574"
          cy="1705"
          style={{ stroke: 'black', strokeWidth: '30', fill: 'none' }}
        />
      </>
    ) : (
      <>
        <path
          style={{ stroke: 'black', strokeLinecap: 'round', strokeWidth: '30' }}
          d="m 2767.3354,1466.7238 -369.6236,491.5473"
        />
        ,
        <path d="m 2574.5273,1559.0176 c -119.2523,-10e-5 -199.312,40.3666 -248.9414,81.1211 -49.6327,40.7575 -69.6523,82.7675 -69.6523,82.7675 a 3.9911375,3.9911375 0 0 0 -0,0.01 c -1.7501,3.7001 -1.7631,8.1801 -0.035,11.8906 a 3.9911375,3.9911375 0 0 0 0,0.01 c 0,0 19.7535,42.051 69.2657,82.8262 25.6157,21.0956 59.4218,42.0807 102.666,57.4238 a 3.9911375,3.9911375 0 0 0 4.5234,-1.3633 l 12.5625,-16.707 a 3.9911375,3.9911375 0 0 0 -1.9238,-6.1836 c -42.8722,-14.3394 -75.5485,-34.5932 -100.041,-54.7637 -35.8677,-29.5383 -52.6909,-56.7213 -58.4082,-67.1054 5.7805,-10.3792 22.7901,-37.6007 58.8008,-67.1719 27.7544,-22.7915 65.9909,-45.6186 117.3281,-59.9629 -35.5186,31.1272 -57.9922,76.5524 -57.9922,127.0957 0,44.5437 17.4572,85.1339 45.9062,115.4336 a 3.9911375,3.9911375 0 0 0 6.0996,-0.334 l 16.418,-21.8359 a 3.9911375,3.9911375 0 0 0 -0.1894,-5.0293 c -20.734,-23.6499 -33.2344,-54.46 -33.2344,-88.2383 0,-74.5847 60.9293,-134.8809 136.8476,-134.8809 19.8389,0 38.6354,4.132 55.6055,11.5528 a 3.9911375,3.9911375 0 0 0 4.7891,-1.2579 l 24.1133,-32.0664 a 3.9911375,3.9911375 0 0 0 -2.4981,-6.3281 c -24.9931,-4.393 -52.285,-6.8984 -82.0098,-6.8984 z m 145.2891,23.6113 a 3.9911375,3.9911375 0 0 0 -3.2109,1.5918 l -12.5625,16.7051 a 3.9911375,3.9911375 0 0 0 1.9179,6.1816 c 42.633,14.3317 75.2439,34.532 99.75,54.6563 36.0103,29.5711 53.0206,56.7929 58.8008,67.1718 -5.7166,10.3835 -22.5405,37.5668 -58.4082,67.1055 -27.7801,22.8783 -66.184,45.8001 -117.9648,60.1328 35.3031,-30.852 57.762,-75.8176 58.1679,-125.9101 0.036,-0.4529 0.068,-0.9051 0.068,-1.3594 a 3.9911375,3.9911375 0 0 0 0,-0.014 c -7e-4,-0.4617 -0.033,-0.9225 -0.07,-1.3828 l -0,-0.084 c -0.3838,-43.9507 -17.7439,-83.9734 -45.8496,-113.9394 a 3.9911375,3.9911375 0 0 0 -6.0996,0.332 l -16.4024,21.8125 a 3.9911375,3.9911375 0 0 0 0.1895,5.0293 c 20.7352,23.6507 33.2363,54.4589 33.2363,88.2383 0,74.5846 -60.9291,134.8789 -136.8477,134.8789 -19.8397,0 -38.6371,-4.13 -55.6074,-11.5508 a 3.9911375,3.9911375 0 0 0 -4.789,1.2578 l -24.1485,32.1153 a 3.9911375,3.9911375 0 0 0 2.5039,6.33 c 24.9935,4.3638 52.292,6.8477 82.041,6.8477 119.7816,0 199.8571,-40.3645 249.3653,-81.1367 49.5117,-40.7753 69.2617,-82.8262 69.2617,-82.8262 a 3.9911375,3.9911375 0 0 0 0,-0.01 c 1.7288,-3.7101 1.716,-8.1882 -0.033,-11.8887 a 3.9911375,3.9911375 0 0 0 -0,-0.01 c 0,0 -20.02,-42.0099 -69.6523,-82.7675 -25.6073,-21.0285 -59.323,-41.949 -102.334,-57.2793 a 3.9911375,3.9911375 0 0 0 -1.3184,-0.2305 z" />{' '}
        [
      </>
    )}
  </svg>
)

export const createForm = (fields): any => {
  let form = {}
  for (let [field, values] of Object.entries(fields)) {
    let {
      label = null,
      type = 'input',
      placeHolder = '',
      required = true,
    } = values as any

    form[field] = {
      field: field,
      errors: [],
      value: '',
      placeHolder: placeHolder,
      required: required,
      type: field === 'password' ? 'password' : type,
      visible: field === 'password' || type === 'password' ? false : true,
      label: label ?? upperChar(spaceCase(field)),
    }
  }
  return form
}
