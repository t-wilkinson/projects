import React from "react"
import {Link} from "react-router-dom"
import "./index.css"

type Time = {
  minute: number
  hour: number
  ispm: boolean
}

export const makeRefs      = (refs, def=null) => refs.reduce((acc,v) => ({...acc, [v]: React.useRef(def)}), {})
export const get           = (o, props, def=null) => props.reduce((xs, x) => (xs && xs[x]) ? xs[x] : def, o)
export const isAdmin       = (email) => email === "admin@gmail.com"
export const upperChar     = (s:string, n=0) => s.slice(0,n) + s.charAt(n).toUpperCase() + s.slice(n+1)
export const spaceCase     = (s:string) => s.replace(/([a-z0-9])([A-Z])/g, "$1 $2")
export const parseCents    = (money) => (parseInt(money)/100).toFixed(2)
export const validateCents = (parsedCents: string) => Number.parseFloat(parsedCents) * 100 > 50
export const padInt        = (n, size=2, char="0") => typeof(n) !== "number" ? n : n.toString().padStart(size, char)

export const toyyyymmdd     = (date) => `${padInt(date.year)}-${padInt(date.month+1)}-${padInt(date.day)}`

export const dateToClock    = (date: Date) => date.toLocaleTimeString('en-US').replace(/:.. /, " ").replace(/EDT/, "")
export const dateToSeconds  = (date:Date) => (date.getHours() * 60 + date.getMinutes()) * 60

export const timeToClock    = (time:Time) => padInt(time.hour) + ":" + padInt(time.minute) + " " + (time.ispm ? "PM" : "AM")
export const timeToSeconds  = (time:Time) => timeToMinutes(time) * 60
export const timeDifference = (start:Time, end:Time) => timeToSeconds(end) - timeToSeconds(start)

const timeToMinutes  = (time:Time) => (hours12To24(time.hour, time.ispm) * 60) + time.minute
const hours12To24    = (hours, ispm) => hours + (ispm && hours !== 12 ? 12 : 0)
const dateToUTC      = (date:Date):Date => new Date(date.getTime() + date.getTimezoneOffset() * 60 * 1000);
const dateToISO      = (date:Date) => date.toISOString().split(/T/)[0]
const dateFromUTC    = (utc:Date):Date => new Date(utc.getTime() - utc.getTimezoneOffset() * 60 * 1000)

export function rotateNum(n, i, max, min=0) {
  let res = n + (i || 0)
  return res > max ? min : res < min ? max : res
}

/* Time */
export function isPast(date) {
  let curDate = new Date()
  let calDate = new Date()
  calDate.setFullYear(date.year)
  calDate.setMonth(date.month)
  calDate.setDate(date.day)
  return calDate < curDate
}


export function monthYear(month, year) {
  let date = new Date(year, month).toLocaleString("default", {month: "long", year: "numeric"})
  if (/EDT/.test(date)) {
    return `${month}/${year}`
  } else {
    return date
  }
}

export function ymdToUTC(
  date: string, start: Time, end: Time
): {date: string, start: number, end: number} {
  if (!date.match(/\d{4}-\d{2}-\d{2}/)) throw new Error("Not valid date.")
  let [year, month, day] = date.split(/-/).map(v=>parseInt(v))
  month = month-1
  let startDate = dateToUTC(new Date( year, month, day, hours12To24(start.hour, start.ispm), start.minute))
  let endDate = dateToUTC(new Date( year, month, day, hours12To24(end.hour, end.ispm), end.minute))
  let utcDate = dateToUTC(new Date(year, month, day))
  return {
    date: dateToISO(utcDate),
    start: dateToSeconds(startDate),
    end: dateToSeconds(endDate),
  }
}

export function getServiceDate(
  dateString: string, startSeconds: number, endSeconds: number
): {date: string, start: Date, end: Date} {
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

export function dateToTime(date: Date): Time {
  return {
    minute: Math.floor(date.getMinutes() / 5) * 5,
    hour: date.getHours() % 12 || 12,
    ispm: date.getHours() >= 12,
  }
}


/* Components */
export function SocialMedia(props) {
  return <>
    <Icon {...props} d="M12 2.163c3.204 0 3.584.012 4.85.07 3.252.148 4.771 1.691 4.919 4.919.058 1.265.069 1.645.069 4.849 0 3.205-.012 3.584-.069 4.849-.149 3.225-1.664 4.771-4.919 4.919-1.266.058-1.644.07-4.85.07-3.204 0-3.584-.012-4.849-.07-3.26-.149-4.771-1.699-4.919-4.92-.058-1.265-.07-1.644-.07-4.849 0-3.204.013-3.583.07-4.849.149-3.227 1.664-4.771 4.919-4.919 1.266-.057 1.645-.069 4.849-.069zm0-2.163c-3.259 0-3.667.014-4.947.072-4.358.2-6.78 2.618-6.98 6.98-.059 1.281-.073 1.689-.073 4.948 0 3.259.014 3.668.072 4.948.2 4.358 2.618 6.78 6.98 6.98 1.281.058 1.689.072 4.948.072 3.259 0 3.668-.014 4.948-.072 4.354-.2 6.782-2.618 6.979-6.98.059-1.28.073-1.689.073-4.948 0-3.259-.014-3.667-.072-4.947-.196-4.354-2.617-6.78-6.979-6.98-1.281-.059-1.69-.073-4.949-.073zm0 5.838c-3.403 0-6.162 2.759-6.162 6.162s2.759 6.163 6.162 6.163 6.162-2.759 6.162-6.163c0-3.403-2.759-6.162-6.162-6.162zm0 10.162c-2.209 0-4-1.79-4-4 0-2.209 1.791-4 4-4s4 1.791 4 4c0 2.21-1.791 4-4 4zm6.406-11.845c-.796 0-1.441.645-1.441 1.44s.645 1.44 1.441 1.44c.795 0 1.439-.645 1.439-1.44s-.644-1.44-1.439-1.44z" />
    <Icon {...props} d="M19 0h-14c-2.761 0-5 2.239-5 5v14c0 2.761 2.239 5 5 5h14c2.762 0 5-2.239 5-5v-14c0-2.761-2.238-5-5-5zm-3 7h-1.924c-.615 0-1.076.252-1.076.889v1.111h3l-.238 3h-2.762v8h-3v-8h-2v-3h2v-1.923c0-2.022 1.064-3.077 3.461-3.077h2.539v3z" />
  </>
}

function Icon({className, ...props}) {
  return <li className={`
    list-none mt-4 inline-block cursor-pointer fill-current logo
    hover:text-pri
    ${className}
    `}
  >
    <svg viewBox="0 0 24 24"><path {...props} /></svg>
  </li>
}

export function Price ({children, ...props}) {
  return <strong {...props}>{children}</strong>
}

export function X(props) {
  return <svg
    viewBox="-170 -170 320 320"
    {...props}
  >
    <path d="m 0,0 104,104 35,-35 -104,-104 104,-104 -35,-35 -104,104 -104,-104 -35,35 104,104 -104,104 35,35 z" />
  </svg>
}

export function Arrow({style={}, className="", ...props}) {
  return <svg
    className={`inline-block origin-center transform stroke-current text-pri
      ${className}
      `}
    viewBox="-40 -10 70 65"
    style={style}
    {...props}
  >
    <path
      className="fill-current"
      style={{stroke: "black", strokeWidth: "14px", strokeLinecap: "round", strokeLinejoin: "round"}}
      d="m 0,0 0,0 -23,23 23,23 -0,0 -23,-23 0,-0 -0,-0 z"
    />
  </svg>
}

export function Logo(props) {
  return <svg
    preserveAspectRatio="xMidYMid slice"
    viewBox="-75 -25 77 75"
    {...props}
  >
    <path className="fill-current" d="m 0,0 a 7.7002913,7.7002913 0 0 0 -8.6925,-3.3634 7.7002913,7.7002913 0 0 0 -5.2646,9.53311 7.7002913,7.7002913 0 0 0 9.5328,5.26436 7.7002913,7.7002913 0 0 0 5.2645,-9.53281 7.7002913,7.7002913 0 0 0 -0.8401,-1.90126 z m -18.9725,-17.66878 a 7.7002913,7.7002913 0 0 0 -8.0813,-3.21038 7.7002913,7.7002913 0 0 0 -5.8175,9.20588 7.7002913,7.7002913 0 0 0 9.2063,5.81735 7.7002913,7.7002913 0 0 0 5.8166,-9.20637 7.7002913,7.7002913 0 0 0 -1.1242,-2.60652 z m -1.8456,25.68115 a 19.762043,19.762043 0 0 0 -21.8429,-8.55251 19.762043,19.762043 0 0 0 -13.9644,24.20894 19.762043,19.762043 0 0 0 24.2085,13.96397 19.762043,19.762043 0 0 0 13.9649,-24.20898 19.762043,19.762043 0 0 0 -2.3661,-5.41142 z m -21.204,-28.20026 a 7.7002913,7.7002913 0 0 0 -8.0212,-2.84035 7.7002913,7.7002913 0 0 0 -5.6021,9.33806 7.7002913,7.7002913 0 0 0 9.3375,5.60234 7.7002913,7.7002913 0 0 0 5.6025,-9.33849 7.7002913,7.7002913 0 0 0 -1.3167,-2.76156 z m -18.4394,18.09941 a 7.7002913,7.7002913 0 0 0 -9.9963,-2.61064 7.7002913,7.7002913 0 0 0 -3.2754,10.38544 7.7002913,7.7002913 0 0 0 10.3857,3.27463 7.7002913,7.7002913 0 0 0 3.2745,-10.38571 7.7002913,7.7002913 0 0 0 -0.3884,-0.66384 z" />
  </svg>
}

export function A({to, children=null, className="", ...props}) {
  return <Link
    to={to}
    className={`font-pri underline ${className}`}
    {...props}
  >{children} </Link>
}

/* Forms */
export function Button({children=null, className="", ...props}) {
  return <button
    type="button"
    className={`px-4 py-2 border-black border-2 rounded-md ${className}`}
    {...props}
  >
    {children}
  </button>
}

export function Required({label, required}) {
  return <span>{label}{" "}
    <span className="text-xs">{required ? "*" : ""}</span>
  </span>
}

export function Checkbox(props) {
  return <label className="checkbox">
    <input type="checkbox" {...props} />
  </label>
}

export function Input({validate, state, dispatch, field}) {
  let f = state.form[field]
  return <label className="flex flex-col w-full relative my-4">
    <Required label={f.label} required={f.required} />
    <input
      name={field}
      value={f.value}
      placeholder={f.placeHolder}
      type={f.visible ? "text" : "password"}
      className={`relative border-b-2 py-1 px-2 focus:border-pri w-full outline-none
        ${f.errors.length === 0 ? "border-gray-3" : "border-err"}
        `}
      onChange={(e) => dispatch({type: "update-field", target: e.target})}
      onBlur={(e) => validate(state, dispatch, e.target)}
    />
    {f.type !== "password" ? null :
    <FormEye
      visible={f.visible}
      toggleVisibility={(e) => dispatch({type: "toggle-visibility", target: {name: field}})}
    />
    }
    <ul className="text-err">{f.errors.map(e=><li key={e}>{e}</li>)}</ul>
  </label>
}

function FormEye ({visible, toggleVisibility}) {
  return <svg
    onClick={toggleVisibility}
    className="absolute top-0 right-0 h-8 w-8 mt-6 mr-1 cursor-pointer"
    viewBox="2170 1350 800 800"
  >
    {visible ?
      <>
        <path style={{stroke: "black", strokeWidth: "30", fill: "none"}} d="m 2574.5273,1559.0176 c -232.2327,-1e-4 -305.9512,155.8886 -305.9512,155.8886 0,0 72.6316,155.8887 305.9512,155.8887 233.3196,0 305.9492,-155.8887 305.9492,-155.8887 0,0 -73.7165,-155.8887 -305.9492,-155.8886 z" />,
        <circle r="150" cx="2574" cy="1705" style={{stroke: "black", strokeWidth: "30", fill: "none"}} />
      </> : <>
        <path style={{stroke: "black", strokeLinecap: "round", strokeWidth: "30"}} d="m 2767.3354,1466.7238 -369.6236,491.5473" />,
        <path d="m 2574.5273,1559.0176 c -119.2523,-10e-5 -199.312,40.3666 -248.9414,81.1211 -49.6327,40.7575 -69.6523,82.7675 -69.6523,82.7675 a 3.9911375,3.9911375 0 0 0 -0,0.01 c -1.7501,3.7001 -1.7631,8.1801 -0.035,11.8906 a 3.9911375,3.9911375 0 0 0 0,0.01 c 0,0 19.7535,42.051 69.2657,82.8262 25.6157,21.0956 59.4218,42.0807 102.666,57.4238 a 3.9911375,3.9911375 0 0 0 4.5234,-1.3633 l 12.5625,-16.707 a 3.9911375,3.9911375 0 0 0 -1.9238,-6.1836 c -42.8722,-14.3394 -75.5485,-34.5932 -100.041,-54.7637 -35.8677,-29.5383 -52.6909,-56.7213 -58.4082,-67.1054 5.7805,-10.3792 22.7901,-37.6007 58.8008,-67.1719 27.7544,-22.7915 65.9909,-45.6186 117.3281,-59.9629 -35.5186,31.1272 -57.9922,76.5524 -57.9922,127.0957 0,44.5437 17.4572,85.1339 45.9062,115.4336 a 3.9911375,3.9911375 0 0 0 6.0996,-0.334 l 16.418,-21.8359 a 3.9911375,3.9911375 0 0 0 -0.1894,-5.0293 c -20.734,-23.6499 -33.2344,-54.46 -33.2344,-88.2383 0,-74.5847 60.9293,-134.8809 136.8476,-134.8809 19.8389,0 38.6354,4.132 55.6055,11.5528 a 3.9911375,3.9911375 0 0 0 4.7891,-1.2579 l 24.1133,-32.0664 a 3.9911375,3.9911375 0 0 0 -2.4981,-6.3281 c -24.9931,-4.393 -52.285,-6.8984 -82.0098,-6.8984 z m 145.2891,23.6113 a 3.9911375,3.9911375 0 0 0 -3.2109,1.5918 l -12.5625,16.7051 a 3.9911375,3.9911375 0 0 0 1.9179,6.1816 c 42.633,14.3317 75.2439,34.532 99.75,54.6563 36.0103,29.5711 53.0206,56.7929 58.8008,67.1718 -5.7166,10.3835 -22.5405,37.5668 -58.4082,67.1055 -27.7801,22.8783 -66.184,45.8001 -117.9648,60.1328 35.3031,-30.852 57.762,-75.8176 58.1679,-125.9101 0.036,-0.4529 0.068,-0.9051 0.068,-1.3594 a 3.9911375,3.9911375 0 0 0 0,-0.014 c -7e-4,-0.4617 -0.033,-0.9225 -0.07,-1.3828 l -0,-0.084 c -0.3838,-43.9507 -17.7439,-83.9734 -45.8496,-113.9394 a 3.9911375,3.9911375 0 0 0 -6.0996,0.332 l -16.4024,21.8125 a 3.9911375,3.9911375 0 0 0 0.1895,5.0293 c 20.7352,23.6507 33.2363,54.4589 33.2363,88.2383 0,74.5846 -60.9291,134.8789 -136.8477,134.8789 -19.8397,0 -38.6371,-4.13 -55.6074,-11.5508 a 3.9911375,3.9911375 0 0 0 -4.789,1.2578 l -24.1485,32.1153 a 3.9911375,3.9911375 0 0 0 2.5039,6.33 c 24.9935,4.3638 52.292,6.8477 82.041,6.8477 119.7816,0 199.8571,-40.3645 249.3653,-81.1367 49.5117,-40.7753 69.2617,-82.8262 69.2617,-82.8262 a 3.9911375,3.9911375 0 0 0 0,-0.01 c 1.7288,-3.7101 1.716,-8.1882 -0.033,-11.8887 a 3.9911375,3.9911375 0 0 0 -0,-0.01 c 0,0 -20.02,-42.0099 -69.6523,-82.7675 -25.6073,-21.0285 -59.323,-41.949 -102.334,-57.2793 a 3.9911375,3.9911375 0 0 0 -1.3184,-0.2305 z" /> [
      </>
    }
  </svg>
}

export function createForm(fields) {
  let form = {}
  for (let [field,values] of Object.entries(fields)) {
    let {
      label=null,
        type="input",
        placeHolder="",
        required=true,
    } = values as any

      form[field] =
        { field: field,
          errors: [],
          value: "",
          placeHolder: placeHolder,
          required: required,
          type: field === "password" ? "password" : type,
          visible: field === "password" || type === "password" ? false : true,
          label: label ?? upperChar(spaceCase(field)),
      }
  }
  return form
}


/* Museum */
/* export function getCookies() { */
/*   let cookies = {} */
/*   document.cookie.split(';').forEach((kv) => { */
/*     let [k, v] = kv.split('=') */
/*     cookies[k.trim()] = v */
/*   }) */
/*   return cookies */
/* } */
