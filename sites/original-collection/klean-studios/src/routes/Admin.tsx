import React from "react"
import CurrencyInput from "react-currency-input-field"
import gsap from "gsap"
import produce, {enableES5} from "immer"
import {Subject} from "rxjs"
import * as ops from "rxjs/operators"
import {BrowserRouter as Router, Switch, Route} from "react-router-dom"

import {
  reload, upperChar, dateToClock, getServiceDate, padInt, createForm, get, monthYear, basicAuth, parseCents,
  Button, A, Logo, Input, Arrow, Checkbox,
} from "../components"
import "./Admin.css"
import * as api from "../api"
import {useStore} from "../store"

enableES5()

function callback(state) { state.runQuery.next() }

export default (props) => {
  const [valid, setValid] = React.useState(true)

  React.useEffect(() => {
    document.title = "Admin | Klean Studios"
  }, [])

  React.useEffect(() => {
    api.getAdminValidate(
      ()=>setValid(true),
      ()=>setValid(false),
    )
  }, [setValid])

  if (valid === true) {
    return Admin(props)
  } else if (valid === false) {
    return <span className="w-full block text-6xl font-pri">
      Not authorized
    </span>
  } else if (valid === null) {
    return <span>
      Loading...
    </span>
  }
}

function Admin(props) {
  return <Router>
    <header className="w-full bg-gray flex justify-between items-center">
      <div className="flex items-center p-4">
        <a href="/">
          <Logo className="w-12"/>
        </a>
        <span className="text-xl">&nbsp;admin</span>
      </div>
      <nav className="flex-end pr-8 space-x-4">
        <A rel="nofollow" to="/admin/services" className="no-underline px-4 py-2">Services</A>
        <A rel="nofollow" to="/admin/rates" className="no-underline px-4 py-2">Rates</A>
        <A rel="nofollow" to="/admin/projects" className="no-underline px-4 py-2">Projects</A>
      </nav>
    </header>
    <Switch>
      <Route path="/admin/services" component={AdminService} />
      <Route path="/admin/rates" component={AdminRate} />
      <Route path="/admin/projects" component={AdminProjects} />
      <Route path="/admin" component={AdminService} />
    </Switch>
  </Router>
}

function adminProjectsSubmit(project) {
  let body = {
    group: project.group,
    src: project.src,
    href: project.href,
    alt: project.alt,
    order: parseInt(project.order),
  }

  if (Object.values(body).some(v=>v === "")) return

  if (project.token === null) { // User creating a new project
    api.postAdminProjects(
      body,
      ()=>{reload()},
      ()=>{reload()},
    )

  } else {
  api.putAdminProjectsByProject(
    project.token, body,
    ()=>{reload()},
    ()=>{reload()},
  )
  }
}

const defaultProject = {
  token: null,
  group: "+ New group",
  src: "",
  href: "",
  alt: "",
  order: 0,
}

function ProjectsInput({project, label, id, onChange, placeholder}) {
  return <label className="w-full flex flex-col items-start">
    <span>{upperChar(label)}</span>
    <input
      name={label}
      onChange={e=>{
        let value = e.target.value
        onChange(s=>({...s, [id]: value}))
      }}
      placeholder={placeholder}
      className="w-full px-2 py-1 focus:border-black border-gray border-b-2 outline-none"
      value={project[id]}
    />
  </label>
}

function AdminProjects() {
  const [projects, setProjects] = React.useState([defaultProject])
  const [selected, select] = React.useState(0)
  const [project, setProject] = React.useState(projects[selected])
  const [processing, setProcessing] = React.useState(false)

  React.useEffect(() => {
    api.getProjectsProjects(
      (res) => {
        setProjects([defaultProject, ...(res.sort((v1,v2)=>v1.order-v2.order))])
      },
      ()=>{}
    )
  }, [setProjects])

  React.useEffect(() => {
    setProject(projects[selected])
  }, [selected, projects, setProject])
  let defaults = {project, onChange: setProject}

  return <main className="col py-16 h-screen">
    <div className="w-1/2">
      {project === undefined
        ? null
        : <form
          className="col space-y-6 w-full"
          name="projects"
        >
          <ProjectsInput {...defaults} id="group" label="group" placeholder="+ New group" />
          <ProjectsInput {...defaults} id="src" label="src" placeholder="The 'src' of the image you want." />
          <ProjectsInput {...defaults} id="href" label="link" placeholder="Link for when people click on the project." />
          <ProjectsInput {...defaults} id="alt" label="description" placeholder="Text for screen readers." />
          <ProjectsInput {...defaults} id="order" label="order" placeholder="0" />
          <div className="flex justify-evenly w-full">
            <Button
              onClick={(e)=>{
                if (processing) return
                  setProcessing(true)
                let form = document.forms.namedItem("projects")
                adminProjectsSubmit({
                  token: project.token,
                  group: form.group.value,
                  src: form.src.value,
                  href: form.link.value,
                  alt: form.description.value,
                  order: form.order.value,
                })
                e.preventDefault()
              }}
            >{project.token === null
              ? "Create New"
              : "Update"
            }</Button>
            <Button
              className="bg-err"
              onClick={(e) => {
                if (processing || project.token === null) return
                  setProcessing(true)
                api.deleteAdminProjectsByProject(
                  project.token,
                  ()=>{reload()},
                  ()=>{reload()},
                )
                  e.preventDefault()
              }}
            >Delete
            </Button>
          </div>
        </form>
      }

      <div className="h-16" />
      <div
        className="grid gap-4"
        style={{gridTemplateColumns: "repeat(auto-fill, 12rem)",
          placeContent: "center",
        }}
      >
        {projects.map((v,i)=>{
          return <div
            key={i}
            onClick={()=>select(i)}
            className={`${i===selected ? "bg-white" : "cursor-pointer bg-gray-3"} border w-48 h-16 flex justify-center items-center px-4 py-2`}
          >
            {v.group}
          </div>
        })}
      </div>
    </div>
  </main>
}

function ratesToFloats(rates) {
  let ret = {...rates}
  for (const [k,v] of Object.entries(rates)) {
    for (const rate of ["duration", "track"]) {
      let r1 = (v as any)[rate] / 100
      let r2 = r1.toFixed(2)
      ret[k][rate] = r2
    }
  }
  return ret
}

function ratesFromFloats(rates) {
  let ret = {...rates}
  for (const [k,v] of Object.entries(rates)) {
    for (const rate of ["duration", "track"]) {
      let r1 = Number.parseFloat((v as any)[rate])
      let r2 = r1.toFixed(2)
      let r3 = r2.replace(/\./, '')
      let r4 = Number.parseInt(r3)
      ret[k][rate] = r4
    }
  }
  return ret
}

function AdminRate() {
  const [rates, setRates] = React.useState({} as any)

  React.useEffect(() => {
    api.getAdminRates(
      (res)=> setRates(ratesToFloats(res)),
      ()=>{},
    )
  }, [setRates])

  if (Object.values(rates).length === 0)  {
    return <span>Loading...</span>
  }
  else {
    return <div
      className="h-screen grid place-items-center"
    >
      <form className="col items-center w-full pt-8">
        <strong className="text-4xl mb-8">
          Change Rates
        </strong>
        <fieldset className="divide-y-1">
          <div className="rate-type relative justify-between">
            <span
              className="whitespace-pre w-32 flex items-center"
              style={{fontWeight: 900}}
            >Per...</span>
            <span className="text-center" style={{width: "6rem"}}>Hour</span>
            <span className="text-center" style={{width: "6rem"}}>Track</span>
          </div>
          {Object.entries(rates).map(([k,v],i)=> {
            return <RateType key={k} rate={k} value={v} setRates={setRates} />
          })
          }
        </fieldset>
        <Button
          className="mt-8"
          onClick={(e) =>
            api.putAdminRates( ratesFromFloats(rates), ()=>{ window.location.replace("/admin") }, ()=>{},)
          }
        >
          Update
        </Button>
      </form>
    </div>
  }
}

function RateType({rate, value, setRates}) {
  let rateType = upperChar(rate)
  return <div className="rate-type relative justify-between">
    <span
      className="whitespace-pre w-32 flex items-center"
      style={{fontWeight: 900}}
    >{ rateType === "Service"
      ? "Service Fee"
      : rateType
    }:{" "}</span>
    <span>
      <MyCurrencyInput name={"duration"} value={value.duration} onChange={(e) => {
        setRates(s=>({...s, [rate]: {track: value.track, duration: Number.parseFloat(e)}}))
        }} />
    </span>
    <span>
      {rateType === "Service"
        ? <div className="charge">NA</div>
        : <MyCurrencyInput name={"track"} value={value.track} onChange={(e) => {
        setRates(s=>({...s, [rate]: {duration: value.duration, track: Number.parseFloat(e)}}))
        }} />
      }
    </span>
  </div>
}

function MyCurrencyInput({name, value, onChange}) {
  return <CurrencyInput
    name={name}
    className="charge"
    defaultValue={value}
    allowDecimals={true}
    prefix="$"
    allowNegativeValue={false}
    decimalsLimit={2}
    onChange={onChange}
  />
}

function AdminService() {
  const root = useStore(s=>s.root)
  React.useEffect(() => {
    api.postServiceMonth(
      {},
      basicAuth(root),
      (res)=> {
        let set = new Set()
        res.forEach((service)=>set.add(service.date.day))
        lCalDispatch({type: "ownedDays", payload: set})
      },
      ()=>{},
    )
  }, [root])

  const [state, ldispatch] = React.useReducer(reducer, initState())
  const [reload, reloadState] = React.useState(0)
  const [calState, lCalDispatch] = React.useReducer(calendarReducer, calendarInitState())

  const input = {state, onChange: callback, dispatch: ldispatch}
  const refs = {
    form: React.useRef(null),
  }

  React.useEffect(() => {
    callback({runQuery: state.runQuery})
  }, [calState, state.runQuery])

  React.useEffect(() => {
    let form = refs.form.current
    if (form === null) return
    state.runQuery.subscribe(() => {
      let {name, email, date} = form.elements
      let calendar: any = document.querySelector("#admin-calendar")
      let {day, month, year} = calendar.dataset
      let yyyymmdd = date.checked
        ? `${year}-${padInt(parseInt(month)+1)}-${padInt(parseInt(day))}`
        : ""
      api.getAdminSearch(
        name.value, email.value, yyyymmdd,
        (s)=>{
          s.sort((a,b) => { // sort by newest service
            let ad = a.service.date
            let bd = b.service.date
            // @ts-ignore
            let res = new Date(bd.year, bd.month, bd.day)- new Date(ad.year, ad.month, ad.day)
            return res
          })
          ldispatch({type: "get-results", results: s})
        },
        ()=>{}
      )
    })
    state.runQuery.next()
  }, [state.runQuery, refs.form, reload])

  return <main className="admin-main">
    <section className="flex flex-col">
      <form ref={refs.form}
      >
        <div className="flex justify-center">
          <Calendar
            ldispatch={lCalDispatch}
            {...calState}
          />
        </div>
        <div className="h-4" />
        <label className="whitespace-pre flex items-center w-min cursor-pointer">
          Use date?{" "}
          <Checkbox
            name="date"
            onChange={() => callback(state)}
          />
        </label>
        <Input {...input} field="name" />
        <Input {...input} field="email" />
      </form>
      <div className="mt-8" />
    </section>
    <section className="results">
      <strong className="w-full text-center">Results</strong>
      <div className="h-px bg-black w-full my-2" />
      {state.results.map(
        ({user, service},i)=> {
          let {date, start} = service
          return <div key={user.email + start + `${date.month}/${date.day}/${date.year}`}
            className={`flex justify-between w-96 cursor-pointer px-4 py-2
              ${state.result === i
                ? "bg-black text-white"
                : "bg-white text-black"
              }`}
            onClick={() => ldispatch({type: "set-result", payload: i})}
          >
            <span>{user.name}</span>
            <span>{`${date.month}/${date.day}/${date.year}`}</span>
          </div>
      }
      )}
    </section>
    <section className="w-96">
      { state.results.length > state.result // arrays indexed at 0
        ?  <ShowService
            key={state.results[state.result].serviceId}
            date={calState}
            {...state.results[state.result]}
            reloadState={reloadState}
        />
        : <div>No service found</div>
      }
    </section>
  </main>

}

function ShowService({date, service, user, serviceId, userId, reloadState}) {
  let props = {...service, ...user}
  const [price, setPrice] = React.useState(0)
  const root = useStore(s=>s.root)
  const [serviceStatus, setServiceStatus] = React.useState(service.status)
  const yyyymmdd = `${date.year}-${padInt(parseInt(date.month)+1)}-${padInt(parseInt(date.day))}`
  let {start: startTime, end: endTime} = getServiceDate(yyyymmdd, props.start, props.end)

  React.useEffect(() => {
    api.postServiceCharge(
      { start: props.start,
        end: props.end,
        day: date.day, month: date.month+1, year: date.year,
        ...service,
      },
      basicAuth(root),
      (res) => setPrice(parseCents(res) as any),
      (e) => {},
    )
  }, [service, setPrice, root, props.end, props.start, date])

  function Check(on) {
    return <div className={`admin-checkbox ${on ? "active" : ""}`} />
  }

  return <div className="show-service flex flex-col w-full">
    <span className="text-4xl">{props.name}</span>
    <span>{props.email}</span>
    <span><strong>{props.date.month}/{props.date.day}/{props.date.year}</strong></span>
    <span><strong>{dateToClock(startTime)}</strong> - <strong>{dateToClock(endTime)}</strong></span>
    <div className="h-4" />
    <span>Tracks: <strong>{props.tracks}</strong></span>
    <span>Price: <strong>${price}</strong></span>
    <span>Status: <strong>{serviceStatus}</strong></span>
    <span>Description:{" "}<strong>“{props.description}”</strong></span>
    <div className="h-4" />
    <span className="flex flex-col">
      <span className="flex items-center whitespace-pre">{Check(props.mastering)} Mastering</span>
      <span className="flex items-center whitespace-pre">{Check(props.mixing)} Mixing</span>
      <span className="flex items-center whitespace-pre">{Check(props.recording)} Recording</span>
    </span>
    <div className="h-4" />
    <div className="flex justify-between w-full mt-4">
      {serviceStatus === "Removed"
        ?  <Button
          className="bg-pri"
          onClick={() => confirmService(serviceId, setServiceStatus, reloadState)}
        >Confirm</Button>
        : <Button
          className="bg-err text-white"
          onClick={() => denyService(serviceId, setServiceStatus, reloadState)}
        >Remove</Button>
      }
    </div>
  </div>
}

function confirmService(serviceId, setState, reloadState) {
  api.putAdminDenyByDeny(
    false,
    serviceId,
    ()=>{
      setState("Confirmed")
      reloadState(s=>s+1)
    },
    ()=>setState(<span className="text-err">Error connecting to server</span>),
  )
}

function denyService(serviceId, setState, reloadState) {
  api.putAdminDenyByDeny(
    true,
    serviceId,
    ()=>{
      setState("Denied")
      reloadState(s=>s+1)
    },
    ()=>setState(<span className="text-err">Error connecting to server</span>),
  )
}

const reducer = produce((s,a) => {
  const f = s.form[get(a, ["target", "name"])]
  switch(a.type) {
    case "update-field": f.value = a.target.value; break
    case "get-results":
      s.results = a.results;
      if (s.result > s.results.length) s.result = 1; // reset 'result' to first entry when bigger than 'results'
      break;
    case "set-result": s.result = a.payload; break;
    default: break
  }
})

const initState = () => {
  return {
    runQuery: new Subject().pipe(ops.debounceTime(200)),
    results: [],
    result: 0,
    form: createForm({
      name: {required: false},
      email: {required: false},
    })
  }
}

const Day = ({className="", children, ...props}) =>
  <li
    className={`inline-block w-10 h-10 cursor-pointer flex justify-center
      relative items-center select-none text-2xl list-none
      ${className}
      `}
    {...props}
  >
    {children}
  </li>

const calendarInitState = () => {
  const today = new Date()
  return {
    editToken: null,
    successType: null,
    ownedDays: new Set(),
    day: today.getDate(),
    month: today.getMonth(),
    year: today.getFullYear(),
    tl: gsap.timeline({defaults: {ease: "elastic.out(1,.9)", duration: 0.2}}),
  }
}

const calendarReducer = produce((s,a) => {
  switch (a.type)  {
    case "ownedDays":
      s.ownedDays = a.payload
      break
    case "changeDay":
      s.day = a.day ?? 1
      s.prev = a.el
      break
    case "changeMonth":
      let month = s.month + a.direction
      let dyear = 0
      if (month > 11) {dyear = 1}
      else if (month < 0) {month = 11; dyear = -1}
      s.year = s.year + dyear
      s.month = month % 12
      s.day = a.day || 1
      break
    default: break;
  }
})

export const Calendar = ({ldispatch, ...state}) => {
  let calendarDays = getCalendarDays({ldispatch, ...state})

  React.useEffect(() => {
    let tl = state.tl
    let day = document.querySelector("#day-" + state.day) || document.querySelector("#day-1")
    let dayRect = day.getBoundingClientRect()
    let focus = document.querySelector("#calendar-focus")
    let focusRect = focus.getBoundingClientRect()
    tl.clear()
    if (dayRect.x !== focusRect.x) tl.to(focus, {x: "+="+(dayRect.x-focusRect.x)})
    if (dayRect.y !== focusRect.y) tl.to(focus, {y: "+="+(dayRect.y-focusRect.y)})
  }, [state.tl, state.day, state.month])

  return <article
    className="w-full md:w-min border"
    id="admin-calendar"
    data-day={state.day}
    data-month={state.month}
    data-year={state.year}
  >
    <div className="bg-white absolute -z-10" />
    <header className="bg-pri flex justify-between px-2 py-1 text-white text-2xl">
      <span
        className="px-2 py-1 select-none font-pri"
      >{monthYear(state.month, state.year)}</span>
      <div className="flex justify-end items-center space-x-2">
        <Arrow
          onClick={() => ldispatch({type: "changeMonth", direction: -1})}
          className="h-8 w-8 cursor-pointer text-white"
          style={{transform: "rotate(90deg)"}}
        />
        <Arrow
          onClick={() => ldispatch({type: "changeMonth", direction: 1})}
          className="h-8 w-8 cursor-pointer text-white"
          style={{transform: "rotate(-90deg)"}}
        />
      </div>
    </header>
    <ul className="flex px-2 py-1 items-center w-full justify-center pt-4">
      {["S", "M", "T", "W", "T", "F", "S"]
        .map((v,i)=><li
          key={v+i}
          className="w-10 h-10 text-center font-pri text-2xl select-none">
          {v}</li>)}
    </ul>
    <div className="flex flex-col px-2 py-1 items-center w-full pb-4">
      <div
        id="calendar-focus"
        className="h-10 w-10 absolute text-white border border-pri"
      />
      {calendarDays}
    </div>
  </article>
}

const getCalendarDays = ({ldispatch, ...state}) => {
  const getDaysInMonth = (month, year) => (new Array(31))
    .fill("")
    .map((v,i)=>new Date(year,month,i+1))
    .filter(v=>v.getMonth() === month || month === 12)
  const days = getDaysInMonth(state.month, state.year)
  const offset = days[0].getDay()
  const today = new Date()
  today.setDate(today.getDate()-1)

  const prevMonthDays = offset === 0 ? [] :
    getDaysInMonth(state.month-1, state.year)
  .slice(-offset)
  .map((v,i)=>
    <Day key={"" + v.getDate() + (state.month-2)}
      className="text-gray"
      onClick={e => ldispatch({type: "changeMonth", direction: -1, day: v.getDate()})}
    >{v.getDate()}</Day>)

    let calendar = [
      ...prevMonthDays,
      ...days.map((v,i) =>
        <Day
          key={"" + v.getDate() + (state.month-1)}
          id={"day-" + v.getDate()}
          onClick={(e) => ldispatch({type: "changeDay", day: v.getDate()})}
        >
          {state.ownedDays.has(v.getDate()) ?
            <div className="absolute bg-gray-3 top-0 right-0 mr-1 mt-2 h-3 w-3" />
            : null}
          <span className={`relative
            ${v.getTime() >= today.getTime() ? "" : "text-gray-6"}
          `}>{v.getDate()}</span>
        </Day>)
    ]

    const nextMonthOffset = calendar.length % 7
    const nextMonthDays = nextMonthOffset === 0 ? [] :
      getDaysInMonth(state.month+1, state.year)
    .slice(0,7-nextMonthOffset)
    .map((v,i)=>
      <Day key={"" + v.getDate() + (state.month+1)}
        className="text-gray"
        onClick={e => {
          ldispatch({type: "changeMonth", direction: 1, day: v.getDate()})
        }}
      >{v.getDate()}</Day>)

      calendar = calendar.concat(nextMonthDays)

      for (let i=0; i < calendar.length; i++)
      calendar.splice(i,7, <div key={i} className="flex"> {calendar.slice(i, i+7)} </div>)

      return calendar
}


