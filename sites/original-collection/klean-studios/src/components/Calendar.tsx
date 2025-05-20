import React from "react"
import {actions, dispatch, useStore} from "../store"
import {useHistory} from "react-router-dom"
import {
  isAdmin, monthYear, dateToTime, isFuture, padInt, rotateNum, timeDifference, timeToClock, get, hours12To24,
  Button, Arrow, Checkbox,
} from "./"
import gsap from "gsap"

const {calendar: {changeDay, changeMonth, editService}} = actions

export const Calendar = (props) => {
  let calendarDays = getCalendarDays(props)

  return <article
    className="w-full md:w-min border"
  >
    <div className="bg-white absolute -z-10" />
    <header className="bg-pri flex justify-between px-2 py-1 text-white text-2xl">
      <span
        className="px-2 py-1 select-none font-pri"
      >{monthYear(props.month, props.year)}</span>
      <div className="flex justify-end items-center space-x-2 pr-8 sm:pr-0">
        <Arrow
          onClick={() => changeMonth({direction: -1})}
          className="h-8 w-8 cursor-pointer text-white"
          style={{transform: "rotate(90deg)"}}
        />
        <Arrow
          onClick={() => changeMonth({direction: 1})}
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
        className={`h-10 w-10 absolute text-white border border-pri`}
      />
      {calendarDays}
    </div>
  </article>
}

export function TimeSelector(props) {
  let startDate = new Date()
  let endDate = new Date()
  endDate.setHours(endDate.getHours() + 1)
  const refs = {
    edit: React.useRef(null),
    menu: React.useRef(null),
  }
  let calendar = useStore(s=>s.calendar)
  let root = props.root

  const [state, ldispatch] = React.useReducer(
    timeSelectorReducer, {
      error: "",
      badTime: false,
      owned: (props.owned || isAdmin(root.user)) ?? false,
      editing: props.editing ?? false,
      startStatic: props.start ?? dateToTime(startDate),
      endStatic: props.end ?? dateToTime(endDate),
      start: props.start ?? dateToTime(startDate),
      end: props.end ?? dateToTime(endDate),
      description: props.description ?? "",
      token: props.token,
      promos: props.promos ?? [],
      recording: props.recording ?? false,
      mixing: props.mixing ?? false,
      mastering: props.mastering ?? false,
      tracks: props.tracks ?? 1,
      submit: props.submit ?? {type: "post"},
      status: props.status ?? null,
    })

    const serviceStartDate = new Date(
      calendar.year,
      calendar.month,
      calendar.day,
      hours12To24(state.start.hour, state.start.ispm),
      state.start.minute
    )

    const serviceEndDate = new Date(
      calendar.year,
      calendar.month,
      calendar.day,
      hours12To24(state.end.hour, state.end.ispm),
      state.end.minute
    )

    React.useEffect(() => {
      if (calendar.editToken !== props.token)
      ldispatch({type: "edit", payload: false})
    }, [calendar.editToken, props.token])

    React.useEffect(() => {
      let edit = refs.edit.current
      if (edit === null) return
      gsap.fromTo(
        edit.children,
        {yPercent: 100, marginTop: "0"},
        {yPercent: 0, marginTop: "-100%", display: "none", ease: "power3.inOut", duration: 0},
      )
    }, [refs.edit])

    React.useEffect(() => {
      let edit = refs.edit.current
      if (edit === null) return
      if (state.editing) {
        gsap.to(edit.children, {display: "block", duration: 0})
        gsap.fromTo(
          edit.children,
          {yPercent: 0, marginTop: "-100%"},
          {yPercent: 100, marginTop: "0", ease: "power3.inOut"}
        )
        gsap.fromTo(edit, {height: 0}, {duration: 0, height: "auto"})
      }

      else {
        if (edit.children[0].style.marginTop === "-100%") return
        gsap.fromTo(
          edit.children,
          {yPercent: 100, marginTop: "0"},
          {yPercent: 0, marginTop: "-100%", ease: "power3.inOut"},
        )
        gsap.to(edit.children, {display: "none", delay: 0.5, duration: 0})
        gsap.fromTo(edit, {height: "auto"}, {height: 0})
      }
    }, [refs.edit, state.editing])

    React.useEffect(() => {
      let menu = refs.menu.current
      if (menu === null) return
      if (state.editing)
        gsap.to(menu, {ease: "power3.inOut", yPercent: -50})
      else
        gsap.to(menu, {ease: "power3.inOut", yPercent: 0})
    }, [state.editing, refs.menu])

    function Time({children}) {
      return <span className="px-2 py-1">
        {timeToClock(children)}
      </span>
    }

    const isServiceFuture = new Date().getTime() < serviceStartDate.getTime()
    const isServiceEndFuture = new Date().getTime() < serviceEndDate.getTime()

    return <div
      className={`col w-full ${props.className || ""} `}
    >
      {props.new && isFuture(calendar)
        ?  <div
          className="flex items-center w-full cursor-pointer bg-sec h-10 justify-center"
          onClick={() => {
            editService({token: props.token})
            ldispatch({type: "edit"})
          }}
        >
          <div className="overflow-hidden h-8">
            <div ref={refs.menu} className="col h-16 justify-between">
              <strong>+create new</strong>
              <strong>close</strong>
            </div>
          </div>
        </div>

        : !props.new
        ?  <div
          className={`flex items-center w-full bg-sec h-10 justify-between
            ${isServiceFuture ? "cursor-pointer" : ""}
          `}
          onClick={() => {
            editService({token: props.token})
            isServiceFuture && ldispatch({type: "edit"})
          }}
        >
          <Time>{state.startStatic}</Time>
          <div className="overflow-hidden h-8">
            <div ref={refs.menu} className="col h-16 justify-between">
              {isServiceFuture ?
                <>
                  <strong>edit</strong>
                  <strong>close</strong>
                </>
                : isServiceEndFuture // Check if sservice is in progress
                  ? <strong>in progress</strong>
                  : <strong>ended</strong>
              }
            </div>
          </div>
          <Time>{state.endStatic}</Time>
        </div>
        : null
      }
      <div className="h-px w-full" />
      <div
        ref={refs.edit}
        className={`w-full overflow-hidden`}
      >
        <EditTimeWheel
          ldispatch={ldispatch}
          className="transform -translate-y-full"
          new={props.new}
          isServiceFuture={isServiceFuture}
          {...state}
        />
      </div>
    </div>
}

const timeSelectorReducer = (oldState, action) => {
  let state = {...oldState}
  switch(action.type) {
    case "changeTime":
      let s = state[action.timeFor]
    let newTime = {
      hour: rotateNum(s.hour, action.hour, 12, 1),
      minute: rotateNum(s.minute, 5 * action.minute, 55),
      ispm: !action.ispm ? s.ispm : !s.ispm,
    }
    let start = action.timeFor === "start" ? newTime : state.start
    let end = action.timeFor === "end" ? newTime : state.end
    let diff = timeDifference(start, end)
    diff <= 0 ? state.badTime = action.timeFor : state.badTime = false
    return {...state,
      [action.timeFor]: newTime,
    }
    case "serverError":
      state.error = action.error
    return state
    case "close":
      state.editing = false
    return state
    case "edit":
      state.error = null
      state.editing = action.payload ?? !state.editing
    return state
    default: return state
  }
}

const EditTimeWheel = ({className="", style=null, ...props}) => {
  const history = useHistory()
  const {ldispatch} = props
  const refs = {
    description: React.useRef(props.description),
    information: React.useRef(null),
  }

  const [price, setPrice] = React.useState("LOADING")
  const [tracks, setTracks] = React.useState(props.tracks)
  const [error, setError] = React.useState(null)
  const [flags, setFlags] = React.useState({
    mixing: props.mixing,
    mastering: props.mastering,
    recording: props.recording,
  })
  const calendar = useStore(s=>s.calendar)

  var tooltipMsg
  switch (props.status) {
    case "NotConfirmed": tooltipMsg = "You have attempted to create a service but have yet to pay."; break
    case "Processing": tooltipMsg = "Payment information is currently being handled. If successful, the service request will be created."; break
    case "Failed": tooltipMsg = "Attempted to create a service but payment somehow failed."; break
    case "Created": tooltipMsg = "You have successfully scheduled a service with Klean Studios!"; break
    case "Removed": tooltipMsg = "Your service has been removed."; break
    default: tooltipMsg = null
  }

  const Flag = ({children, flag}) =>
    <label className="cursor-pointer w-full flex justify-between items-center">{children}:
      {" "}<Checkbox
        checked={flags[flag]}
        onChange={() => setFlags(s=> ({...s, [flag]: !s[flag]}))}
      />
    </label>

  React.useEffect(() => {
    // Get user charge
    ldispatch({type: "serverError", error: ""})
    dispatch({
      filter: "calendar", type: "getCharge",
      token: props.token,
      start: props.start, end: props.end,
      mastering: flags.mastering,
      mixing: flags.mixing,
      recording: flags.recording,
      tracks: tracks,
      promos: [],
      onSuccess: (p) => {
        setError(null)
        setPrice(p)
      },
      onError: (err) => {
        setError(err)
        setPrice("0.00")
      },
    })
  }, [
    flags.mastering, flags.mixing, flags.recording,
    props.start, props.end, tracks, props.promos, props.token,
    calendar.day, calendar.month,
    ldispatch
  ])

  function submit() {
    if (props.submit.type === "post") { // POST
      if (error) return
      dispatch({
        filter: "confirmService", type: "addService",
        payload: {
          filter: "timeSelector", start: props.start, end: props.end,
          description: get(refs, ["description", "current", "value"]) || props.description,
          mixing: flags.mixing, mastering: flags.mastering, recording: flags.recording,
          tracks: tracks, promos: props.promos,
        },
      })
      history.push('/dashboard/confirm')

    // PUT service with 'NotConfirmed' status
    } else if (props.submit.type === "put" && props.status === "NotConfirmed") {
      if (error) return
      dispatch({
        filter: "confirmService", type: "addService",
        payload: {
          token: props.token,
          filter: "timeSelector", start: props.start, end: props.end,
          description: get(refs, ["description", "current", "value"]) || props.description,
          mixing: flags.mixing, mastering: flags.mastering, recording: flags.recording,
          tracks: tracks, promos: props.promos,
        },
      })
      history.push('/dashboard/confirm')

    // DELETE / PUT
    } else {
      if (props.submit.type === "put" && error) return
      dispatch({
        ...props.submit,
        filter: "timeSelector", start: props.start, end: props.end,
        token: props.token,
        description: get(refs, ["description", "current", "value"]) || props.description,
        mixing: flags.mixing, mastering: flags.mastering, recording: flags.recording,
        tracks: tracks, promos: props.promos,
        onSuccess: () =>  props.ldispatch({type: "close"}) ,
        onError: (e) => props.ldispatch({type: "serverError", error: "Perhaps the date is overlapping?"}),
      })
    }
  }

  React.useEffect(() => {
    let information = refs.information.current
    if (information === null) return
    let tooltip = information.querySelector(".tooltip")
    const mouseenter = (e) => {gsap.to(
      tooltip,
      {display: "block", delay: 0, duration: 0},
    )}
    const mouseleave = (e) => { gsap.to(
      tooltip,
      {display: "none", delay: 0, duration: 0},
    )}

    information.addEventListener("mouseenter", mouseenter)
    information.addEventListener("mouseleave", mouseleave)

    return () => {
      information.removeEventListener("mouseenter", mouseenter)
      information.removeEventListener("mouseleave", mouseleave)
    }
  }, [refs.information])

  return <form
    className={`space-y-4 col bg-white px-4 py-4 w-full relative
      ${className}
      `}
    style={style || {}}
    onSubmit={(e) => e.preventDefault()}
  >
    <span className="text-err w-full text-center block text-sm">{error === null ? props.error : error}</span>
    <div
      className="relative flex w-full items-center justify-between"
    >
      <div className="flex col">
        <strong>start</strong>
        <TimeWheel timeFor="start" badTime={props.badTime} ldispatch={props.ldispatch} {...props.start} />
      </div>
      <div className="flex col">
        <div>&nbsp;</div>
        <strong
          className={`text-2xl md:text-3xl lg:text-4xl
            ${(props.badTime || error ? "text-err" : "text-black")} `}
        >
          ${price}
        </strong>
      </div>
      <div className="flex col">
        <strong>end</strong>
        <TimeWheel timeFor="end" badTime={props.badTime} ldispatch={props.ldispatch} {...props.end} />
      </div>
    </div>
    <label className="w-full">
      <input
        placeholder="Additional Info"
        className="px-2 py-1 m-0 w-full bg-pri-2 placeholder-gray-7"
        onKeyPress={(e) => {
          if (e.key === "Enter") submit()
        }}
        ref={refs.description} type="text" defaultValue={props.description}
      />
    </label>
    <div className="w-full col">
      <div className="w-full md:w-64">
        <Flag flag="recording">Recording</Flag>
        <Flag flag="mixing">Editing/Mixing</Flag>
        <Flag flag="mastering">Mastering</Flag>
        <label className="w-full flex justify-between items-center">
          Tracks:&nbsp;
          <input type="number" value={tracks}
            className="w-12 border text-center"
            onChange={(e) => {
              let value = parseInt(e.target.value)
              if (isNaN(value)) setTracks(null)
                else if (value < 0) setTracks(0)
                  else if (value > 99) setTracks(99)
                    else setTracks(value)
            }}
          />
        </label>
        {props.status === null
          ? null
          : <div className="relative w-full">
            <label
              className="w-full flex justify-between items-center"
            >
              <span>Status:</span>
              <span
                className="flex"
              >
                {props.status}
                <span
                  ref={refs.information}
                  className="flex"
                >
                  <svg
                    className="w-4 px-1 box-content"
                    viewBox="0 0 512 512"
                  >
                    <path d="M256,0C114.497,0,0,114.507,0,256c0,141.503,114.507,256,256,256c141.503,0,256-114.507,256-256 C512,114.497,397.492,0,256,0z M256,472c-119.393,0-216-96.615-216-216c0-119.393,96.615-216,216-216 c119.393,0,216,96.615,216,216C472,375.393,375.384,472,256,472z"/>
                    <path d="M256,214.33c-11.046,0-20,8.954-20,20v128.793c0,11.046,8.954,20,20,20s20-8.955,20-20.001V234.33 C276,223.284,267.046,214.33,256,214.33z"/>
                    <circle cx="256" cy="162.84" r="27"/>
                  </svg>
                  <div
                    className="tooltip px-4 py-2 absolute top-0 right-0 w-full hidden bg-white border text-center z-20"
                    style={{
                      boxShadow: `2px 2px 2px rgba(0 0 0 / 50%)`,
                      transform: `translateY(-50%)`,
                    }}
                  >
                    {tooltipMsg}
                  </div>
                </span>
              </span>
            </label>
          </div>
        }
      </div>
    </div>
    <div
      className={`flex w-full space-x-6
        ${props.new ? "justify-center" : "justify-between"}
        `}
    >
      <Button
        className="bg-pri"
        onClick={(e) => submit()}
      >{
        props.status === "NotConfirmed"
          ? "Finish Payment"
          : props.new ? "Create" : props.owned ? "Update" : "Submit"
      }</Button>
      {/* {props.new ? null : */}
      {/* <Button */}
      {/*   className="text-white bg-err" */}
      {/*   onClick={() => { */}
      {/*     dispatch({ */}
      {/*       ...props.submit, */}
      {/*       token: props.token, */}
      {/*       filter: "timeSelector", type: "delete", */}
      {/*       onError: (e) => props.ldispatch({type: "serverError", error: "Perhaps the date is overlapping?"}), */}
      {/*       onSuccess: (s) => props.ldispatch({type: "close"}), */}
      {/*     }) */}
      {/*   }} */}
      {/* > */}
      {/*   Remove */}
      {/* </Button> */}
      {/* } */}
    </div>
  </form>
}

const TimeWheel = (props: any) =>
  <div
    className={"col " + (props.badTime === props.timeFor ? "text-err" : "")}
  >
    <div className="flex">
      <TimeColumn field='hour' {...props} />
      <div className="col justify-center"><span>:</span></div>
      <TimeColumn field='minute' {...props} />
      <TimeColumn field='ispm' {...props} />
    </div>
  </div>

const TimeColumn = ({field, ...rest}: any) =>
  <div className="col">
    <ChangeTime action={{[field]: 1}} {...rest} />
    <span>{typeof(rest[field]) === "number" ? padInt(rest[field])
    : rest[field] ? "PM" : "AM"
      }</span>
    <ChangeTime action={{[field]: -1}} {...rest} />
  </div>

const ChangeTime = ({ldispatch, timeFor, action}) =>
  <button
    type="button"
    onClick={e => ldispatch({type: "changeTime", timeFor: timeFor, ...action})}
  >
    {(action.hour === 1 || action.minute === 1 || action.ispm === 1) ?
      <Arrow
        className="h-8 w-8 cursor-pointer rotate-180 text-black"
      /> : <Arrow
        className="h-8 w-8 cursor-pointer text-black"
      />
    }
  </button>

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

const getCalendarDays = (state) => {
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
      onClick={e => changeMonth({direction: -1, day: v.getDate()})}
    >{v.getDate()}</Day>)

    let calendar = [
      ...prevMonthDays,
      ...days.map((v,i) =>
        <Day
          key={"" + v.getDate() + (state.month-1)}
          id={"day-" + v.getDate()}
          onClick={(e) => changeDay({day: v.getDate()})}
        >
          {state.ownedDays.has(v.getDate())
            ?  <div className="absolute bg-gray-6 top-0 right-0 mr-1 mt-2 h-3 w-3" />
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
          changeMonth({direction: 1, day: v.getDate()})
        }}
      >{v.getDate()}</Day>)

      calendar = calendar.concat(nextMonthDays)

      for (let i=0; i < calendar.length; i++)
      calendar.splice(i,7, <div key={i} className="flex"> {calendar.slice(i, i+7)} </div>)

      return calendar
}

