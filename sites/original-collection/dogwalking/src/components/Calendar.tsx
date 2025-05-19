import React from "react"
import {actions, useStore, dispatch} from "../store"
import gsap from "gsap"
import {
 dateToTime, monthYear, isPast, padInt, get, rotateNum, timeDifference, timeToClock,
 Button, Arrow,
} from "./"

const {calendar: {changeDay, changeMonth, editService}} = actions

export const Calendar = (props) => {
  let calendarDays = getCalendarDays({...props})
  /* let root = useStore(s=>s.root) */

  return <article
    className="w-full md:w-min md:rounded-lg md:border-2 md:border-black"
  >
    <div className="bg-white absolute -z-10" />
    <header className="bg-pri flex justify-between px-2 py-1 text-2xl md:rounded-t-md">
      <span
        className="px-2 py-1 select-none font-pri text-xl"
      >{monthYear(props.month, props.year)}
      </span>
      <div className="flex justify-end items-center space-x-2">
        <Arrow
          onClick={() => changeMonth({direction: -1})}
          className="h-8 w-8 cursor-pointer text-black"
        />
        <Arrow
          onClick={() => changeMonth({direction: 1})}
          className="h-8 w-8 cursor-pointer text-white"
          style={{transform: "rotate(-180deg)"}}
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
        className={`h-10 w-10 bg-pri absolute text-white rounded-full
        `}
          // ${root.userAgent('safari') ? "hidden" : ""}
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

  const [state, ldispatch] = React.useReducer(
    timeSelectorReducer, {
      error: "",
      badTime: false,
      owned: props.owned ?? false,
      editing: props.editing ?? false,
      startStatic: props.start ?? dateToTime(startDate),
      endStatic: props.end ?? dateToTime(endDate),
      start: props.start ?? dateToTime(startDate),
      end: props.end ?? dateToTime(endDate),
      description: props.description ?? "",
      token: props.token,
      submit: props.submit ?? {type: "post"},
    })

    React.useEffect(() => {
      if (calendar.editToken !== props.token)
      ldispatch({type: "edit", payload: false})
    }, [calendar.editToken, props.token])

    React.useEffect(() => {
      let edit = refs.edit.current
      if (edit === null) return

      if (state.editing)
        gsap.fromTo(
          edit.children,
          {yPercent: 0, marginTop: "-100%"},
          {yPercent: 100, marginTop: "0", ease: "power3.inOut"}
        )
      else {
        if (edit.children[0].style.marginTop === "-100%") return
        gsap.fromTo(
          edit.children,
          {yPercent: 100, marginTop: "0"},
          {yPercent: 0, marginTop: "-100%", ease: "power3.inOut"},
        )
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

    return <div
      className={`col w-full bg-sec rounded-lg ${props.className || ""}`}
    >
      <div
        className={`
          flex items-center w-full cursor-pointer h-10 px-4 py-2
          ${props.new ? "justify-center" : "justify-between"}
          `}
        onClick={() => {
          editService({token: props.token})
          !isPast(calendar) && ldispatch({type: "edit"})
        }}
      >
        {props.new ? null : <Time>{state.startStatic}</Time>}
        <div className="overflow-hidden h-8">
          <div ref={refs.menu} className="col h-16 justify-between">
            {!isPast(calendar)?
              <>
                <strong>{props.new ? "+create new" : "edit"}</strong>
                <strong>close</strong>
              </>
              : <span>ended</span>
            }
          </div>
        </div>
        {props.new ? null : <Time>{state.endStatic}</Time>}
      </div>
      <div className="h-px w-full bg-pri" />
      <div ref={refs.edit} className="w-full overflow-hidden">
          <EditTimeWheel
            ldispatch={ldispatch}
            className="transform -translate-y-full"
            style={{marginTop: "-100%"}}
            new={props.new}
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
  const {ldispatch} = props
  const refs = {
    description: React.useRef(null),
  }

  const [price, setPrice] = React.useState("LOADING")

  React.useEffect(() => {
    ldispatch({type: "serverError", error: ""})
    dispatch({
      filter: "calendar", type: "getCharge",
      start: props.start, end: props.end,
      onSuccess: p => setPrice(p),
      onError: () => setPrice("0.00"),
    })
  }, [
    props.start, props.end, ldispatch
  ])

  function submit() {
    dispatch({
      ...props.submit,
      filter: "timeSelector", start: props.start, end: props.end,
      token: props.token,
      description: get(refs, ["description", "current", "value"]) || props.description,
      onSuccess: () => {
        props.ldispatch({type: "close"})
      },
      onError: (e) => {
        props.ldispatch({type: "serverError", error: "Perhaps the date is overlapping?"})
      },
    })
  }

  return <form
    className={`space-y-4 col bg-white px-4 py-4 pt-0 w-full
      rounded-b-lg
      ${className}
      `}
    style={style || {}}
    onSubmit={(e) => e.preventDefault()}
  >
    <span className="absolute top-0 text-err">{props.error}</span>
    <div
      className="relative flex w-full items-center justify-between"
    >
      <div className="flex col">
        <strong>start</strong>
        <TimeWheel timeFor="start" badTime={props.badTime} ldispatch={props.ldispatch} {...props.start} />
      </div>
      <strong
        className={`text-xl sm:text-2xl md:text-3xl lg:text-4xl mt-8
          ${(props.badTime || parseInt(price) <= 0 ? "text-err" : "text-black")}
          `}
      >
        ${props.badTime ? 0 : price}
      </strong>
      <div className="flex col">
        <strong>end</strong>
        <TimeWheel timeFor="end" badTime={props.badTime} ldispatch={props.ldispatch} {...props.end} />
      </div>
    </div>
    <label className="w-full">
      <input
        placeholder="Additional Info"
        className="px-2 py-1 m-0 w-full bg-pri-2 placeholder-gray-7 rounded-sm"
        onKeyPress={(e) => {
          if (e.key === "Enter") submit()
        }}
        ref={refs.description} type="text" defaultValue={props.description}
      />
    </label>
    <div
      className={`flex w-full space-x-6
        ${props.new ? "justify-center" : "justify-between"}
        `}
    >
      <Button
        className="bg-pri hover:bg-pri-3"
        onClick={(e) => submit()}
      >{props.new ? "Create" : props.owned ? "Update" : "Submit"}</Button>
      {props.new ? null :
      <Button
        className="text-white bg-err hover:bg-err-3"
        onClick={() => {
          dispatch({
            ...props.submit,
            token: props.token,
            filter: "timeSelector", type: "delete",
            onError: (e) => props.ldispatch({type: "serverError", error: "Perhaps the date is overlapping?"}),
            onSuccess: (s) => props.ldispatch({type: "close"}),
          })
        }}
      >
        Remove
      </Button>
      }
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
        className="h-8 w-8 cursor-pointer rotate-90 text-black"
      /> : <Arrow
        className="h-8 w-8 cursor-pointer -rotate-90 text-black"
      />
    }
  </button>

const Day = ({className="", children, ...props}) =>
  <li
    className={`inline-block w-10 h-10 cursor-pointer flex justify-center
      relative items-center select-none text-2xl list-none
      rounded-full
      md:hover:bg-pri-3
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
  const offset = days[0].getDay() // get day of week; 0-6; sun-sat

  // Previous month days
  const prevMonthDays = offset === 0
    ? []
    : getDaysInMonth(state.month-1, state.year)
      .slice(-offset)
      .map((v,i)=>
        <Day key={"" + v.getDate() + (state.month-2)}
          className="text-gray"
          onClick={e => changeMonth({direction: -1, day: v.getDate()})}
        >{v.getDate()}</Day>)

    // Current month days
    let calendar = [
      ...prevMonthDays,
      ...days.map((v,i) =>
        <Day
          key={"" + v.getDate() + (state.month-1)}
          id={"day-" + v.getDate()}
          onClick={(e) => changeDay({day: v.getDate()})}
        >
          {state.ownedDays.has(v.getDate()) ?
            <div className="absolute bg-gray-4 rounded-full top-0 right-0 mr-1 mt-2 h-3 w-3" />
            : null}
          <span className="relative">{v.getDate()}</span>
        </Day>)
    ]

    // Next month days
    const nextMonthOffset = calendar.length % 7
    const nextMonthDays = nextMonthOffset === 0
      ? []
      : getDaysInMonth(state.month+1, state.year)
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
      calendar.splice(i,7, <div key={i} className="flex">{calendar.slice(i, i+7)}</div>)

      return calendar
}

