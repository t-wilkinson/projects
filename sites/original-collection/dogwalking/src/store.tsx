import {dispatch, mergeEpics, ofMap, createStore} from "rxjs-redux-store"
import myslices from "./slice"
import * as api from "./api"
import {
  timeToSeconds, parseCents, ymdToUTC, toyyyymmdd
} from "./components"

const [Provider, useStore, subscribe, actions]: any = createStore(myslices)
export {useStore, actions, dispatch, Provider, subscribe}

mergeEpics(
ofMap(["root"], ["getUser"], (s,a) => {
    api.getSessionUser(
      (res) => {
        dispatch({
          filter: "root", type: "rootLoginUser",
          user: res.email, token: res.token,
        })
        a.onSuccess()
      },
      (e) => {
        console.error(e.message)
      }
    )
}),
ofMap(["root"], ["getUsers"], (s,a) => {
  api.getSessionUsers(
    (res) => a.onSuccess(res),
    (e) => a.onError(e),
  )
}),
ofMap(["root"], ["changeUser"], (s,a) => {
  api.postSessionUser(
    a.token,
    (res) => {
      dispatch({filter: "root", type: "getUser"})
      a.history.push("/calendar" || a.history.location.s)
    },
    (e) => console.error(e.message),
  )
}),
ofMap(["root"], ["logout"], (s,a) => {
  api.postSessionLogout(
    () => {
      dispatch({filter: "root", type: "rootLogoutUser"})
      a.history.push("/" || a.history.location.s)
    },
    (e) => console.error(e.message),
  )
}),
ofMap(["root"], ["login"], (s,a) => {
  api.postSessionLogin(
    a.body,
    (suc) => {
      a.onSuccess(suc)
      if (a.body.remember) {
        dispatch({filter: "root", type: "getUser"})
      } else {
        sessionLogin(suc, a.onError)
      }
    },
    (err) => {
      a.onError(err)
    }
  )
}),
ofMap(["root"], ["register"], (s,a) => {
  api.postSessionRegister(
    a.body,
    (suc) => {
      a.onSuccess(suc)
      if (a.body.remember) {
        dispatch({filter: "root", type: "getUser", response: suc, remember: a.body.remember})
      } else {
        sessionLogin(suc, a.onError)
      }
    },
    (err) => {
      a.onError(err)
    },
  )
}),
ofMap(["header"], ["toggle"], (s,a) => {
  let {header: {tl, visible}} = s
  if (visible) { tl.play() }
  else { tl.reverse() }
}),
ofMap(["calendar"], ["changeMonth"], (s,a) => {
  return ({...a, type: "changeDay", day: a.day})
}),
ofMap(["calendar"], ["changeDay"], (s,a) => {
  let {calendar: {tl}} = s
  let day = document.querySelector("#day-" + a.day) || document.querySelector("#day-1")
  let dayRect = day.getBoundingClientRect()
  let focus = document.querySelector("#calendar-focus")
  let focusRect = focus.getBoundingClientRect()
  tl.clear()
  if (dayRect.x !== focusRect.x) tl.to(focus, {x: "+="+(dayRect.x-focusRect.x)})
  if (dayRect.y !== focusRect.y) tl.to(focus, {y: "+="+(dayRect.y-focusRect.y)})
}),
ofMap(["calendar", "root"],
           ["login", "logout", "getServices", "changeMonth"],
           (s,a) => {
  let {calendar: {year, month}} = s
  api.postServiceMonth(
    { year: year,
      month: month+1,
    },
    basicAuth(s.root),
    (services) => dispatch(
      {type: "receivedMonthlyServices", filter: "calendar", services: services}
    ),
    (e) => console.error("There appears to be an issue with the server, I have been notified and will fix it asap", e),
  )
}),
ofMap(["calendar"], ["getServices", "changeDay"], (s,a) => {
  let {calendar: {year, month, day}} = s
  api.postServiceDay(
    { year: year,
      month: month+1,
      day: day,
    },
    basicAuth(s.root),
    (services) => dispatch(
      {type: "receivedServices", services: services, filter: "calendar"}
    ),
    (e) => console.error("Error communicating with the server.", e),
  )
}),
ofMap(["timeSelector"], ["post"], (s,a) => {
  let date = toyyyymmdd(s.calendar)
  let utc = ymdToUTC(date, a.start, a.end)
  let [year, month, day] = utc.date.split(/-/).map(v=>parseInt(v))
  api.postService(
    { description: a.description,
      year: year,
      month: month,
      day: day,
      start: utc.start,
      end: utc.end,
    },
    basicAuth(s.root),
    (s) => {
      a.onSuccess(s)
      onCalendarSuccess("POST")
      dispatch({type: "getServices"})
    },
    a.onError,
  )
}),
ofMap(["timeSelector"], ["put"], (s,a) => {
  let date = toyyyymmdd(s.calendar)
  let utc = ymdToUTC(date, a.start, a.end)
  api.putServiceByToken(
    a.token,
    { token: a.token,
      start: utc.start,
      end: utc.end,
      description: a.description,
    },
    basicAuth(s.root),
    (s) => {
      a.onSuccess(s)
      onCalendarSuccess("PUT")
      dispatch({type: "getServices"})
    },
    a.onError,
  )
}),
ofMap(["timeSelector"], ["delete"], (s,a) => {
  api.deleteServiceByToken(
    a.token,
    basicAuth(s.root),
    (suc) => {
      a.onSuccess(suc)
      onCalendarSuccess("DELETE")
      dispatch({type: "getServices"})
    },
    a.onError,
  )
}),
ofMap(["calendar"], ["getCharge"], (s,a) => {
  api.postServiceCharge(
    { start: timeToSeconds(a.start),
      end: timeToSeconds(a.end),
    },
    basicAuth(s.root),
    (res) => {
      a.onSuccess(parseCents(res))
    },
    (e) => {
      a.onError(e)
    },
  )
}),

)

function onCalendarSuccess(method) {
  dispatch({filter: "calendar", type: "onSuccess", payload:method})
  setTimeout(() => {
    dispatch({filter: "calendar", type: "onSuccess", payload: null})
  }, 2000)
}

function sessionLogin(response,onError) {
  const {email, token, session} = response
  if (!(email && token && session)) {
    if (onError) onError("Did not recieve email/token from server.")
    return
  } else {
    dispatch({
      filter: "root", type: "rootLoginUser",
      user: email,
      token: token,
      session: session,
    })
  }
}

function basicAuth(root) {
  if (root.session) {
    let auth = btoa(`${root.user}:${root.session}`)
    return `Basic ${auth}`
  } else {
    return null
  }
}
