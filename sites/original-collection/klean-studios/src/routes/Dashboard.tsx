import React from "react"
import {useStore, dispatch} from "../store"
import {
  A,
  isFuture, toyyyymmdd, dateToTime, dateToClock,
} from "../components"
import * as calendar from "../components/Calendar"
import StripeCheckout from "../components/Stripe"

export default (props) => {
  const state = useStore(s => s.calendar)
  const root = useStore(s => s.root)

  React.useEffect(() => {
    document.title = "Dashboard | Klean Studios"
  }, [])

  React.useEffect(() => {
    dispatch({filter: "calendar", type: "getServices"})
    dispatch({filter: "root", type: "getUser"})
  }, [root.user])

  React.useEffect(() => {
    dispatch({filter: "calendar", type: "changeDay", day: new Date().getDate()})
  }, [state.tl])
  let payment = <Payment root={root} />

  return <main
    className="w-full h-full min-h-screen col
    md:bg-white md:flex-row md:items-start
    "
  >
    { state.successType ?
      <div className="absolute w-full h-full col justify-evenly"
      >
        <span className="font-pri text-2xl bg-white border border-black
          py-8 px-4 z-30">
          {state.successType}
        </span>
      </div>
      : null
    }
    <div
      className="w-full md:h-screen col bg-white justify-between md:pt-16"
    >
      <calendar.Calendar
        month={state.month} year={state.year} ownedDays={state.ownedDays}
      />
      <div className="w-full hidden md:block">{payment}</div>
    </div>
    <TimeSheet
      year={state.year} month={state.month}
      day={state.day} services={state.services}
    />
    <div className="w-full md:hidden">{payment}</div>
  </main>
}

const Payment = ({root}) =>
  <div className="h-48 w-full justify-center col bg-gray-3 py-4 space-y-4">
    {root.user
      ?  <>
        <StripeCheckout />
        <div className="text-xl">{root.user}</div>
      </>
      : <A to="/login" className="
        md:bg-pri md:text-sec md:border border-pri
        hover:bg-sec hover:text-pri
        font-sec text-xl py-4 px-8
        ">
        Please sign-in to request a day
      </A>
    }
  </div>

const TimeSheet = (props) => {
  const cal = useStore(s=>s.calendar)
  const root = useStore(s=>s.root)

  return <div
    className="md:min-h-screen sm:h-full w-full bg-pri
    text-xl py-8 px-8 col pb-16
    relative
    "
    style={{flexGrow: 1}}
  >
    <span className="w-full text-center absolute top-0 text-gray text-xs" style={{lineHeight: "3"}}>Prices are subject to change.</span>
    <ul
      className="w-full h-full"
    >
      {root.user && isFuture(cal) ?
        <calendar.TimeSelector className="py-2" new={true} owned={true} token={null} />
        : null
      }
      <ShowServices
        ymd={toyyyymmdd(props)}
        services={props.services}
        root={root}
      />
    </ul>
  </div>
}

const ShowServices = ({ymd, services, root}) =>
  services.map((service) => {
    let {owned, start, end, token, status} = service
    return <li
      key={token}
      className="flex justify-between relative py-2"
    >
      {owned ?
      <calendar.TimeSelector
        owned={owned}
        {...service}
        status={status}
        date={ymd}
        root={root}
        start={dateToTime(start)} end={dateToTime(end)}
        submit={{type: "put"}}
      /> : <div
        className="bg-white flex justify-between w-full items-center"
      >
        <span className="px-2 py-1">{dateToClock(start)}</span>
        <span>slot filled</span>
        <span className="px-2 py-1">{dateToClock(end)}</span>
      </div>
      }
    </li>
  })
