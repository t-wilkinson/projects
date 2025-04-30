import React from "react"
import { ScrollTrigger } from "gsap/ScrollTrigger"
import {gsap} from "gsap"
import {
  isAdmin, isPast, dateToClock, toyyyymmdd, dateToTime,
  A,
} from "../components"
import {Calendar, TimeSelector} from "../components/Calendar"
import {useStore, dispatch, actions} from "../store"

const {calendar: {changeDay}} = actions

gsap.registerPlugin(ScrollTrigger);

export default props => {
  const state = useStore(s=>s.calendar)
  const root = useStore(s=>s.root)

  React.useEffect(() => {
    window.scrollTo({top: 0, left: 0, behavior: "smooth"})
  }, [])

  React.useLayoutEffect(() => {
    dispatch({filter: "root", type: "getUser"})
    dispatch({type: "getServices"})
  }, [root.user])

  React.useEffect(() => {
    changeDay({day: new Date().getDate()})
  }, [state.tl])

  return <>
    { state.successType ?
      <div className="absolute inset-0 mx-2 col justify-evenly"
      >
        <span className="font-pri text-2xl bg-white border-2 rounded-lg
          py-8 px-4 z-50">
          {state.successType}
        </span>
      </div>
      : null
    }
    <main
      className="w-full h-full
      bg-white
      col bg-pri min-h-screen
      md:bg-white md:flex-row md:items-start relative
      "
    >
      <div
        className="w-full col relative bg-white
        md:mt-16 md:mx-10 md:h-full md:flex-grow
        " // md:max-w-lg
      >
        <Calendar
          month={state.month} year={state.year} day={state.day} ownedDays={state.ownedDays}
        />
      </div>
      <Logos />
      <TimeSheet
        year={state.year} month={state.month}
        day={state.day} services={state.services}
      />
    </main>
  </>
}

const Logos = () =>
  <>
    {/* <Logo */}
    {/*   style={{transform: "translate(-10px,0) rotate(-30deg) hidden sm:block"}} */}
    {/*   className="w-16 absolute bottom-1/2 right-0 text-black" */}
    {/* /> */}
    {/* <Logo */}
    {/*   className="w-32 absolute left-0 -ml-3 mt-10 text-black hidden md:block" */}
    {/*   style={{transform: "translate(50%, -50px) rotate(-30deg)"}} */}
    {/* /> */}
    {/* <Logo */}
    {/*   style={{transform: "translate(0, -50px) rotate(-35deg) hidden sm:block"}} */}
    {/*   className="w-96 absolute left-0 bottom-0 text-black" */}
    {/* /> */}
  </>

const TimeSheet = (props) => {
  const root = useStore(s=>s.root)
  const calendar = useStore(s=>s.calendar)

  return <div
    className="w-full bg-pri rounded-l-sm
    text-xl py-8 px-8 col
    min-h-screen md:flex-grow
    "
  >
    {root.user ?
      null : <A
        to="/login"
        className="bg-white px-4 my-2 py-2 border-2 rounded-lg w-full text-center no-underline hover:bg-pri-2"
      > Please sign-in to request a day.
      </A>
    }
    <ul
      className="w-full "
    >
      {root.user && !isPast(calendar) ?
        <TimeSelector
          className="my-2"
          new={true}
          owned={true}
          token={null}
        />
        : null
      }
      { props.services.length > 0 ?  <>
        <div className="bg-black  w-full rounded-lg" style={{height: "2px"}} />
        <ShowServices
          ymd={toyyyymmdd(props)}
          services={props.services}
          root={root}
        />
      </>
      : null
      }
    </ul>
  </div>
}

const ShowServices = ({ymd, services, root}) =>
  services.map((service) => {
    let {owned, start, end, description, token} = service
    owned = owned || isAdmin(root.user)
    return <li
      key={token}
      className="flex justify-between relative py-2"
    >
      {owned ?
      <TimeSelector
        owned={owned}
        date={ymd} description={description}
        start={dateToTime(start)} end={dateToTime(end)}
        token={token}
        submit={{type: "put"}}
      /> : <div
        className="bg-white flex justify-between w-full rounded-lg px-4 items-center"
      >
        <span className="px-2 py-1">{dateToClock(end)}</span>
        <span className="px-2 py-1">{dateToClock(start)}</span>
      </div>
      }
    </li>
  })
