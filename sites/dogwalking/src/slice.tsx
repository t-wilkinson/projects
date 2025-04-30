import gsap from "gsap"
import {toyyyymmdd, getServiceDate, dateToSeconds} from "./components"

export default {
  root: {
    initialState: {
      user: null,
      token: null,
      session: null,
      userAgent: (agent) => RegExp(agent, 'i').test(window.navigator.userAgent),
    },
    reducers: {
      rootLoginUser: (s,a) => {
        s.user = a.user
        s.token = a.token
        s.session = a.session ?? null
      },
      rootLogoutUser: (s) => {
        s.user = null
      },
    },
  },

  header: {
    initialState: {
      visible: false, form: "register", tl: gsap.timeline({pause: true})
    },
    reducers: {
      updateTl: (s, a) => {
        const childs = Array.from(document.querySelector('#header-aside-content').children)
          .map(e=>Array.from(e.children))
          .flat()
        // Header animation for small screens
        s.tl = gsap.timeline()
          .fromTo("body", {overflow: "auto"}, {overflow: "hidden", duration: 0.1}, 0)
          .fromTo("#header-aside", {overflow: "auto"}, {overflow: "hidden", duration: 0.1}, 0)
          .fromTo("#header-aside", {display: "none"}, {display: "block", ease: "circ.in"}, 0)
          .fromTo("#header-aside-bg",
                  {opacity: 0, y: "-50%"},
                  {opacity: 1, ease: "circ.out", duration: 0.5, y: 0, }, 0)
          .fromTo("#header-aside-content", {opacity: 0}, {opacity: 1}, 0)
          .fromTo(childs,
                  {y: "100", opacity: 0},
                  {y: "0", opacity: 1,
                    duration: 0.4,
                    stagger: {each: 0.1, from: "start"},
                  }, 0)
          .progress(s.tl.progress())
          .pause()
      },
      toggle: (s,a) => {
        s.visible = a.payload ?? !s.visible
      },
    },
  },

  home: {
    initialState: {
      selectedService: "walk",
      rates: {
        walking: "...",
        sitting: "...",
      },
    },
    reducers: {
      setRates: (s,a) => {
        s.rates.walking = a.payload
        s.rates.sitting = (parseFloat(a.payload) * 2).toFixed(2)
      },
      changeService: (s, a) => {
        s.selectedService = a.service
      },
    },
  },

  calendar: {
    initialState: () => {
      const today = new Date()
      return {
        services: [],
        editToken: null,
        successType: null,
        ownedDays: new Set(),
        day: today.getDate(),
        month: today.getMonth(),
        year: today.getFullYear(),
        tl: gsap.timeline({defaults: {ease: "elastic.out(1,.9)", duration: 0.2}}),
      }
    },
    reducers: {
      onSuccess: (s,a) => {
        switch (a.payload) {
          case "POST":
            s.successType = "Succesfully created service!"
            break;
          case "PUT":
            s.successType = "Succesfully changed service!"
            break;
          case "DELETE":
            s.successType = "Succesfully deleted service!"
            break;
          default:
            s.successType = null
            break;
        }
      },
      editService: (s,a) => {
        s.editToken = a.token
      },
      changeDay: (s, a) => {
        s.day = a.day ?? 1
        s.prev = a.el
      },
      changeMonth: (s, a) => {
        let month = s.month + a.direction
        let dyear = 0
        if (month > 11) {dyear = 1}
        else if (month < 0) {month = 11; dyear = -1}
        s.year = s.year + dyear
        s.month = month % 12
        s.day = a.day || 1
      },
      receivedServices: (s, a) => {
        let ymd = toyyyymmdd(s)
        s.services = a.services.map((service) => {
          let {start, end} = service
          return {...service, ...getServiceDate(ymd, start, end)}
        }).sort((a,b) => dateToSeconds(a.start) - dateToSeconds(b.start))
      },
      receivedMonthlyServices: (s, a) => {
        s.ownedDays = new Set()
        a.services.forEach(service => service.owned && s.ownedDays.add(service.day))
      },
    },
  },
}
