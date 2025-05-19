import gsap from "gsap"
import {
  toyyyymmdd, getServiceDate, dateToSeconds
} from "./components"

export default {
  confirmService: {
    initialState: { },
    reducers: {
      addService: (s,a) => {
        Object.assign(s, a.payload)
      },
    },
  },
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
        s.userToken = a.token
        s.session = a.session ?? null
      },
      rootLogoutUser: (s) => {
        s.user = null
      },
    },
  },
  header: {
    initialState: {
      active: false,
    },
    reducers: {
      updateHeader: (s,a) => {
        s.active = a.active ?? !s.active
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
          case "POST": s.successType = "Succesfully created service!"; break;
          case "PUT": s.successType = "Succesfully changed service!"; break;
          case "DELETE": s.successType = "Succesfully deleted service!"; break;
          default: s.successType = null; break;
        }
      },
      editService: (s,a) => {
        s.editToken = a.token
      },
      changeDay: (s,a) => {
        s.day = a.day ?? 1
        s.prev = a.el
      },
      changeMonth: (s,a) => {
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
          return {...service, ...service.date, ...getServiceDate(ymd, start, end)}
        }).sort((a,b) => dateToSeconds(a.start) - dateToSeconds(b.start))
      },
      receivedMonthlyServices: (s, a) => {
        s.ownedDays = new Set()
        a.services.forEach(service => service.owned && s.ownedDays.add(service.date.day))
      },
    },
  },
}
