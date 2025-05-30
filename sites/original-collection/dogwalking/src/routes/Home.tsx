import React from "react"
import gsap from "gsap"
import {Link} from "react-router-dom"
import {
  get, makeRefs,
  A, SocialMedia, Button
} from "../components"

import {actions, dispatch, useStore} from "../store"
const {home: {changeService, setRates}} = actions


export default () => {
  const state = useStore(s => s.home)
  const refs = makeRefs(["main"])

  React.useEffect(() => {
    let trigger = {
      toggleActions: "play reverse play reverse",
      start: "500px bottom",
      trigger: "#home\\:choose-us",
    }

    gsap.fromTo(
      ".price",
      { color: "#20c9c6"},
      { color: "white",
        duration: 0.5,
        scrollTrigger: trigger,
      }
    )

    gsap.fromTo(
      refs.main.current,
      { background: "white"},
      { background: "rgb(199, 235, 234)",
        duration: 0.5,
        scrollTrigger: trigger,
      }
    )

  }, [refs.main])

  React.useEffect(() => {

    for (const el of Array.from(document.querySelectorAll(".my\\:fade-in")))
      gsap.fromTo(
        el.children,
        { opacity: 0, y: 50},
        { opacity: 1, y: 0, stagger: 0.3,
          scrollTrigger: {
            toggleActions: "play none play none",
            trigger: el,
            start: "100px bottom",
          },
        }
      )

      let chooseUs = document.querySelector("#home\\:choose-us")
      gsap.fromTo(chooseUs,
        { opacity: 0, y: 50},
        { opacity: 1, y: 0,
          scrollTrigger: {
            toggleActions: "play none play none",
            trigger: chooseUs,
            start: "100px bottom",
          },
        }
      )

    let service = document.querySelector("#home\\:services")
    gsap.fromTo(
      service,
      { opacity: 0, y: 50},
      { opacity: 1, y: 0, stagger: 0.3,
        scrollTrigger: {
          toggleActions: "play none play none",
          trigger: service,
          start: "200px bottom",
        },
      }
    )


  }, [])

  React.useEffect(() => {
    dispatch({
      filter: "calendar", type: "getCharge",
      start: {minute: 0, hour: 0, ispm: false},
      end: {minute: 30, hour: 0, ispm: false}, // 30min
      onSuccess: (suc) => setRates({payload: suc}),
      onError: () => {},
    })
  }, [])

  return <main
    ref={refs.main}
    className=""
  >
    <section
      className="h-screen text-white h-128
      overflow-hidden relative px-4"
    >
      <div
        className="h-full inset-0 absolute"
        style={{
          background: "no-repeat center url(https://www.dogingtonpost.com/wp-content/uploads/2014/04/walkingthedog2.jpg)",
          backgroundSize: "cover",
          filter: "brightness(0.3)",
        }}
      />
      <nav className="absolute right-0 h-full flex flex-col justify-center px-2 mr-4">
        <SocialMedia className="w-8 h-8 hidden sm:block" fill="#20c9c6" />
      </nav>
      <div
        // className="w-full pt-24 md:pt-0 flex flex-col h-full md:ml-16 md:justify-center"
        className="z-10 relative w-full mt-24 md:ml-16"
      >
        <h2 className="font-pri">Friendly Dog Walking</h2>
        <h3
          style={{fontFamily: "Roboto", color: "rgb(255 255 255 / 70%)"}}
        >for residents of montgomery county</h3>
        <div className="w-full col sm:flex-row">
          <Link
            to="/calendar"
            className="mt-16 inline-block text-black rounded-md"
          >
            <Button className="text-2xl p-2 bg-pri hover:bg-pri-3">Schedule a service</Button>
          </Link>
        </div>
      </div>
    </section>

    <section id="home:services" className="py-2 w-full col py-16">
      <div className="max-w-md w-full">
        <ul className="flex justify-evenly w-full">
          <ServiceHeader state={state} service="walk" label="Walking">
            <g><path d="m336 216-24 32-40 16-24 16-16-16v-16l24-8 40-24 24-48 16.468 39.404z" fill="#fdc9a6"/><path d="m360 320 24 72 48 64 24-40-32-40-24-96z" fill="#a8a8a8"/><path d="m280 456-16 16v16h64v-32z" fill="#683b0d"/><path d="m400 267.155v-11.155h-64l-56 96v104h48v-96l32-32 25.26-25.26a50.324 50.324 0 0 0 14.74-35.585z" fill="#cbcbcb"/><path d="m224 416v32l-16 40h32l16-40v-26l-24-30z" fill="#c38325"/><path d="m112 408-27.095 20.321a8 8 0 0 0 -2.628 9.372l13.723 34.307 24-8-8-24 16-16z" fill="#c38325"/><path d="m400 256v-98.559a18 18 0 0 0 -3.014-9.971l-20.986-31.54h-40l8 16-23.768 32.733a27.249 27.249 0 0 0 -3.8 24.629 27.25 27.25 0 0 0 13.668 15.756l5.9 2.952v48z" fill="#ff6268"/><path d="m365.933 175.822 18.067 48.178-2.683 38.78-9.317 17.22 16.425 14.606 19.575-3.606 8-75-18.247-51.752a16.931 16.931 0 0 0 -21.942-10.212 16.932 16.932 0 0 0 -9.878 21.786z" fill="#fdc9a6"/><path d="m432 456v24h32l16-40-24-24z" fill="#683b0d"/><path d="m328 59.93v32a8 8 0 0 0 8 8h8v16h32v-16l8-24h-16l-8-16z" fill="#fdc9a6"/><path d="m328 59.93-3.13-9.389a20.165 20.165 0 0 1 7.945-23.154 20.162 20.162 0 0 1 22.37 0l12.815 8.543h14.774a14.928 14.928 0 0 1 13.352 8.252 14.928 14.928 0 0 1 -.931 14.956l-11.195 16.792h-16l-8-16z" fill="#683b0d"/><path d="m264 344a36.612 36.612 0 0 0 8.845-37.466l-.845-2.534 12.172 3.043a32.3 32.3 0 0 1 23.836 25 32.3 32.3 0 0 1 -12.291 32.171l-15.717 11.786z" fill="#c38325"/><path d="m64 320-16 8-8 16-16 8 16 32 32-8 24 8 4.032 16.127a32 32 0 0 0 19.16 21.95l24.808 9.923v16l-16 40h32l16-32v-24l48-16 8-24 32 40 27 16 14.372 40h22.628l-8-56-24-24v-16.4a49.1 49.1 0 0 0 -18.427-38.34 78.763 78.763 0 0 0 -49.2-17.26h-14.773a78.758 78.758 0 0 0 -11.138.792l-32.231 4.6a78.007 78.007 0 0 1 -35.7-3.219l-30.531-10.173-24-32z" fill="#ea9d2d"/><path d="m245.66 277.66c-.49.49-112.05 64.35-112.05 64.35l-30.95 46.43a8 8 0 1 1 -13.32-8.88l32-48a8.08 8.08 0 0 1 2.69-2.51l110.1-62.92z" fill="#0292c9"/><circle cx="72" cy="343" fill="#c38325" r="7"/><path d="m485.657 434.343-23.685-23.685-30.635-38.292-19.361-77.447a7.98 7.98 0 0 0 3.975-6.035l8-72a8.007 8.007 0 0 0 -.406-3.544l-15.571-44.164a69.369 69.369 0 0 0 -11.628-37.11l-12.346-18.554v-12.284l7.251-21.753 10.6-15.9a22.928 22.928 0 0 0 -19.077-35.645h-12.352l-10.8-7.2a28.164 28.164 0 0 0 -42.342 32.34l2.72 8.157v30.7a16.019 16.019 0 0 0 16 16 8 8 0 0 0 -7.155 11.578l5.826 11.653-20.913 28.8a35.25 35.25 0 0 0 -6.313 15.407 8.008 8.008 0 0 0 -.5.849l-17.06 34.12-37.249 22.349-23.166 7.727a8 8 0 0 0 -5.47 7.59v16a8 8 0 0 0 2.343 5.657l.594.593-99.8 57.029-18.237-6.079-22.5-30a8 8 0 0 0 -13.056.362l-14.822 22.238-14.1 7.05a8 8 0 0 0 -3.577 3.577l-6.808 13.615-13.615 6.808a8 8 0 0 0 -3.577 10.733l16 32a8 8 0 0 0 9.095 4.183l29.74-7.435 17.62 5.869 2.967 11.866a39.808 39.808 0 0 0 3.142 8.373l-20.209 15.161a8 8 0 0 0 -2.628 9.371l16 40a7.994 7.994 0 0 0 9.958 4.619l24-8a8 8 0 0 0 5.059-10.12l-6.436-19.31 7.631-7.63 7.216 2.886v16.695l-15.155 30.311a8 8 0 0 0 7.155 11.578h32a8 8 0 0 0 6.656-3.562l16-24a8.005 8.005 0 0 0 1.344-4.438v-26.234l32-10.666v19.359l-15.428 38.57a8 8 0 0 0 7.428 10.971h32a8 8 0 0 0 7.428-5.029l16-40a8 8 0 0 0 .572-2.971v-6.7l11.388 6.752-1.769 3.015-15.276 15.276a8 8 0 0 0 -2.343 5.657v16a8 8 0 0 0 8 8h64a8 8 0 0 0 8-8v-124.687l22.5-22.5 17.907 53.72a8 8 0 0 0 1.189 2.27l46.404 61.863v21.334a8 8 0 0 0 8 8h32a8 8 0 0 0 7.428-5.029l16-40a8 8 0 0 0 -1.771-8.628zm-95.449-267.435 17.639 50.027-7.118 64.056-7.979 1.183-8.3-6.779 4.668-12.874a8.017 8.017 0 0 0 .461-2.191l2.4-35.795a8 8 0 0 0 -.491-3.344l-18.067-48.177a8.931 8.931 0 1 1 16.784-6.106zm-52.955-132.865a12.134 12.134 0 0 1 13.5 0l12.814 8.543a8 8 0 0 0 4.433 1.344h14.774a6.928 6.928 0 0 1 5.765 10.77l-8.82 13.23h-6.775l-5.789-11.578a8 8 0 0 0 -7.155-4.422h-26.234l-1.306-3.919a12.191 12.191 0 0 1 4.793-13.968zm6.747 57.887h-8v-24h19.056l5.789 11.578a8 8 0 0 0 7.155 4.422h4.9l-4.489 13.47a8.006 8.006 0 0 0 -.411 2.53v8h-16v-8a8 8 0 0 0 -8-8zm-17.294 77.433 23.768-32.733a8 8 0 0 0 .681-8.278l-2.211-4.422h22.77l11.311 17c.931 1.4 1.784 2.837 2.574 4.3a24.942 24.942 0 0 0 -27.157 33.4l17.458 46.56-1.527 22.81h-30.373v-40a8 8 0 0 0 -4.422-7.155l-5.9-2.952a19.25 19.25 0 0 1 -6.968-28.53zm-86.706 84.403 18.53-6.176a8.075 8.075 0 0 0 1.586-.73l40-24a8.006 8.006 0 0 0 3.039-3.282l9.725-19.449a35.04 35.04 0 0 0 13.638 12.071l1.482.741v.389l-21.054 28.072-37.917 15.167a7.991 7.991 0 0 0 -1.467.772l-18.542 12.365-9.02-9.019zm2.343 31.891a8 8 0 0 0 10.095 1l23.307-15.538 39.226-15.69a8 8 0 0 0 3.429-2.629l9.6-12.8v13.837l-29.653 50.833a39.962 39.962 0 0 0 -12.235-5.388l-12.172-3.043a8 8 0 0 0 -9.529 10.291l.844 2.534a28.416 28.416 0 0 1 -3.773 25.539 86.915 86.915 0 0 0 -33.109-6.6h-14.773a87.128 87.128 0 0 0 -12.269.872l-32.232 4.6a70.261 70.261 0 0 1 -20.385-.1l89.953-51.4zm49.829 71.143a57.45 57.45 0 0 0 -9.6-9.786c-2.2-1.756-4.48-3.386-6.825-4.912a44.2 44.2 0 0 0 6.884-27.193 24.292 24.292 0 0 1 9.542 41.891zm-222.112 11.439-25.774 6.444-9.553-19.105 8.845-4.423a8 8 0 0 0 3.577-3.577l6.808-13.615 13.615-6.808a7.994 7.994 0 0 0 3.078-2.717l9.728-14.592 17.216 22.954a8 8 0 0 0 3.87 2.79l14.064 4.687-22.8 34.2-18.2-6.068a7.992 7.992 0 0 0 -4.474-.17zm30.566 93.786-10.921-27.3 15.662-11.747a39.937 39.937 0 0 0 7.407 4.937l-6.431 6.431a8 8 0 0 0 -1.932 8.187l5.47 16.41zm133.957 17.975h-14.767l11.612-29.029a8 8 0 0 0 .572-2.971v-30.7l2.939-8.818 13.061 16.325v21.652zm37.417-4.687 13.657-13.656a8 8 0 0 0 1.191-1.531l7.146 19.874h-21.994zm39 4.687-12.48-34.707a8 8 0 0 0 -3.449-4.175l-25.733-15.256-31.091-38.862a8 8 0 0 0 -13.836 2.468l-6.736 20.206-44.205 14.736a8 8 0 0 0 -5.47 7.59v29.578l-12.281 18.422h-14.775l10.211-20.422a7.994 7.994 0 0 0 .845-3.578v-24a8 8 0 0 0 -5.029-7.428l-24.808-9.923a24.061 24.061 0 0 1 -14.37-16.462l-3.173-12.694 26.647-39.972.734.245a86.467 86.467 0 0 0 39.358 3.549l32.232-4.6a70.831 70.831 0 0 1 10.009-.715h14.776a71.018 71.018 0 0 1 44.2 15.506 40.9 40.9 0 0 1 15.424 32.094v16.4a8 8 0 0 0 2.343 5.657l22.115 22.114 6.318 44.229zm11.346-125.657a8 8 0 0 0 -2.346 5.657v60.686l-16-16v-13.086a57.141 57.141 0 0 0 -3.873-20.693l.39-.293a40.5 40.5 0 0 0 15.336-40.14 40.042 40.042 0 0 0 -5.81-14.1l30.557-52.374h30.971l-4.087 11.273a8 8 0 0 0 2.459 8.921l12.257 10.018c-.824.991-1.678 1.957-2.592 2.871zm48.81-26.182 19.762-19.761a58.328 58.328 0 0 0 6.128-7.229l19.193 76.773a7.978 7.978 0 0 0 1.514 3.056l28.525 35.657-15.017 25.028-40.082-53.444zm87.427 143.839h-18.583v-13.784l17.588-29.316 13.025 13.025z"/><circle cx="72" cy="343" r="7"/></g>
          </ServiceHeader>
          <ServiceHeader state={state} service="sit" label="Sitting">
            <g><path d="M24,468V331.472a40,40,0,0,1,25.6408-37.3339L128,264h80l78.3592,30.1381A40,40,0,0,1,312,331.472V488H44A20,20,0,0,1,24,468Z" style={{fill: "#c85aed"}} /><g><path d="M197.1194,296H138.8806a20,20,0,0,1-19.1565-25.747L136,216h64l16.2759,54.253A20,20,0,0,1,197.1194,296Z" style={{fill: "#ffd4cf"}} /><path d="M200,216H136l-4.6665,15.5552L151.8418,241.81a36.185,36.185,0,0,0,16.1,3.8008h0a36.1735,36.1735,0,0,0,16.2637-3.8829l20.4113-10.3359Z" style={{fill: "#fbb8b0"}} /><path d="M232,88H104v93.459a30,30,0,0,0,16.5836,26.8328l38.4151,19.2076a20,20,0,0,0,17.98-.0458L215.5529,207.92A30,30,0,0,0,232,181.1559Z" style={{fill: "#ffd4cf"}} /></g><path d="M80,68V24h92a20,20,0,0,1,20,20V88H100A20,20,0,0,1,80,68Z" style={{fill: "#fa5d3f"}} /><path d="M240,88H192V40l34.3246,11.4415A20,20,0,0,1,240,70.4152Z" style={{fill: "#d3463d"}} /><path d="M112,24h28a20,20,0,0,1,20,20V88a0,0,0,0,1,0,0H132a20,20,0,0,1-20-20V24A0,0,0,0,1,112,24Z" style={{fill: "#fed672"}} /><path d="M232,120h14a10,10,0,0,1,10,10v20a10,10,0,0,1-10,10H232a0,0,0,0,1,0,0V120A0,0,0,0,1,232,120Z" style={{fill: "#ffd4cf"}} /><path d="M80,120H94a10,10,0,0,1,10,10v20a10,10,0,0,1-10,10H80a0,0,0,0,1,0,0V120A0,0,0,0,1,80,120Z" transform="translate(184 280) rotate(-180)" style={{fill: "#ffd4cf"}} /><g><rect x="128" y="128" width="16" height="16"/><rect x="192" y="128" width="16" height="16"/></g><path d="M168,198c-13.458,0-24-8.7852-24-20h16c0,1.41,3.0371,4,8,4s8-2.59,8-4h16C192,189.2148,181.458,198,168,198Z"/><path d="M192,432H152v56h96A56,56,0,0,0,192,432Z" style={{fill: "#ffd4cf"}} /><g><path d="M488,376H456L440,264h0a48,48,0,0,1,48,48Z" style={{fill: "#d3463d"}} /><path d="M438,336H352l-16.7-44.5337A30,30,0,0,0,307.21,272H276.3606a20,20,0,0,0-17.8884,11.0557L232,328l-32,16v18a30,30,0,0,0,30,30h54.21l-10.1052,48H246a30,30,0,0,0-30,30v18H438a50,50,0,0,0,50-50V386A50,50,0,0,0,438,336Z" style={{fill: "#fa5d3f"}} /><path d="M232,328v20a20,20,0,0,1-20,20H200V344Z" style={{fill: "#fed672"}} /><path d="M320,272v52a20,20,0,0,1-20,20h0a20,20,0,0,1-20-20V272" style={{fill: "#fed672"}} /><g><path d="M192,424H88V376H72v56a8,8,0,0,0,8,8h64v40H44a12.0134,12.0134,0,0,1-12-12V331.4717a32.1911,32.1911,0,0,1,20.5122-29.8672L111.0247,279.1A28.0722,28.0722,0,0,0,138.88,304H197.12a28.0722,28.0722,0,0,0,27.8557-24.9l12.1531,4.6741,5.7432-14.9336L221.77,260.7251l-12.2371-40.79,9.6338-4.8785A37.8185,37.8185,0,0,0,240,181.1562V168h6a18.0206,18.0206,0,0,0,18-18V130a18.0206,18.0206,0,0,0-18-18h-6V96a8,8,0,0,0,8-8V70.415a27.9661,27.9661,0,0,0-19.1455-26.5634L197.9813,33.5605A28.0446,28.0446,0,0,0,172,16H80a8,8,0,0,0-8,8V48H88V32h16V68a27.8317,27.8317,0,0,0,2.7042,12H100A12.0134,12.0134,0,0,1,88,68V64H72v4A28.0431,28.0431,0,0,0,96,95.7148V112H90a18.0206,18.0206,0,0,0-18,18v20a18.0206,18.0206,0,0,0,18,18h6v13.459a37.7925,37.7925,0,0,0,21.0059,33.9883l9.3979,4.6989L114.23,260.7251,46.7686,286.6709A48.2876,48.2876,0,0,0,16,331.4717V468a28.0314,28.0314,0,0,0,28,28H192V480H160V440h32a48.0674,48.0674,0,0,1,13.7183,1.9873l4.5634-15.3359A64.0739,64.0739,0,0,0,192,424Zm54-296a2.0023,2.0023,0,0,1,2,2v20a2.0023,2.0023,0,0,1-2,2h-6V128ZM232,70.415V80H200V51.1L223.7944,59.03A11.9863,11.9863,0,0,1,232,70.415ZM184,44V80H168V44a27.8317,27.8317,0,0,0-2.7042-12H172A12.0134,12.0134,0,0,1,184,44ZM120,68V32h20a12.0134,12.0134,0,0,1,12,12V80H132A12.0134,12.0134,0,0,1,120,68ZM90,152a2.0023,2.0023,0,0,1-2-2V130a2.0023,2.0023,0,0,1,2-2h6v24Zm22,29.459V96H224v85.1562a21.8959,21.8959,0,0,1-12.0615,19.627l-38.5747,19.5332a12.0548,12.0548,0,0,1-10.7876.0274l-38.4151-19.2071A21.8816,21.8816,0,0,1,112,181.459Zm43.42,53.1953a28.1153,28.1153,0,0,0,25.1724-.0645l14.4389-7.3115,13.5821,45.2735A12,12,0,0,1,197.12,288H138.88a12,12,0,0,1-11.4937-15.4482l13.5427-45.1426Z"/><path d="M496,312a56.0632,56.0632,0,0,0-56-56,8.0005,8.0005,0,0,0-7.9194,9.1318l8.9926,62.9488q-1.5267-.08-3.0732-.0806H376v16h62a42.0475,42.0475,0,0,1,42,42v52a42.0475,42.0475,0,0,1-42,42H344V470a22.0248,22.0248,0,0,1,22-22h26a8,8,0,0,0,8-8V430a22.0248,22.0248,0,0,1,22-22h26V392H422a38.0433,38.0433,0,0,0-38,38v2H366a38.0433,38.0433,0,0,0-38,38v10H224V470a22.0248,22.0248,0,0,1,22-22h66V432H283.9653l6.7367-32h.7135a48.0929,48.0929,0,0,0,39.0591-20.1006l28.0352-39.25a7.9968,7.9968,0,0,0,.9809-7.458l-16.7-44.5342A38.1761,38.1761,0,0,0,307.21,264H276.3608a27.8449,27.8449,0,0,0-24.91,15.2129l-25.1436,42.6894-29.8847,14.9424A8,8,0,0,0,192,344v18a38.0433,38.0433,0,0,0,38,38h44.3507l-6.7367,32H246a38.0433,38.0433,0,0,0-38,38v18a8,8,0,0,0,8,8H438a58.0656,58.0656,0,0,0,58-58V386a58.1408,58.1408,0,0,0-.8629-10H496ZM312,280.5342V324a12,12,0,0,1-24,0V280h19.21A21.75,21.75,0,0,1,312,280.5342Zm-88,60.41V348a12.0134,12.0134,0,0,1-12,12h-4V348.9443Zm-10.9739,35.0369A28.0336,28.0336,0,0,0,240,348V330.1812l25.3652-43.065c.0928-.1582.1807-.3193.2622-.4824A11.9417,11.9417,0,0,1,272,280.8108V324a28,28,0,0,0,56,0V294.7837l15.0127,40.0347L317.4551,370.6a32.0644,32.0644,0,0,1-26.04,13.4H230A21.9658,21.9658,0,0,1,213.0261,375.9812Zm244.6907-44.5305-8.334-58.3394A40.073,40.073,0,0,1,480,312v34.0393A58.1692,58.1692,0,0,0,457.7168,331.4507Z"/></g></g><path d="M348,224l-38.1421-38.1416a20,20,0,1,1,28.2842-28.2852L348,167.4316l9.8579-9.8584a20,20,0,0,1,28.2842,28.2852Z" style={{fill: "#fed672"}} /><path d="M348,232a7.9761,7.9761,0,0,1-5.6567-2.3428l-38.1421-38.1416a28,28,0,0,1,39.5976-39.6L348,156.1172l4.2007-4.2a28,28,0,0,1,39.5981,39.5986l-38.1421,38.1416A7.9761,7.9761,0,0,1,348,232Zm-24-72.2842a12,12,0,0,0-8.4854,20.4854L348,212.6865l32.4854-32.4853a12,12,0,0,0-16.9708-16.9707l-9.8574,9.8574a7.9995,7.9995,0,0,1-11.3144,0l-9.8579-9.8584A11.923,11.923,0,0,0,324,159.7158Z"/><path d="M423,108.2124,396.3934,81.606a15,15,0,0,1,21.2129-21.2129L423,65.7866l5.3935-5.3935A15,15,0,0,1,449.6063,81.606Z" style={{fill: "#fed672"}} /><path d="M423,116.2129a8,8,0,0,1-5.6567-2.3428L390.7368,87.2627A23,23,0,0,1,423,54.4766a23,23,0,0,1,32.2632,32.7861L428.6567,113.87A8,8,0,0,1,423,116.2129Zm-16-52.21a6.9968,6.9968,0,0,0-4.95,11.9453L423,96.8994l20.95-20.9512A7,7,0,1,0,434.05,66.05l-5.3931,5.3936a7.9995,7.9995,0,0,1-11.3144,0l-5.3936-5.3946A6.9771,6.9771,0,0,0,407,64.0029Z"/></g>
          </ServiceHeader>
          {/* <Service service="groom" label="Grooming"> */}
          {/*   <g><path d="m224 176-48 48q-2.145-1.065-4.16-2.31a57.872 57.872 0 0 1 -27.72-45.69q-.12-1.875-.12-3.78v-20.22h48z" fill="#f3af3e"/><path d="m208 200-20 12-16.16 9.69a57.872 57.872 0 0 1 -27.72-45.69h40.61l19.33 19.94z" fill="#5b3726"/><path d="m368 152v20.22q0 1.905-.12 3.78a57.929 57.929 0 0 1 -31.88 48l-48-48 32-24z" fill="#f3af3e"/><path d="m367.88 176a57.883 57.883 0 0 1 -24.58 43.65l-23.3-11.65-16-8 3.94-4.06 19.33-19.94z" fill="#5b3726"/><path d="m344 240v104h-176v-104a88 88 0 0 1 176 0z" fill="#f3af3e"/><path d="m360 32-8 32 88 56v216h32v-232l-104-69.333z" fill="#b9b9b9"/><path d="m472 360v48a80 80 0 0 1 -80 80h-272a80 80 0 0 1 -80-80v-48z" fill="#6fbbc6"/><rect fill="#8aced8" height="32" rx="16" width="464" x="24" y="328"/><path d="m346.788 89.862-35.788-53.062 22.435-10.24a28.331 28.331 0 0 1 35.253 9.932 28.332 28.332 0 0 1 -4 36.406z" fill="#cbcbcb"/><path d="m224 288v16a40.056 40.056 0 0 1 -37.61-26.39 40.128 40.128 0 0 1 -18.39-13.65v-23.96h8a23.937 23.937 0 0 0 8 17.87v-9.87h16v16a24.032 24.032 0 0 0 24 24z" fill="#da922a"/><path d="m344 240v23.96a40.128 40.128 0 0 1 -18.39 13.65 40.056 40.056 0 0 1 -37.61 26.39v-16a24.032 24.032 0 0 0 24-24v-16h16v9.87a23.937 23.937 0 0 0 8-17.87z" fill="#da922a"/><path d="m215.351 203.999h25.298v16.001h-25.298z" fill="#d18d28" transform="matrix(.949 -.316 .316 .949 -55.345 82.99)"/><path d="m275.999 199.351h16.001v25.298h-16.001z" fill="#d18d28" transform="matrix(.316 -.949 .949 .316 -6.93 414.386)"/><path d="m291.578 287.155-7.156-14.31a27.932 27.932 0 0 1 -20.422 1.747v-18.592h-16v18.6a27.951 27.951 0 0 1 -20.422-1.75l-7.156 14.31a43.9 43.9 0 0 0 35.578 1.566 43.9 43.9 0 0 0 35.578-1.571z" fill="#744630"/><path d="m192 320a15.915 15.915 0 0 1 -2.14 8h-21.86v-32a24 24 0 0 1 24 24z" fill="#d18d28"/><path d="m344 296v32h-21.86a15.915 15.915 0 0 1 -2.14-8 24 24 0 0 1 24-24z" fill="#d18d28"/><g fill="#c0f5f9"><path d="m232 136h-16a78.415 78.415 0 0 1 4.676-26.718l15.049 5.436a62.45 62.45 0 0 0 -3.725 21.282z"/><path d="m243.009 100.529-13.19-9.058a78.107 78.107 0 0 1 24.365-22.911l5.7-3.42 8.232 13.72-5.7 3.42a62.212 62.212 0 0 0 -19.407 18.249z"/><path d="m272 136h-16a78.415 78.415 0 0 1 4.676-26.718l15.049 5.436a62.45 62.45 0 0 0 -3.725 21.282z"/><path d="m283.009 100.529-13.19-9.058a78.107 78.107 0 0 1 24.365-22.911l5.7-3.42 8.232 13.72-5.7 3.42a62.212 62.212 0 0 0 -19.407 18.249z"/><path d="m312 136h-16a78.415 78.415 0 0 1 4.676-26.718l15.049 5.436a62.45 62.45 0 0 0 -3.725 21.282z"/></g><circle cx="80" cy="264" fill="#b3e4e8" r="32"/><circle cx="48" cy="184" fill="#c0f5f9" r="24"/><circle cx="392" cy="256" fill="#c0f5f9" r="16"/><path d="m96 392a8 8 0 0 1 -8 8h-48v-16h48a8 8 0 0 1 8 8z" fill="#8aced8"/><path d="m128 400h-8a8 8 0 0 1 0-16h8a8 8 0 0 1 0 16z" fill="#8aced8"/><path d="m256 416a8 8 0 0 0 8-8v-8a8 8 0 0 0 -16 0v8a8 8 0 0 0 8 8z" fill="#744630"/><path d="m240 424v-8a8 8 0 0 0 -16 0v8a8 8 0 0 0 16 0z" fill="#744630"/><path d="m280 408a8 8 0 0 0 -8 8v8a8 8 0 0 0 16 0v-8a8 8 0 0 0 -8-8z" fill="#744630"/><path d="m272 440h-8v-8a8 8 0 0 0 -16 0v8h-8a8 8 0 0 0 0 16h32a8 8 0 0 0 0-16z" fill="#744630"/><path d="m384 464a8 8 0 0 1 0-16 48.053 48.053 0 0 0 48-48 8 8 0 0 1 16 0 64.072 64.072 0 0 1 -64 64z" fill="#8aced8"/><path d="m352 464h-8a8 8 0 0 1 0-16h8a8 8 0 0 1 0 16z" fill="#8aced8"/><path d="m168 336h-16a16 16 0 0 1 -16-16 24 24 0 0 1 24-24 24 24 0 0 1 24 24 16 16 0 0 1 -16 16z" fill="#f3af3e"/><path d="m360 336h-16a16 16 0 0 1 -16-16 24 24 0 0 1 24-24 24 24 0 0 1 24 24 16 16 0 0 1 -16 16z" fill="#f3af3e"/><path d="m256 264a16 16 0 0 1 -16-16v-16h32v16a16 16 0 0 1 -16 16z" fill="#5b3726"/><path d="m40 360h432v16h-432z" fill="#69b0bb"/><path d="m299.63 32.278h32v80h-32z" fill="#b9b9b9" transform="matrix(.829 -.559 .559 .829 13.535 188.831)"/><path d="m480 321.376v-217.376a8 8 0 0 0 -3.562-6.656l-104.621-69.744a36.2 36.2 0 0 0 -41.7-8.321l-16.27 7.426-.684-1.014a8 8 0 0 0 -11.105-2.16l-26.531 17.897a8 8 0 0 0 -2.16 11.105l13.933 20.667a78.15 78.15 0 0 0 -17.486 18.272l13.186 9.059a62.278 62.278 0 0 1 13.271-14.031l18.9 28.022-14.493-5.235a78.415 78.415 0 0 0 -4.678 26.713h16a62.447 62.447 0 0 1 3.585-20.867l2.513 3.726a8 8 0 0 0 11.1 2.161l26.53-17.892a8 8 0 0 0 2.161-11.106l-.685-1.014 8.968-8.5 65.828 41.884v195.608h-48a32.036 32.036 0 0 0 -32-32v-48a96.878 96.878 0 0 0 -1.311-15.882 66.247 66.247 0 0 0 25.174-47.606c.091-1.414.137-2.858.137-4.292v-20.22a8 8 0 0 0 -8-8h-48a8.009 8.009 0 0 0 -4.8 1.6l-13.379 10.03a96.091 96.091 0 0 0 -91.644 0l-13.377-10.03a8.009 8.009 0 0 0 -4.8-1.6h-48a8 8 0 0 0 -8 8v20.22c0 1.434.046 2.878.137 4.3a66.243 66.243 0 0 0 25.174 47.6 96.86 96.86 0 0 0 -1.311 15.88v48a32.036 32.036 0 0 0 -32 32h-88a24 24 0 0 0 -8 46.624v41.376a88.1 88.1 0 0 0 88 88h272a88.1 88.1 0 0 0 88-88v-41.362a24.005 24.005 0 0 0 0-45.262zm-153.11-218.095-35.785-53.061 13.266-8.947 35.784 53.062zm32.3-36.188-11.049 10.467-25.224-37.4 13.844-6.318a20.331 20.331 0 0 1 22.426 33.254zm17.603 3.207a36.168 36.168 0 0 0 4.759-16.979l82.448 54.96v211.719h-16v-200a8 8 0 0 0 -3.7-6.75zm-24.793 233.7a16.019 16.019 0 0 1 16 16 8.009 8.009 0 0 1 -8 8h-16a8.009 8.009 0 0 1 -8-8 16.019 16.019 0 0 1 16-16zm-96-15.274a43.8 43.8 0 0 0 32 0v15.274a40.069 40.069 0 0 0 37.612-26.388 39.957 39.957 0 0 0 10.388-5.634v20.327a32.012 32.012 0 0 0 -16 27.695h-128a32.012 32.012 0 0 0 -16-27.695v-20.327a39.957 39.957 0 0 0 10.388 5.634 40.069 40.069 0 0 0 37.612 26.388v-15.274a43.8 43.8 0 0 0 32 0zm-8-40.726v-8h16v8a8 8 0 0 1 -16 0zm98.074-41.238a96.25 96.25 0 0 0 -12.101-22.762h24.607a50.32 50.32 0 0 1 -12.506 22.762zm-194.074-46.762h37.334l15.487 11.611a8 8 0 0 0 9.028.39 80.024 80.024 0 0 1 84.3 0 8 8 0 0 0 9.028-.39l15.489-11.611h37.334v8h-32.73a8 8 0 0 0 -5.745 2.432l-5.438 5.61a8 8 0 0 0 -.238 10.881 79.613 79.613 0 0 1 20.151 53.077 23.943 23.943 0 0 1 -8 17.869v-9.869h-16v16a24.028 24.028 0 0 1 -22.062 23.915c.55-.247 1.1-.49 1.64-.76l-7.156-14.31a27.932 27.932 0 0 1 -20.422 1.747v-3.968a24.039 24.039 0 0 0 16-22.624v-16a8 8 0 0 0 -8-8h-32a8 8 0 0 0 -8 8v16a24.039 24.039 0 0 0 16 22.624v3.976a27.951 27.951 0 0 1 -20.422-1.75l-7.156 14.31c.54.27 1.09.513 1.64.76a24.028 24.028 0 0 1 -22.062-23.92v-16h-16v9.869a23.943 23.943 0 0 1 -8-17.869 79.619 79.619 0 0 1 20.151-53.077 8 8 0 0 0 -.238-10.881l-5.438-5.61a8 8 0 0 0 -5.745-2.432h-32.73zm1.419 24h24.608a96.264 96.264 0 0 0 -12.1 22.762 50.311 50.311 0 0 1 -12.508-22.762zm6.581 120a16.019 16.019 0 0 1 16 16 8.009 8.009 0 0 1 -8 8h-16a8.009 8.009 0 0 1 -8-8 16.019 16.019 0 0 1 16-16zm304 104a72.081 72.081 0 0 1 -72 72h-272a72.081 72.081 0 0 1 -72-72v-8h40a8 8 0 0 0 0-16h-40v-16h416zm13.674-58.347a7.991 7.991 0 0 1 -5.674 2.347h-432a8 8 0 1 1 0-16h94.131a23.943 23.943 0 0 0 17.869 8h16a23.943 23.943 0 0 0 17.869-8h140.262a23.943 23.943 0 0 0 17.869 8h16a23.943 23.943 0 0 0 17.869-8h94.131a8.009 8.009 0 0 1 5.674 13.653z"/><path d="m215.351 203.999h25.298v16.001h-25.298z" transform="matrix(.949 -.316 .316 .949 -55.345 82.99)"/><path d="m275.999 199.351h16.001v25.298h-16.001z" transform="matrix(.316 -.949 .949 .316 -6.93 414.386)"/><path d="m216 136h16a62.45 62.45 0 0 1 3.725-21.282l-15.049-5.436a78.415 78.415 0 0 0 -4.676 26.718z"/><path d="m254.184 68.56a78.107 78.107 0 0 0 -24.365 22.911l13.19 9.058a62.212 62.212 0 0 1 19.407-18.249l5.7-3.42-8.232-13.72z"/><path d="m256 136h16a62.45 62.45 0 0 1 3.725-21.282l-15.049-5.436a78.415 78.415 0 0 0 -4.676 26.718z"/><path d="m40 264a40 40 0 1 0 40-40 40.045 40.045 0 0 0 -40 40zm40-24a24 24 0 1 1 -24 24 24.028 24.028 0 0 1 24-24z"/><path d="m48 152a32 32 0 1 0 32 32 32.036 32.036 0 0 0 -32-32zm0 48a16 16 0 1 1 16-16 16.019 16.019 0 0 1 -16 16z"/><path d="m368 256a24 24 0 1 0 24-24 24.028 24.028 0 0 0 -24 24zm24-8a8 8 0 1 1 -8 8 8.009 8.009 0 0 1 8-8z"/><path d="m120 400h8a8 8 0 0 0 0-16h-8a8 8 0 0 0 0 16z"/><path d="m256 416a8 8 0 0 0 8-8v-8a8 8 0 0 0 -16 0v8a8 8 0 0 0 8 8z"/><path d="m232 432a8 8 0 0 0 8-8v-8a8 8 0 0 0 -16 0v8a8 8 0 0 0 8 8z"/><path d="m280 432a8 8 0 0 0 8-8v-8a8 8 0 0 0 -16 0v8a8 8 0 0 0 8 8z"/><path d="m232 448a8 8 0 0 0 8 8h32a8 8 0 0 0 0-16h-8v-8a8 8 0 0 0 -16 0v8h-8a8 8 0 0 0 -8 8z"/><path d="m384 464a64.072 64.072 0 0 0 64-64 8 8 0 0 0 -16 0 48.053 48.053 0 0 1 -48 48 8 8 0 0 0 0 16z"/><path d="m344 464h8a8 8 0 0 0 0-16h-8a8 8 0 0 0 0 16z"/></g> */}
          {/* </Service> */}
        </ul>
      </div>
      <div className="px-8 py-4 col">
        {services(state.rates)[state.selectedService]}
      </div>
    </section>

    <section
      id="home:choose-us"
      className="px-4 py-8 col w-full"
    >
      <div className="max-w-screen-md w-full">
        <h2 className="text-4xl md:text-6xl py-8 relative text-black">
          Why choose us?
          <div className="bg-pri h-1 rounded-lg" />
        </h2>
        <ul className="list-none list-inside col w-full">
          <C.Item heading="Dogs of all sizes" src="https://media.nature.com/lw800/magazine-assets/d41586-020-01430-5/d41586-020-01430-5_17977552.jpg">
            Dogs of any size are welcome.
          </C.Item>
          <C.Item heading="Safety Oriented" src="https://www.petcareins.com/media/cms_blog_image_thumbnail/blog_images/0001/55/e03470b1000d7cb43abef10fd96983a66e582992.jpeg">
            We understand how important your good boy/girl is to you. That's why we make it a priority to keep your pet safe.
          </C.Item>
          <C.Item heading="Keep you in the loop" src="https://www.washingtonpost.com/resizer/uwlkeOwC_3JqSUXeH8ZP81cHx3I=/arc-anglerfish-washpost-prod-washpost/public/HB4AT3D3IMI6TMPTWIZ74WAR54.jpg">
            You can see your requested services, when they occur, how much they are, and more, all through the <A to="/calendar">calendar.</A>
          </C.Item>
        </ul>
      </div>
    </section>

  </main>
}

const ServiceHeader = ({state, ...props}) => {
  return <li
    onClick={() => changeService({service: props.service})}
    className={"px-2 py-1 rounded-lg cursor-pointer col z-10 " +
      (state.selectedService === props.service ?
      "text-white" : "")}
  >
    <svg className="h-24 w-24 sm:h-32 sm:w-32" viewBox="0 0 512 512">{props.children}</svg>
    <strong className="text-4xl"
      style={{fontFamily: "Lemonada"}}
    >{props.label}</strong>
  </li>
}


const services = ({walking, sitting}) => ({
  walk: <Service
    description="A short walk around the block, a long walk around the neighborhood. Whatever you need."
  >
    <strong
      style={{fontFamily: "Lemonada"}}
      className="text-pri price"
    >${walking}</strong>&nbsp;
    per 30 minutes
  </Service>,
  sit: <Service
    description="Something about dog sitting. For a day. A decade. Whatever you need."
  >
    <strong
      style={{fontFamily: "Lemonada"}}
      className="text-pri price"
    >${sitting}</strong>&nbsp;
    per night
  </Service> ,
})

const Service = (props) => {
  const ref = React.useRef()
  React.useEffect(() => {
    if (get(ref, ["current", "children"]).length === 0)
      return

    gsap.fromTo(
      Array.from((ref as any).current.children).slice(0,-1),
      {opacity: 0, y: 20, },
      { stagger: {
        each: 0.1,
        ease: "ease.in",
      },
      opacity: 1, y: 0}
    )
  }, [props])

  return <>
    <div className="absolute transform -translate-y-20 left-0 right-0 bg-pri h-16" />
    <div ref={ref} className="space-y-4 max-w-lg relative text-2xl md:text-4xl h-64 md:h-96 z-10">
      <p>{props.description}</p>
      <p className="text-2xl sm:text-3xl md:text-4xl flex justify-center h-16 items-center">
        {props.children}
      </p>
      <span className="absolute bottom-0 text-xs w-full flex justify-end">Rates are subject to change.</span>
    </div>
  </>
}

const C = {
  Testimonial(props) {
    return <li className="relative bg-gray-3 h-full w-64 flex-none rounded-lg px-4 py-2">
      <blockquote>
        <p>{props.msg}</p>
        <cite className="absolute bottom-0 right-0 mr-4 mb-2">-{props.cite}</cite>
      </blockquote>
    </li>
  },
  Item(props) {
    return <li
      className="col my:fade-in md:flex-row-reverse py-5 w-full md:w-auto"
    >
      <div className="w-64 h-64 relative rounded-sm overflow-hidden">
        <div
          className="w-full h-full absolute bg-cover bg-center
          transform duration-200 hover:scale-105"
          style={{backgroundImage: `url(${props.src})`}}
        />
      </div>
      <div className="py-10 md:py-0 w-full px-4 md:px-0 md:pr-2 md:w-96 mr-2">
        <h3 className="">{props.heading}</h3>
        <p className="text-xl md:text-2xl">{props.children}</p>
      </div>
    </li>
  },
}


// Dogwalking home page for image
/*
<section
      className="h-screen text-white h-128
      overflow-hidden relative px-4 col w-full flex flex-col items-center justify-center"
    >

      <div
        className="h-full inset-0 absolute"
        style={{
          background: "no-repeat center url(https://www.dogingtonpost.com/wp-content/uploads/2014/04/walkingthedog2.jpg)",
          backgroundSize: "cover",
          filter: "brightness(0.3)",
        }}
      />

      <div className="w-full relative z-10 text-pri flex items-center justify-center">
        <Logo className="h-24" />
        <h1 className="font-pri" style={{fontSize: "10rem"}} >Paw Pals</h1>
      </div>

      <div className="h-10" />

      <div
        className="z-10 relative w-full flex flex-col items-center"
      >
        <h2 className="font-pri text-6xl">Friendly Dog Walking</h2>
        <h3
          style={{fontFamily: "Roboto", color: "rgb(255 255 255 / 70%)"}}
        >for residents of montgomery county</h3>
      <div className="h-10" />

        <div className="w-full flex justify-center items-center">
          <Link
            to="/calendar"
            className="mt-16 inline-block text-black rounded-md"
          >
            <Button className="text-2xl p-2 bg-pri hover:bg-pri-3">Schedule a service</Button>
          </Link>
        </div>
      </div>

      <nav className="absolute right-0 h-full flex flex-col justify-center px-2 mr-4">
        <SocialMedia className="w-8 h-8 hidden sm:block" fill="#20c9c6" />
      </nav>

    </section>
    */

