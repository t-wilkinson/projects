import React from "react"
import gsap from "gsap"
import {useStore} from "../store"
import {
 BlurryImg,
} from "../components"
import * as api from "../api"

export default () => {
  React.useEffect(() => {
    gsap.fromTo(
      "h2",
      {y: 100},
      {y: -100,
        scrollTrigger: {
          trigger: "h2",
          scrub: 0.5,
        },
      },
    )
  }, [])

  React.useEffect(() => {
    document.title = "Projects | Klean Studios"
  }, [])

  React.useEffect(() => {
    // @ts-ignore
    const blurryImageLoad = new BlurryImageLoad()
    blurryImageLoad.load()
  }, [])
      // <Project key={i} group={v.group} href={v.href} src={v.src} alt={v.alt} />

  const [projects, setProjects] = React.useState([
    { group: 'one', href: '', src: '', alt: '', }
  ])
  // React.useEffect(() => {
  //  api.getProjectsProjects(
  //    (res)=>setProjects(res.sort((v1,v2)=>v1.order-v2.order)),
  //    (err)=>{},
  //  )
  // }, [])

  return <main
    className="w-full col"
  >
    <section
      className="w-full h-screen max-w-screen-xl flex flex-col justify-between relative
      mx-4 sm:mx-8 md:mx-16
      sm:px-8 sm:py-4
      md:px-32 md:py-16
      "
    >
      <BlurryImg
        className="h-screen w-full md:w-auto absolute right-0 top-0 object-cover -z-10"
        // className="h-screen pt-32 pb-16 w-full sm:px-8 md:px-0 md:w-auto absolute right-0 top-0 object-cover -z-10"
        style={{transform: "scaleX(-1)"}}
        src="max-1.jpg"
        alt="Portfolio picture of Max"
      />
      <div
        className="h-screen pt-32 pb-16 absolute right-0 w-full top-0 object-cover -z-10"
        style={{background: "rgba( 255 255 255 / 50%)"}}
      />
      <h2
        className="
        text-6xl
        sm:text-7xl sm:max-w-screen-sm
        md:text-7xl md:max-w-screen-md
        lg:max-w-screen-lg lg:text-8xl
        pt-16
        mt-8 ml-8
        md:mt-0 md:ml-0
        "
      >
        check out my projects
      </h2>
      <div
        className="text-sm sm:text-base md:text-lg md:w-128 -z-10 space-y-4 px-8 py-4 text-white bg-black"
      >
        <p><strong>Founder of Klean Studios:</strong> Maxwell Klimczak</p>
        <p><strong>Resides:</strong> Roanoke, VA</p>
        <p><strong>Education:</strong>  Currently pursuing a music degree with emphasis in composition at Mary Baldwin University</p>
        <p><strong>Quote:</strong> “Since my childhood I have been obsessed with music. I like how it brings people together and allows us to express ourselves. That is why I want to help people bring their musical ideas to life. It is a language that we can all speak with our own unique slang representing where we came from and how we were brought up.”</p>
      </div>
    </section>
    <section
      className="w-full px-8
      my-64
      sm:max-w-screen-sm mg:max-w-screen-md lg:max-w-screen-lg xl:max-w-screen-xl
      "
    >
      {projects.map((v,i)=>
      <Project key={i} group={v.group} href={v.href} src={v.src} alt={v.alt} />
        )}
      {/* <Project group="Jackie McBean" alt="Jackie McBean soundcloud" href="https://soundcloud.com/jackie-mcbean/jackie-m-anymore" src="https://i1.sndcdn.com/artworks-6b0Pqxebn3TLkJ84-GWlwkQ-t500x500.jpg" /> */}
      {/* <Project group={`[your name]`} alt="Your name here." href="/register" src="/assets/images/record.jpg" /> */}
      <div
        className="w-full h-1 bg-black"
        style={{height: "1px"}}
      />
    </section>
  </main>
}

const Project = (props) => {
  const refs = {
    img: React.useRef(null),
    div: React.useRef(null),
  }
  const userAgent = useStore(s=>s.root.userAgent)

  React.useEffect(() => {
    if (userAgent('mobile')) return
    let div: any = refs.div.current
    if (div === null) return
    let span = div.querySelector("span")

    if (span.scrollWidth > span.clientWidth) {
      span.insertAdjacentText("afterbegin",  ` ${span.innerHTML} `)
    }
  }, [refs.div, userAgent])

  React.useEffect(() => {
    if (userAgent('mobile')) return
    let img: any = refs.img.current
    let div: any = refs.div.current
    if (img === null || div === null) return

    gsap.set(img, {xPercent: -50, yPercent: -50})
    let tl = gsap.timeline()
    let marquee = gsap.timeline()

    const mousemove = (e) => {
      let dX = e.pageX - div.offsetLeft
      let dY = e.pageY - div.offsetTop
      let brightness = 1 + gsap.utils.normalize(
        0, 200,
        Math.abs(e.movementX) + Math.abs(e.movementY)
      )
      tl.clear()
      tl.to(img, {
        duration: 0.2,
        ease: "linear",
        filter: `brightness(${brightness}) blur(${1.1 * brightness}px)`,
        rotation: 15 * gsap.utils.normalize(0, 35, e.movementX)
      }, 0)
      .to(img, {
        ease: "power1.out",
        duration: 0.8,
        filter: "brightness(1) blur(0px)",
        rotation: 0,
      }, ">")
      tl.to(
        img,
        { duration: 1,
          ease: "power1.out",
          x: dX,
          y: dY,
        }, 0)
    }

    div.addEventListener("mouseenter", (e) => {
      let span = div.querySelector("span")
      gsap.timeline({defaults: {duration: 0.2}})
      .to(img, {opacity: 1})
      .fromTo(span, {yPercent: 0}, {ease: "power1.in", yPercent: -100}, 0)
      .fromTo(span, {yPercent: 100}, {ease: "power1.out", yPercent: 0}, '>')

      if (span.scrollWidth > span.clientWidth) {
        marquee.fromTo(
          span,
          {x: 0}, {delay: 0.5, duration: 10, x: -span.scrollWidth/2, repeat: -1, ease: "linear"}
        )
      }

      div.addEventListener("mousemove", mousemove)

      mousemove(e)
    })

    div.addEventListener("mouseleave", (e) => {
      gsap.timeline({defaults: {duration: 0.2}})
      .to(img, {opacity: 0})
      mousemove(e)
      marquee.progress(0).clear()
      div.removeEventListener("mousemove", mousemove)
    })

  }, [refs.img, refs.div, userAgent])

  return <a
    ref={refs.div}
    className={`w-full py-8 relative items-center block h-full
    `}
    href={props.href}
    target="_blank"
    rel="noopener noreferrer"
  >
    <div
      className={`w-full bg-black absolute top-0 `}
      style={{height: "1px"}}
    />
    <img
      style={userAgent('mobile') ?  {filter: "brightness(0.5)"} : {}}
      className={`absolute -z-10 top-0 object-contain flex justify-center items-center
      ${userAgent('mobile') ?
        "opacity-1 w-full h-full h-128"
      : "opacity-0 w-128 h-96"
      }
      `}
      ref={refs.img}
      alt={props.alt}
      src={props.src}
    />
    <div className={`w-full h-full overflow-hidden ${userAgent('mobile') ? "my-10 py-24" : "h-full"} `}
    >
      <span
        className={`font-pri whitespace-pre
        text-2xl sm:text-6xl md:textl-7xl lg:text-8xl xl:text-9xl relative
        ${userAgent("mobile")
          ? "text-center py-4 underline bg-white bg-opacity-50 col justify-center w-full"
          : "h-min block"}
        `}
      >{props.group}
      </span>
    </div>
  </a>
}
