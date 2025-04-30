import React from 'react'
import { gsap } from 'gsap'
import { SplitText } from 'gsap/SplitText'
import { ScrollTrigger } from 'gsap/ScrollTrigger'
import { useStore } from '../store'
import { A, BlurryImg } from '../components'

export default () => {
  let root = useStore((s) => s.root)
  let refs = {
    booth: React.useRef(null),
    mic: React.useRef(null),
  }

  React.useEffect(() => {
    document.title = 'Klean Studios'
  }, [])

  React.useEffect(() => {
    const timeline = gsap.timeline({
      defaults: { duration: 3, ease: 'power3.in', delay: 0.3 },
    })
    if (root.userAgent('safari')) {
      timeline
        .fromTo(
          'tspan',
          { stroke: '#0000' },
          {
            stagger: { amount: 2, grid: [2, 7], from: 'start' },
            stroke: '#fff',
          },
          0
        )
        .fromTo(
          'tspan',
          { fill: 'black' },
          {
            fill: 'white',
            duration: 2.5,
            stagger: { amount: 2, grid: [2, 7], from: 'start' },
          },
          '2.0'
        )
        .fromTo('#scroll span', { color: '#0000' }, { color: '#fff' }, '>0.5')
        .fromTo('#scroll polygon', { stroke: '#0000' }, { stroke: '#fff' }, '<')
        .fromTo('#scroll polygon', { fill: '#0000' }, { fill: '#fff' }, '<')
      return
    } else {
      // Not safari

      timeline
        .fromTo(
          'tspan',
          { stroke: '#0000' },
          { duration: 0, stroke: '#fff' },
          0
        )
        .fromTo(
          'tspan',
          { strokeDashoffset: '200', strokeDasharray: '200' },
          {
            strokeDashoffset: '0',
            stagger: { each: 0.6, grid: [2, 7], from: 'start' },
            duration: 2.5,
          }
        )
        .fromTo(
          'tspan',
          { fill: 'black' },
          {
            fill: 'white',
            duration: 2.5,
            stagger: { amount: 2, grid: [2, 7], from: 'start' },
          },
          '2.0'
        )

        .to('#scroll', { display: 'flex', duration: 0 }, 0)
        .fromTo('#scroll span', { color: '#0000' }, { color: '#fff' }, '>2.4')
        .fromTo('#scroll polygon', { stroke: '#0000' }, { stroke: '#fff' }, '<')
        .fromTo('#scroll polygon', { fill: '#0000' }, { fill: '#fff' }, '<')
        .fromTo(
          '#scroll',
          { y: 40 },
          { y: 80, repeat: -1, yoyo: true, duration: 1, ease: 'sine.inOut' },
          '<'
        )

      gsap.utils.toArray('h2').forEach((el: any, i) => {
        let split = new SplitText(el, { type: 'lines' })
        el.style.position = 'relative'
        el.style.display = 'inline-block'
        el.style.color = '#00000000'
        el.style.overflowX = 'hidden'
        el.style.overflowY = 'hidden'

        split.lines.forEach((line: any, i) => {
          let div = document.createElement('div')
          line.style.fontFamily = '\'Abril Fatface\''
          div.setAttribute(
            'style',
            `background: black; position: absolute;
              top: 0; left: 0;
              width: ${el.clientWidth}px;
              height: ${line.clientHeight}px`
          )
          line.appendChild(div)

          let tl
          tl = gsap
            .timeline({ defaults: { ease: 'power3.inOut' } })
            .fromTo(div, { x: '-100%' }, { delay: i / 5, x: '0' }, '0')
            .to(line, { duration: 0, color: 'black' }, '>')
            .fromTo(div, { x: '0' }, { x: '110%' }, '>')
            .pause()

          ScrollTrigger.create({
            animation: tl,
            trigger: el,
            start: 'top bottom',
            end: 'bottom top',
            toggleActions: 'restart reset restart reset',
          })
        })
      })
    }

    return () => timeline.kill()
  }, [root, root.userAgent])

  React.useEffect(() => {
    if (root.userAgent('safari')) return
    if (refs.booth === undefined) return
    let imgs = Array.from((refs as any).booth.current.querySelectorAll('img'))
    imgs.forEach((el: any, i) => {
      ScrollTrigger.create({
        trigger: el,
        start: 'top top',
        pinSpacing: false,
        anticipatePin: 1,
      })
    })

    let texts = Array.from(
      (refs as any).booth.current.querySelectorAll('.text')
    )
    texts.forEach((el: any, i) => {
      let tl = gsap
        .timeline()
        .fromTo(
          el,
          { visibility: 'hidden' },
          { visibility: 'visible', delay: 0 },
          0
        )
        .fromTo(el.querySelector('h3'), { y: 200 }, { y: 0 }, 0)
        .fromTo(
          new SplitText(el.querySelector('p'), { type: 'lines' }).lines,
          { y: 200 },
          { y: 0, stagger: 0.1 },
          0.2
        )
        .pause()
      if (i === 0) tl.progress(1)
      if (i === 0) el.style.marginTop = '0'
      ScrollTrigger.create({
        animation: tl,
        toggleActions:
          i === 0 ? 'play reset play complete' : 'play reset play reset',
        trigger: el,
        start: 'top top',
        ...(i === 0 ? { end: 'bottom center' } : {}),
        ...(i === 2 ? { end: 'bottom top-=200px' } : {}),
        pin: true,
        pinSpacing: false,
        anticipatePin: 1,
      })
    })
  }, [refs, root])

  const [onResize, setOnResize] = React.useState(0)
  React.useEffect(() => {
    window.onresize = () => {
      if (window.innerWidth > 786) setOnResize((s) => s + 1)
    }
  }, [setOnResize])

  return (
    <>
      <div
        className="col justify-center items-center h-screen overflow-hidden fixed text-center
      -z-10 w-screen bg-black"
      >
        <Welcome className="col w-screen sm:mx-8 md:mx-16 items-center justify-center text-center" />
        <div
          id="scroll"
          className="hidden fixed col justify-center w-full bottom-0"
        >
          <span>scroll</span>
          <svg viewBox="-10 -5 120 80" className="w-10 h-10 mb-16">
            <polygon
              points="0,0 100,0 50,70"
              style={{ stroke: '#0000', strokeWidth: '4', fill: '#0000' }}
            />
          </svg>
        </div>
      </div>
      <div className="h-screen" />
      <main>
        <section className="w-screen relative z-10 bg-white">
          <h2 className="md:m-24 md:mt-0 pt-16 mx-8 sm:mb-24 sm:mx-24 text-4xl sm:text-6xl">
            Mission
          </h2>
          <ul className="px-8 w-screen col" key={onResize}>
            {[
              {
                text: (
                  <span className="w-full text-center">
                    The goal of <strong>Klean Studios</strong> is to provide
                    quality recordings and a welcoming atmosphere to work.
                  </span>
                ),
                alt: 'Working one on one to produce music.',
                src: 'client-5.jpg',
              },
              {
                text: (
                  <span className="w-full text-center">
                    We want to create positive energy and encourage originality
                    in <strong>your music.</strong>
                  </span>
                ),
                className: 'pb-16 md:pb-0',
                alt: 'Singer and pianist creating music.',
                src: 'client-3.jpg',
              },
              {
                text: (
                  <span className="w-full text-center">
                    Reserve recording time using the{' '}
                    <A to="/dashboard">dashboard</A>. You can view when your
                    reserved dates and times are, and pay for services at your
                    convenience.
                  </span>
                ),
                className: 'py-32',
                alt: 'Guitarist and vocalist in the booth.',
                src: 'client-1.jpg',
              },
            ].map(({ alt, text, src, ...props }, i) => (
              <Mission
                alt={alt}
                text={text}
                src={src}
                key={i}
                i={i}
                {...props}
              />
            ))}
          </ul>
        </section>
        <section className="h-screen w-screen" />
        <section ref={refs.booth} className="bg-white w-screen col">
          <div className="w-full max-w-screen-lg" key={onResize}>
            <h2 className="md:m-24 py-16 sm:my-24 ml-8 sm:ml-24 text-4xl sm:text-6xl">
              The Studio
            </h2>
            <BoothText
              heading="Create"
              text="I offer recording, mixing, mastering, and other services to help you reach your creative potential."
              alt="Microphone infront of acoustic foam in dark room with purple back lighting."
              src="client-6.jpg"
            />
            <BoothText
              heading="Quality"
              text="The recording booth is insulated with acoustic foam pads and has a thick curtain to give easy access in and out of the booth while still limiting outside noise during recording."
              alt="Custom-made recording booth with curtain drawn."
              src="client-8.jpg"
            />
            <BoothText
              heading="Audio"
              text="Inside the booth is where the magic happens. You get to show your talent and write your story. To catch a vibe, there are LED lights with a large selection of colors and patterns to compliment your style."
              alt="Dark room with mike surrounded with acoustic foam and blue accent lighting."
              src="client-7.jpg"
            />
          </div>
        </section>
      </main>
    </>
  )
}

const BoothText = (props) => {
  const root = useStore((s) => s.root)

  React.useEffect(() => {
    // @ts-ignore
    new BlurryImageLoad().load()
  }, [])

  return (
    <div className="relative w-full h-screen col">
      <div className="h-screen relative w-full max-w-screen-xl">
        <div
          className="text px-8 md:pl-32 md:pt-16 z-10 overflow-hidden
        h-screen w-full md:max-w-2xl lg:max-w-3xl
        justify-center flex flex-col
        "
          style={{
            marginTop: root.userAgent('safari')
              ? ''
              : `-${window.innerHeight / 2}px`,
          }}
        >
          <div className="absolute left-0 mx-4 right-0 h-96 sm:h-64 md:h-96 opacity-50 bg-white z-10" />
          <h3 className="relative text-6xl md:text-8xl lg:text-9xl overflow-hidden z-10">
            {props.heading}
          </h3>
          <p className="relative text-xl font-black py-4 overflow-hidden z-10">
            {props.text}
          </p>
        </div>
        <div className="overflow-hidden absolute w-full md:w-2/3 top-0 h-full right-0 z-0">
          <BlurryImg
            className={`absolute top-0 right-0 bottom-0 h-full w-full z-0
          ${root.userAgent('Chrome') ? 'object-contain' : 'object-cover'}`}
            style={{ borderRadius: '1px' }}
            src={props.src}
            alt={props.alt}
          />
        </div>
        <div className="absolute top-0 bottom-0 right-0 h-full w-1/2 object-cover z-0" />
      </div>
    </div>
  )
}

const Welcome = (props) => {
  const font = { fontFamily: '\'Abril Fatface\'' }
  const toTspan = (string) =>
    Array.prototype.map.call(string, (c, i) => (
      <tspan key={i} style={font}>
        {c}
      </tspan>
    ))
  return (
    <svg
      viewBox="0 0 56 46"
      style={{ strokeWidth: '0.2', stroke: '#0000' }}
      fill="#0000"
      {...props}
    >
      <text x="6.5" y="18" letterSpacing="-0.3">
        {toTspan('Klean  ')}
      </text>
      <text x="0" y="40" letterSpacing="-0.3">
        {toTspan('Studios')}
      </text>
    </svg>
  )
}

const Mission = ({ alt, text, src, i, className = '' }) => {
  let refs = {
    img: React.useRef(),
    p: React.useRef(),
  }
  let userAgent = useStore((s) => s.root.userAgent)

  React.useEffect(() => {
    // @ts-ignore
    new BlurryImageLoad().load()
  }, [])

  React.useEffect(() => {
    let img = refs.img.current
    if (userAgent('mobile')) return
    if (img === undefined) return

    const dy = gsap.utils.mapRange(0, 1920, 30, 90, window.innerWidth)
    gsap.timeline().fromTo(
      img,
      { y: i % 2 ? -dy : dy },
      {
        y: i % 2 ? dy : -dy,
        scrollTrigger: {
          trigger: img,
          scrub: 1,
          start: 'top bottom',
          end: 'bottom top',
          id: 'img',
        },
      }
    )
  }, [refs.img, i, userAgent])

  React.useEffect(() => {
    if (userAgent('mobile')) return
    let p = refs.p.current
    if (p === undefined) return
    let lines = new SplitText((p as any).querySelector('span'), {
      type: 'lines',
    }).lines
    let tl = gsap
      .timeline()
      .fromTo(lines, { y: 100 }, { y: 0, stagger: 0.2, ease: 'power1.out' })

    ScrollTrigger.create({
      trigger: p,
      animation: tl,
    })
  }, [refs.p, userAgent])

  return (
    <li
      key={src}
      className={`w-full max-w-screen-lg flex items-center justify-between py-16 flex-col
      ${i % 2 ? 'md:flex-row-reverse' : 'md:flex-row'}
      `}
    >
      <div
        ref={refs.img}
        className={`w-full sm:w-96 h-auto ${
          window.innerWidth < 786 && userAgent('mobile') ? '' : className
        }`}
      >
        <img
          className="object-cover"
          src={'/assets/images/' + src}
          alt={alt}
          style={{ boxShadow: '8px 8px 0px rgba(32 32 32 / 0.9)' }}
        />
      </div>
      <div className="w-12" />
      <p
        ref={refs.p}
        className="mt-8 sm:mt-0 text-2xl w-full md:w-128 overflow-hidden flex border px-4 py-4 md:px-16 md:py-8 "
        style={{ boxShadow: '' }}
      >
        {text}
      </p>
    </li>
  )
}
