import React from 'react'
import { useSpring, to } from 'react-spring'
import { useTrail, config, animated } from '@react-spring/three'
import { Canvas, useThree, useFrame, extend } from '@react-three/fiber'
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls'

// import Donut from './Donut'
import Particles from './Particles'
// import { TextureLoader } from 'three/src/loaders/TextureLoader'

const palette = {
  pri: '#5037FF', // '#2F12FF',
  sec: '#473D8F',
  bg: '#1C2124',
  light: '#EEECFF',
  mono: {
    default: '#',
    '700': '#3A364E',
  },
}

extend({ OrbitControls })

const CameraControls = () => {
  const {
    camera,
    gl: { domElement },
  } = useThree()
  const controls = React.useRef<any>()
  React.useLayoutEffect(() => {
    // camera.position.z = 5
    // camera.position.set(-47.6, -179, -100)
    // camera.rotation.set(2.08, -0.23, 2.76)
  }, [])

  useFrame(() => {
    // console.log(state.camera)
    // controls.current.update()
  })

  return null
  // @ts-ignore
  // return <orbitControls ref={controls} args={[camera, domElement]} enablePan={true} />
}

export const Animation = () => {
  const donuts = [1, 2, 3, 4, 5]
  const trail = useTrail(donuts.length, {
    loop: { reverse: true },
    config: config.gentle,
    from: { translate: 0, color: 0 },
    to: { translate: 1, color: 1 },
  })
  let xxx = React.useRef(1.4)

  // Probably don't need react-spring for this (its overkill)
  const [parallax, api] = useSpring(() => ({
    from: {
      x: 0,
      y: 0,
      scale: 1,
      mx: 0,
      my: 0,
      scroll: 0,
    },
  }))

  React.useEffect(() => {
    const onScroll = () => {
      const scrollY = window.scrollY / window.innerHeight
      api.start({
        to: {
          x: Math.sin(scrollY + Math.PI) * 2,
          y: scrollY * 4.2,
          scale: Math.sin(scrollY * 5) + 1,
          scroll: scrollY,
        },
        config: config.default,
      })

      // const scale = Math.exp(-scrollY * 8) * 1.4
      const scale = 2 / (scrollY + 1) - 1
      xxx.current = scale < 0.01 ? 0 : scale
    }

    const onMouseMove = (e: MouseEvent) => {
      api.start({
        to: {
          mx: e.clientX / window.innerWidth - 0.5,
          my: e.clientY / window.innerHeight - 0.5,
        },
        config: config.molasses,
      })
    }

    window.addEventListener('scroll', onScroll)
    window.addEventListener('mousemove', onMouseMove)
    return () => {
      window.removeEventListener('scroll', onScroll)
      window.removeEventListener('mousemove', onMouseMove)
    }
  }, [])

  const position = to([parallax.x, parallax.y, parallax.mx, parallax.my], (x, y, mx, my) => [
    x + -mx * 3 + 2,
    y + my * 3,
    0,
  ]) as any
  const scale = to([parallax.scale], scale => scale) as any

  // to([parallax.scroll], scroll => {
  //   const scale = Math.exp(-scroll) * 2
  //   // const scale = - Math.log(scroll + 1)
  //   // TODO: Is there a better way to do this?
  //   console.log(scale)
  //   xxx.current = scale < 0.05 ? 0 : scale
  // }) as any

  return (
    <div className="fixed inset-0">
      <Canvas>
        <CameraControls />
        {/* <animated.group position={position} scale={scale}> */}
        {/*   <pointLight position={[-3, 1, 0]} /> */}
        {/*   {/1* <pointLight position={[1, 1, 0]} color={palette.pri}/> *1/} */}
        {/*   <group rotation={[0.8, 0, 0.4]} position={[0, 0, 0]} scale={0.01}> */}
        {/*     {trail.map((props, i) => ( */}
        {/*       <Donut key={i} size={i + 1} {...props} /> */}
        {/*     ))} */}
        {/*   </group> */}
        {/* </animated.group> */}
        <group position={[0, -100, 0]} scale={0.3}>
          <Particles size={50} xxx={xxx}/>
        </group>
      </Canvas>
    </div>
  )
}
