import React from 'react'
import { animated } from '@react-spring/three'

const palette = {
  pri: '#5037FF',
  sec: '#473D8F',
  bg: '#1C2124',
  light: '#EEECFF',
  mono: {
    default: '#',
    '700': '#3A364E',
  },
}

const Donut = ({ size, ...props }) => {
  const ref = React.useRef<any>()
  const position = props.translate.to((translate: number) => [
    0,
    (3 - size) ** 3 * 1 + translate * 150,
    0,
  ])

  const color = props.color.to({
    range: [0, 1],
    output: [palette.pri, palette.sec],
  })
  // const colorMap = useLoader(TextureLoader, 'threeTone.jpg')
  // colorMap.minFilter = THREE.NearestFilter
  // colorMap.magFilter = THREE.NearestFilter

  return (
    <animated.mesh {...props} ref={ref} position={position} rotation={[Math.PI / 2, 0 , 0]}>
      <torusGeometry args={[2 ** size * 3, size ** 2, size * 8, size * 25]} />
      <animated.meshToonMaterial color={color} />
    </animated.mesh>
  )
}

export default Donut
