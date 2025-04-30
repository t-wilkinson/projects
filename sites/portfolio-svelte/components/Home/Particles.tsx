import React from 'react'
import { useFrame } from '@react-three/fiber'
import * as THREE from 'three'

const SEPARATION = 100
const AMOUNTX = 50
const AMOUNTY = 50
let count = 0

const vertexShader = `
attribute float scale;

void main() {
  vec4 mvPosition = modelViewMatrix * vec4( position, 1.0 );
  gl_PointSize = scale * ( 300.0 / - mvPosition.z );
  gl_Position = projectionMatrix * mvPosition;
}
`

const fragmentShader = `
uniform vec3 color;

void main() {
  if ( length( gl_PointCoord - vec2( 0.5, 0.5 ) ) > 0.475 ) discard;
  gl_FragColor = vec4( color, 1.0 );
}
`

const initParticles = (size: number) => {
  const numParticles = size * size
  const positions = new Float32Array(numParticles * 3)
  const scales = new Float32Array(numParticles)

  // Positions and Scales
  let i = 0
  let j = 0
  for (let ix = 0; ix < size; ix++) {
    for (let iy = 0; iy < size; iy++) {
      positions[i] = ix * SEPARATION - (size * SEPARATION) / 2 // x
      positions[i + 1] = 0 // y
      positions[i + 2] = iy * SEPARATION - (size * SEPARATION) / 2 // z

      scales[j] = 1

      i += 3
      j++
    }
  }

  // Geometry
  const geometry = new THREE.BufferGeometry()
  geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3))
  geometry.setAttribute('scale', new THREE.BufferAttribute(scales, 1))

  // Material
  const material = new THREE.ShaderMaterial({
    uniforms: {
      color: { value: new THREE.Color(0x473d8f) },
    },
    vertexShader,
    fragmentShader,
  })

  return { geometry, material }
}

export default ({ xxx, size, }) => {
  const particles = initParticles(size)

  useFrame(() => {
    const scale = xxx.current
    const positions = particles.geometry.attributes.position.array as Array<number>
    const scales = particles.geometry.attributes.scale.array as Array<number>

    let i = 0
    let j = 0

    for (let ix = 0; ix < size; ix++) {
      for (let iy = 0; iy < size; iy++) {
        positions[i + 1] = Math.sin((ix + count) * 0.3) * 50 + Math.sin((iy + count) * 0.5) * 50

        scales[j] =
          (Math.sin((ix + count) * 0.3) + 1) * scale + (Math.sin((iy + count) * 0.5) + 1) * scale

        i += 3
        j++
      }
    }

    // const q = new THREE.Quaternion()
    // q.setFromAxisAngle(new THREE.Vector3(0, 0, 0.1), Math.PI)
    // particles.geometry.applyQuaternion(q)
    particles.geometry.attributes.position.needsUpdate = true
    particles.geometry.attributes.scale.needsUpdate = true

    count += 0.05
  })

  return <points geometry={particles.geometry} material={particles.material} />
}
