import React from "react"
import styled from "styled-components"

export default (props) => {
  return <div>
    <h1>Hi</h1>
    <SMain />
  </div>
}

const SMain = styled(Main)`
overflow-x: hidden;
img {
  width: 500px;
  height: 300px;
  object-fit: cover;
  filter: url(#noise);
}
#Gradient1 {
  stop:nth-of-type(1) { stop-color: red; }
  stop:nth-of-type(2) { stop-color: blue; stop-opacity: 0; }
  stop:nth-of-type(3) { stop-color: purple; }
}
`

function Main({className}) {
  return <main className={className}>
    <svg width="1600" height="800" viewBox="0 0 100 100">
      <defs>
        <radialGradient id="Gradient2" cx="0.5" cy="0.5" r="0.4" fx="0.25" fy="0.10" spreadMethod="reflect">
          <stop offset="0%" stopColor="rebeccapurple" />
          <stop offset="70%" stopColor="cyan" stopOpacity="0.7" />
          <stop offset="100%" stopColor="pink" stopOpacity="0.3" />
        </radialGradient>
        <linearGradient id="Gradient1" x1="0" y1="0" x2="1" y2="0.0">
          <stop offset="0%" />
          <stop offset="50%" />
          <stop offset="100%" />
        </linearGradient>
        <g fill="white">
          <mask id="Mask1">
            <rect x="0" y="0" width="80" height="80" rx="20" ry="6" fill="white" opacity="0.5" />
          </mask>
        </g>
        <filter id="Filter1" filterUnits="userSpaceOnUse">
          <feGaussianBlur in="SourceGraphic" stdDeviation="2 100" result="blur"/>
          {/* <feOffset dx="30" dy="30" /> */}
        </filter>
      </defs>

      <g transform="scale(0.25) matrix(1 0 0 1 0 0)">
        <rect x="0" y="0" width="200" height="200" fill="blue" />
        <circle cx="100" cy="100" r="50" stroke="yellow" strokeWidth="40" strokeOpacity=".5" fill="red" />
      </g>
      <image x="0" y="0" width="100" height="100" filter="url(#Filter1)" transform="rotate(45)"
         xlinkHref="https://developer.mozilla.org/static/img/favicon144.png"/>
      <text x="0" y="0" fontSize="4">Masks (with fill="white") == Clips</text>
      <path d="M 10 10 H 90 V 90 h -80 Z" mask="url(#Mask1)" fill="url(#Gradient1)" stroke="#000"/>
      <rect
        rx="10" ry="10" width="50" height="50" x="10" y="10" fill="url(#Gradient2)"
        transform="translate(20,-10) scale(0.5) rotate(10) skewY(50) skewX(50) matrix(1 1 1 0 0 0)"
      />
      <path d="M 10 60 C 20 80, 40 10, 50 80 L 70 10"
        strokeWidth="5" strokeLinecap="round" strokeLinejoin="miter"
        stroke="black" fill="transparent"
      />
      <polyline points="40 140 80 100 120 140" stroke="black" strokeWidth="20"
          strokeLinecap="round" fill="none" strokeLinejoin="miter"/>
    </svg>
    <img alt="" src="https://therightsofnature.org/wpContent/uploads/2018/01/turkey-3048299_1920-1366x550.jpg"/>
  </main>
}
