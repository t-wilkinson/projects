import React from 'react'
import Link from 'next/link'

import { A } from './Layout'

export const Wrapper = ({
  children,
  inner = {},
  outer = {},
}: {
  children: React.ReactNode
  inner?: any
  outer?: any
}) => {
  outer.className = 'w-full items-center px-4 xl:px-0' + ' ' + (outer.className || '')
  inner.className = 'w-full max-w-screen-lg' + ' ' + (inner.className || '')

  return (
    <div {...outer}>
      <div {...inner}>{children}</div>
    </div>
  )
}

const around = x => (x === 1 ? x + 3 : x - 3)
const points = [
  [1, 1],
  [179, 1],
  [179, 59],
  [1, 59],
  [1, 1],
]
  .map(([x, y], i) =>
    i % 2 === 0 ? `${x},${around(y)} ${around(x)},${y}` : `${around(x)},${y} ${x},${around(y)}`
  )
  .join(' ')

export const ButtonContent = ({ children }) => (
  <div className="button bg-pri overflow-hidden px-[1em] py-[0.75em] w-max text-light rounded-md">
    <svg viewBox="0 0 180 60" className="button__svg" preserveAspectRatio="none">
      <polyline points={points} className="bg-line" />
      <polyline points={points} className="bg-line" />
    </svg>
    {children}
  </div>
)

export const Button = ({ children, className = '', ...props }) => {
  return (
    <button {...props} className={`font-bold w-max ${className}`}>
      <ButtonContent>{children}</ButtonContent>
    </button>
  )
}

export const ButtonLink = ({ active=false, children, href, className = '' }) => {
  return (
    <Link href={href}>
      <a className={`${className} ${active ? 'font-bold' : ''}`}>
        <ButtonContent>{children}</ButtonContent>
      </a>
    </Link>
  )
}

export const TextLink = ({ active=false, children, className = '', href, ...props }) => {
  return (
    <Link href={href}>
      <a className={`link ${className} ${active ? 'font-bold' : ''}`} {...props}>
        {children}
      </a>
    </Link>
  )
}

export const Heading = ({ children }) => {
  return (
    <h3 className="text-light text-5xl font-bold mb-8">
      <A>
        <div className="bg-pri w-[1.5em] theme h-[0.25em] mb-1" />
        {children}
      </A>
    </h3>
  )
}
