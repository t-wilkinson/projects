import React from 'react'
import Link from 'next/link'
import { useTrail, animated } from 'react-spring'

import { Heading, ButtonLink, Wrapper, TextLink } from './'
import Icon from './Icon'

export const Header = ({activeSection}) => {
  const [scrollOffset, setScrollOffset] = React.useState(0)

  React.useEffect(() => {
    const onScroll = () => {
      setScrollOffset(window.scrollY)
    }
    document.addEventListener('scroll', onScroll)

    return () => {
      document.removeEventListener('scroll', onScroll)
    }
  }, [])

  return (
    <Wrapper
      outer={{
        className: `bg-bg transition-all duration-700 ease-out`,
        style: scrollOffset < 25 ? {} : {
          boxShadow: `
            0px 1px 1px rgba(0,0,0,0.12),
            0px 4px 4px rgba(0,0,0,0.12)
          `
        },
      }}
    >
      <header className="flex flex-col sm:flex-row text-light w-full py-4 items-center justify-between">
        <AnimateOnEnter
          className="flex-row items-center space-x-4 self-start sm:self-auto"
          config={inView => ({
            to: {
              x: !inView ? -48 : 0,
              opacity: !inView ? 0 : 1,
            },
          })}
        >
          <Link href="/">
            <a className="inline">
            <span className="flex flex-row items-center space-x-2">
              <Icon icon="treywilkinson" />
              <span className="link"><h2>Trey Wilkinson</h2></span>
            </span>
          </a>
          </Link>
        </AnimateOnEnter>
        <AnimateOnEnter
          className="flex-row items-center space-x-4 self-end sm:self-auto"
          config={inView => ({
            to: {
              x: !inView ? 48 : 0,
              opacity: !inView ? 0 : 1,
            },
          })}
        >
          <TextLink active={activeSection === 'home'} href="/">Home</TextLink>
          <TextLink active={activeSection === 'my-work'} href="#my-work">My work</TextLink>
          <TextLink active={activeSection === 'about-me'} href="#about-me">About me</TextLink>
          <ButtonLink active={activeSection === 'hire-me'} href="#hire-me">Hire me</ButtonLink>
        </AnimateOnEnter>
      </header>
    </Wrapper>
  )
}

export const Footer = () => {
  return <div />
}

const animateEnterConfig = (inView: boolean) => ({
  delay: 500,
  config: {
    frequency: 0.15,
  },
  to: {
    y: !inView ? 48 : 0,
    opacity: !inView ? 0 : 1,
  },
})

export const AnimateOnEnter = ({
  config = () => ({}),
  children,
  ...props
}: {
  config?: (inView: boolean) => any
  children: any
  [x: string]: any
}) => {
  const [inView, setInView] = React.useState(false)
  children = React.Children.toArray(children)
  const trail = useTrail(children.length, {
    ...animateEnterConfig(inView),
    ...config(inView),
  } as any)
  const ref = React.useRef(null)

  React.useEffect(() => {
    const onScroll = () => {
      const rect = ref.current.getBoundingClientRect()
      if ((rect.top > 0 && rect.top < window.innerHeight) || (rect.bottom > 0 && rect.bottom < window.innerHeight)) {
        setInView(true)
        window.removeEventListener('scroll', onScroll)
      }
    }
    window.addEventListener('scroll', onScroll)
    onScroll()
    return () => window.removeEventListener('scroll', onScroll)
  }, [])

  return (
    <div ref={ref} {...props}>
      {trail.map((transition, i) => (
        <animated.div key={i} style={transition as any}>
          {children[i]}
        </animated.div>
      ))}
    </div>
  )
}

export const A = AnimateOnEnter

export const Section = ({ className='', label, children = null as React.ReactNode, animate = false }) => {
  return (
    <Wrapper outer={{className}}>
      <div
        id={label.toLowerCase().replace(' ', '-')}
        className="relative"
        style={{
          top: '-100px',
        }}
      />
      <section className="mb-32">
        <Heading>{label}</Heading>
        {animate ? <AnimateOnEnter>{children}</AnimateOnEnter> : children}
      </section>
    </Wrapper>
  )
}
