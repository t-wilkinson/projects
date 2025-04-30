import React from 'react'
import styled, { keyframes, ThemeProvider } from 'styled-components'
import gsap from 'gsap'
import TextPlugin from 'gsap/TextPlugin'
import { C, theme } from '../../components'
import Projects from './Projects'
import Skills from './Skills'
import Section from './Section'
import { toCaps } from '../../utils'

gsap.registerPlugin(TextPlugin)

export default (props) => {
  const refs = {
    txt: React.useRef(null),
    cursor: React.useRef(null),
  }

  React.useEffect(() => {
    document.title = 'Trey Wilkinson'
  }, [])

  React.useEffect(() => {
    const cursor = refs.cursor.current
    const txt = refs.txt.current
    const child = txt.children
    if (cursor === null || txt === null) return
    // console.log(Array.from(child).slice(-1)[0])
    // Array.from(child).slice(-1)[0].style.color = 'black'

    gsap.to(
      cursor,
      { opacity: 0, duration: 1, ease: 'power2.inOut', repeat: -1 },
      0
    )
    gsap
      .timeline({ ease: 'power1.in' })
      .to(child[0], { duration: 1, text: 'Hi' }, 1.0)
      .to(child[0], { duration: 0.2, text: 'Hi,' }, '>0.75')
      .set(cursor, { y: '1.5em' }, '>0.1')
      // .to(
      //   child[1],
      //   {
      //     duration: 2,
      //     text: 'Thanks for visiting.',
      //     yoyo: true,
      //     repeat: 1,
      //     repeatDelay: 0.25,
      //   },
      //   '>'
      // )
      .to(child[1], { duration: 2, text: 'I\'m Trey Wilkinson.' }, '>')
      .fromTo(
        '.see-work *, .see-work',
        { opacity: 0 },
        {
          stagger: 0.5,
          opacity: 1,
          duration: 1,
          onComplete: () =>
            document.querySelector('.arrow').classList.add('animate'),
        },
        '>'
      )
  }, [refs.cursor, refs.txt])

  return (
    <ThemeProvider theme={theme}>
      <Main>
        <Aside className="aside">
          <PageLink to="intro">
            <header className="me">
              <strong className="me__name">Trey Wilkinson</strong>
              <span className="me__position">Software Engineer</span>
            </header>
          </PageLink>
          <div className="seperator" />
          <nav className="nav">
            <PageLink to="projects" />
            <PageLink to="skills" />
            <PageLink to="about" />
            <PageLink to="contact" />
            <a href="/resume" target="_blank" className="nav__item">
              Resume
            </a>
            <li className="nav__social-media">
              <a
                className="nav__social-media--mail"
                href="mailto:winston.trey.wilkinson@gmail.com"
                target="_blank"
                rel="noopener noreferrer"
                onClick={(e) => {
                  e.preventDefault()
                  navigator.clipboard.writeText(
                    'winston.trey.wilkinson@gmail.com'
                  )
                }}
              >
                {C.email}
                <span className="nav__social-media--copy">Copy</span>
                <span className="nav__social-media--copied">Copied</span>
              </a>
              <a
                href="https://github.com/t-wilkinson"
                target="_blank"
                rel="noopener noreferrer"
              >
                {C.github}
              </a>
              <a
                href="https://www.linkedin.com/in/trey-wilkinson-24b081210/"
                target="_blank"
                rel="noopener noreferrer"
              >
                {C.linkedin}
              </a>
            </li>
          </nav>
        </Aside>
        <Intro id="intro">
          {/* <SLoading /> */}
          <div className="intro">
            <div className="txt" ref={refs.txt}>
              <span /> <span /> <span /> <span />
            </div>
            <span ref={refs.cursor}>_</span>
          </div>
          <a
            href="#projects"
            label="Navigate to the projects section"
            className="see-work"
            onClick={(e) => {
              e.preventDefault()
              document.querySelector('#projects').scrollIntoView(true)
            }}
            style={{ opacity: 0 }}
          >
            <span>See my work.</span>
            <svg className="arrow" fill="white" viewBox="0 0 69 100">
              <path
                transform="rotate(90,266.21964,113.78034)"
                d="m 250.80721,341.57166 c -0.001,-0.002 -0.003,-0.003 -0.005,-0.005 l -29.06338,-28.92339 c -2.17996,-2.16995 -5.69988,-2.15995 -7.86984,0.019 -2.16995,2.17995 -2.15995,5.69988 0.018,7.86983 l 19.51359,19.41859 h -75.4014 c -3.06993,0 -5.55988,2.48995 -5.55988,5.55988 0,3.06994 2.48995,5.55988 5.55988,5.55988 h 75.4014 l -19.51259,19.41859 c -2.17995,2.16995 -2.18995,5.68988 -0.019,7.86983 2.16996,2.17996 5.68988,2.18996 7.86984,0.019 l 29.06438,-28.92338 c 0.001,-0.002 0.003,-0.004 0.005,-0.005 2.17995,-2.16996 2.16995,-5.70988 -0.001,-7.87983 z"
              />
            </svg>
          </a>
        </Intro>
        <Projects />
        <Skills />
        <Section to="about">
          <ul>
            <li>
              <h2
                style={{
                  color: theme.pri,
                  borderLeft: `0.25rem solid ${theme.pri}`,
                  padding: '0.5rem 0 0.5rem 1rem',
                }}
              >
                I am a focused and driven individual with a serious passion to
                learn and create.
              </h2>
            </li>
            <li>
              <h3>Interests</h3>
              <p>
                I love learning about the functional pieces of websites, such as
                the internet, automating administrative tasks, data security,
                and providing an exceptional user experience.
              </p>
              <p>
                When not developing websites, I enjoy playing the electric
                guitar alongside songs by my favorite guitarists like Jimi
                Hendrix, Jimmy Page, and David Gilmore.
              </p>
              <p>
                Additionally, math subjects like category theory, topology, and
                abstract algebra quench my thirst for abstract and difficult
                concepts. As such, I consistently find myself learning more
                about these, primarily through graduate-level books.
              </p>
              <p>
                Computational physics is another passion of mine as it combines
                so many things I love: math, (theoretical) physics,
                visualizations, computers, and pushing the boundaries.
              </p>
            </li>
          </ul>
        </Section>
        <Section to="contact">
          <address>
            <Info label="Email:">
              <a
                target="_blank"
                rel="noopener noreferrer"
                href="mailto:winston.trey.wilkinson@gmail.com"
              >
                {C.email} winston.trey.wilkinson@gmail.com
              </a>
            </Info>
            <Info label="Github:">
              <a
                target="_blank"
                rel="noopener noreferrer"
                href="https://github.com/t-wilkinson"
              >
                {C.github} GitHub.com/t-wilkinson
              </a>
            </Info>
            <Info label="LinkedIn:">
              <a
                target="_blank"
                rel="noopener noreferrer"
                href="https://www.linkedin.com/in/trey-wilkinson-24b081210/"
              >
                {C.linkedin} LinkedIn.com/in/trey-wilkinson-24b081210
              </a>
            </Info>
          </address>
        </Section>
      </Main>
    </ThemeProvider>
  )
}

function Info({ label, children }) {
  return (
    <li>
      <span>{label}</span>
      <span>{children}</span>
    </li>
  )
}

const PageLink = ({ className = '', to, children = toCaps(to) }) => (
  <a
    href={`#${to}`}
    className={`nav__item ${className}`}
    label={`Navigate to the ${toCaps(to)} section`}
    onClick={(e) => {
      e.preventDefault()
      document.querySelector(`#${to}`).scrollIntoView(true)
    }}
  >
    {children}
  </a>
)

const Main = styled.main`
  overflow-x: hidden;
  padding-left: 200px;
  * {
    font-family: 'Roboto', sans-serif;
    box-sizing: border-box;
    font-size: 1.5rem;
  }
  a {
    text-decoration: none;
  }
  .heading {
    margin: 0;
    padding: 2rem 2rem;
    background: ${(p) => p.theme.pri};
    font-size: 4rem;
    @media (max-width: 768px) {
      font-size: 2rem;
      text-align: center;
    }
    font-weight: 100;
    color: white;
  }

  @media (max-width: 768px) {
    padding-left: 0;
    .aside {
      display: none;
    }
  }
`

const bouncedown = keyframes`
0% { transform: translate(0, 36px); }
75% { transform: translate(0, 36px); }
92.5% { transform: translate(0, 86px); }
100% { transform: translate(0, 76px); };
`

const Intro = styled.section`
  position: relative;
  width: 100%;
  height: 100vh;
  display: grid;
  place-items: center;
  background: ${(p) => p.theme.gray};

  .svg {
    height: 100%;
    width: 100%;
    position: absolute;
    display: none;
  }

  .see-work {
    position: absolute;
    bottom: 0;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-items: between;
    height: 200px;
    padding: 2rem;

    span {
      font-size: 1rem;
      color: white;
      position: relative;
    }
    span::after {
      background: ${(p) => p.theme.pri};
      display: block;
      content: '';
      position: absolute;
      bottom: 0;
      width: 100%;
      height: 2px;
    }
    .arrow {
      height: 3.5rem;
      transform: translate(0, 36px);
    }
    .animate {
      animation: ${bouncedown} alternate infinite 1.5s ease-in-out;
    }
  }

  .intro {
    z-index: 10;
    display: flex;
    justify-content: center;
    // width: 1000px;
    width: 100%;

    * {
      font-size: clamp(1rem, 5.5vw, 5rem);
    }
    span {
      color: white;
      height: 1em;
      padding: 0.75em 0;
      font-weight: 900;
    }
    .txt {
      display: flex;
      flex-direction: column;
      align-items: center;

      span {
        text-align: center;
      }
    }
  }
`

const Aside = styled.aside`
  position: fixed;
  left: 0;
  top: 0;
  bottom: 0;
  width: 200px;
  background: #2e284e;
  z-index: 1;

  .me {
    padding: 0.5rem 1rem;
    .me__name,
    .me__position {
      margin: 0;
    }
    .me__name {
      color: white;
    }
    .me__position {
      color: rgb(255 255 255 / 60%);
      font-size: 1rem;
    }
    &:hover .me__name {
      text-decoration: underline;
      text-decoration-color: ${(p) => p.theme.pri};
    }
  }

  strong {
    color: ${(p) => p.theme.pri};
    display: block;
    font-size: 2rem;
  }

  .seperator {
    height: 1px;
    width: 100%;
    background: black;
  }

  .nav {
    padding-top: 0.5rem;
    .nav__item {
      list-style: none;
      padding: 0.75rem 1rem;
      &:hover {
        background: #3a444b;
        color: white;
      }
      font-size: 1rem;
      display: block;
      text-decoration: none;
      color: rgb(255 255 255 / 60%);
      display: block;
    }
    .nav__social-media {
      height: 2.5rem;
      display: flex;
      fill: rgb(255 255 255 / 60%);
      margin-left: 1rem;

      svg {
        border-radius: 2px;
        height: 2.5rem;
        width: 2.5rem;
        padding: 0.5rem;
        fill: rgb(255 255 255 / 60%);
        &:hover {
          background: #3a444b;
          fill: white;
        }
      }

      .nav__social-media--copy,
      .nav__social-media--copied {
        display: none;
        position: absolute;
        color: #3a444b;
        left: 0;
        opacity: 1;
      }

      .nav__social-media--mail:hover .nav__social-media--copy {
        display: block;
      }

      * + * {
        margin-left: 1rem;
      }
    }
  }
`
