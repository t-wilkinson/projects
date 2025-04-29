import React from 'react'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faAt, faGlobe, faMapMarker } from '@fortawesome/free-solid-svg-icons'
import { faGithub } from '@fortawesome/free-brands-svg-icons'
import './resume.css'

const theme = {
  // blue: '#003D74',
  // darkBlue: '#002E58',
  blue: '#473d8f',
  darkBlue: '#2e284e',
  secFont: "'Roboto', sans-serif",
  priFont: "'Roboto Slab', serif",
}

function Education({ school, time, gpa }) {
  return (
    <div style={{ padding: '0 1rem', marginTop: '1rem' }}>
      <div style={{ fontWeight: 'bold' }}>{school}</div>
      <div>
        <small>{time}</small>
      </div>
      <div>{gpa}</div>
    </div>
  )
}

export default () => {
  React.useEffect(() => {
    document.title = 'Trey Wilkinson | Resume'
  }, [])

  return (
    <div style={{ display: 'grid', placeItems: 'center', background: '#ddd' }}>
      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '30% 70%',
          fontFamily: theme.secFont,
          width: '1000px',
          border: `1px solid black`,
          margin: '2rem 0',
          boxShadow: `0.25rem 0.25rem 0 0 ${theme.darkBlue}`,
          background: 'white',
        }}
      >
        <aside style={{ color: 'white', background: theme.blue }}>
          <div style={{ padding: '1rem 1rem 0rem' }}>
            <div
              style={{
                fontWeight: '700',
                fontFamily: theme.priFont,
                fontSize: '2rem',
                textTransform: 'uppercase',
              }}
            >
              Trey Wilkinson
            </div>
            <div style={{ fontWeight: '700', lineHeight: '2rem' }}>
              Full Stack Developer
            </div>
          </div>
          <SideSectionH>Personal Info</SideSectionH>
          <div style={{ padding: '1rem 1rem 0' }}>
            <ul style={{ margin: '0', paddingLeft: '0', listStyle: 'none' }}>
              <li style={{ whiteSpace: 'pre' }}>
                <FontAwesomeIcon icon={faAt} />{' '}
                <ExtLink
                  href="mailto:winston.trey.wilkinson@gmail.com"
                  txt="winston.trey.wilkinson@gmail.com"
                />
              </li>
              <li style={{ whiteSpace: 'pre' }}>
                <FontAwesomeIcon icon={faGlobe} />{' '}
                <ExtLink
                  href="https://treywilkinson.com"
                  txt="treywilkinson.com"
                />
              </li>
              <li style={{ whiteSpace: 'pre' }}>
                <FontAwesomeIcon icon={faGithub} />{' '}
                <ExtLink
                  href="https://github.com/t-wilkinson"
                  txt="t-wilkinson"
                />
              </li>
              <li style={{ whiteSpace: 'pre' }}>
                <FontAwesomeIcon icon={faMapMarker} /> Bethesda MD, 20817
              </li>
            </ul>
          </div>

          <SideSectionH>Skills</SideSectionH>
          <SideSection label="Frontend" progress={80}>
            <li>JavaScript · TypeScript</li>
            <li>React · React Native</li>
            <li>CSS3 · SCSS · HTML5</li>
            <li>Responsive</li>
          </SideSection>

          <SideSection label="Backend" progress={80}>
            <li>Node.js · Next.js · Express</li>
            <li>REST · GraphQL</li>
            <li>SQL · Redis · MongoDB</li>
            <li>Nginx · Strapi</li>
          </SideSection>

          <SideSection label="Others">
            <li>AWS</li>
            <li>Git · Nix</li>
            <li>Linux</li>
            <li>CI · CD</li>
            <li>Python · Haskell · Go · Rust</li>
          </SideSection>

          <SideSectionH>Interests</SideSectionH>
          <SideSection label="Computers">
            <li>Machine Learning</li>
            <li>Information Security</li>
            <li>System Administration</li>
          </SideSection>

          <SideSection label="Math">
            <li>Category Theory</li>
            <li>Homotopy Type Theory</li>
            <li>Calculus</li>
            <li>Linear Algebra</li>
          </SideSection>

          <SideSection label="Others">
            <li>Electric Guitar</li>
          </SideSection>

          <SideSectionH>Education</SideSectionH>
          <Education
            school="Mary Baldwin University"
            time="2018 - 2019"
            gpa="3.6 GPA"
          />
          <Education
            school="Walter Johnson High School"
            time="2013 - 2018"
            gpa=""
          />
        </aside>
        <main style={{ color: 'black', padding: '1rem 1rem' }}>
          <p style={{ marginTop: '0.5rem' }}>
            A focused, driven, and adaptable individual with a passion for
            learning, creating, and problem-solving. A natural and proactive
            leader. Three years of experience in web development and many
            related JavaScript frameworks.
          </p>

          <MainSectionH>Projects</MainSectionH>
          <MainSection
            heading={
              <ExtLink
                href="http://ai.treywilkinson.com"
                txt="Neural Style Transfer"
              />
            }
            subheading="NST implementation"
            date1="2021-01"
            date2="2021-01"
          >
            <li>
              NST uses a trained deep neural network to combine the style of one
              image and the content of another.
            </li>
            <li>Used React to quickly develop an interesting UI.</li>
            <li>Used Next.js to power both the frontend and backend.</li>
            <li>
              Wrote a script in Python using PyTorch to implement a NST
              algorithm which is applied to user-provided images.
            </li>
          </MainSection>

          <MainSection
            heading={
              <ExtLink href="https://kleanstudio.com" txt="Klean Studios" />
            }
            subheading="Music recording studio"
            date1="2020-06"
            date2="2020-12"
          >
            <li>
              Used React, TypeScript, and Tailwind to develop a tailored UI and
              UX.
            </li>
            <li>
              Used Node.js and Express to develop a REST compliant architecture.
            </li>
            <li>
              Created a user dashboard, integrated with Stripe, and an admin
              dashboard, to automate most business and site functionality.
            </li>
            <li>
              Used PostgreSQL to provide searching functionality for the admin
              dashboard.
            </li>
            <li>
              Automated hosting on an EC2 instance running Nginx with Jenkins
              and my Linux knowledge.
            </li>
          </MainSection>

          <MainSection
            heading={
              <ExtLink
                href="http://penguins.treywilkinson.com"
                txt="Penguin Sweaters"
              />
            }
            subheading="Promote penguin conservation efforts"
            date1="2020-12"
            date2="2021-01"
          >
            <li>Drove the UI and UX with React, TypeScript, and SCSS.</li>
            <li>
              Used a Next.js server to query Strapi endpoints using GraphQL.
            </li>
            <li>Hosted site on an EC2 instance running Nginx.</li>
          </MainSection>

          <MainSectionH>Experience</MainSectionH>
          <MainSection
            heading="Window Cleaning"
            subheading="Wells Window Cleaning"
            date1="2015"
            date2="2020"
          >
            <li>
              Professionally cleaned windows (mostly during summer) with a small
              team at residences of high portfolio individuals in the Washington
              DC area.
            </li>
            <li>
              Developed attention to detail and strong work ethic as I strived
              to provide only the best results for clients.
            </li>
            <li>
              Learned risk management and communication in order to work safely
              and effectively.
            </li>
            <li>
              Developed problem-solving techniques and creative thinking as I
              accomplished difficult tasks even when lacking the necessary
              tools.
            </li>
          </MainSection>

          <MainSection
            heading="Entrepreneurial Art Internship"
            subheading="Arts on the Block"
            date1="2014"
            date2="2015"
          >
            <li>
              Created commissioned artwork at a non-profit business for
              companies in the Washington DC area.
            </li>
            <li>
              Worked with a group of individuals to design, create, and install
              commissioned artwork, which required time management,
              communication, planning, and resourcefulness.
            </li>
          </MainSection>

          <MainSection
            heading="Eagle Scout"
            subheading="Award earned in 2016"
            date1="2008"
            date2="2016"
          >
            <li>
              Conceptualized personal community service project, fundraised from
              local organizations. Lead, organized, and collaborated with ten
              volunteers over two weeks to provide lasting services for a local,
              under-funded elementary school.
            </li>
            <li>
              Consistently lead and managed activities with over 15 individuals,
              requiring organization, communication, and risk management.
            </li>
          </MainSection>
        </main>
      </div>
    </div>
  )
}

function ExtLink({ href, txt }) {
  return (
    <a href={href} target="_blank" rel="noopener noreferrer">
      {txt}
    </a>
  )
}

function SideSectionH({ children }) {
  return (
    <h3
      style={{
        background: theme.darkBlue,
        fontFamily: theme.priFont,
        margin: '1rem 0 0',
        padding: '0.5rem 1rem',
        fontSize: '1.5rem',
      }}
    >
      {children}
    </h3>
  )
}

function ProgressBar({ progress }) {
  return (
    <div
      style={{
        position: 'relative',
        height: '1ex',
        background: theme.darkBlue,
      }}
    >
      <div
        style={{
          position: 'absolute',
          background: 'white',
          height: '100%',
          top: '0',
          left: '0',
          right: `${100 - progress}%`,
        }}
      />
    </div>
  )
}

function SideSection({ label = null, children, progress = null }) {
  return (
    <div style={{ padding: '0 1rem', marginTop: '1rem' }}>
      {label ? <strong style={{ fontWeight: '700' }}>{label}</strong> : null}
      {progress ? <ProgressBar progress={progress} /> : null}
      <ul style={{ margin: '0.5rem 0 1rem 0', padding: '0 1.75rem' }}>
        {children}
      </ul>
    </div>
  )
}

function MainSectionH({ children }) {
  return (
    <h3
      style={{
        fontFamily: theme.priFont,
        borderTop: '2px solid #ddd',
        borderBottom: '2px solid #ddd',
        padding: '0.25rem 0',
        textTransform: 'uppercase',
        color: theme.blue,
        fontWeight: '700',
        fontSize: '1.5rem',
      }}
    >
      {children}
    </h3>
  )
}

function MainSection({ date1, date2, heading, subheading, children }) {
  return (
    <div
      className="main-section"
      style={{ display: 'grid', gridTemplateColumns: '8em 1fr' }}
    >
      <div>{date1} -</div>
      <div style={{ fontSize: '1.25rem', color: theme.blue }}>{heading}</div>
      <div>{date2}</div>
      <div>
        <div style={{ fontWeight: '700' }}>{subheading}</div>
        <ul style={{ padding: '0 1rem' }}>{children}</ul>
      </div>
    </div>
  )
}
