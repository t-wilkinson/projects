import React from 'react'
import Section from './Section'
import StackItem from './StackItem'
import { theme } from '../../components'

export default function () {
  return (
    <Section to="skills">
      <StackItem
        alt="Frontend web development tools"
        src="/assets/images/logos/javascript.png"
        description="Frontend"
        tools={[
          <>
            <strong>ReactJS</strong> / RxJS / GSAP
          </>,
          <>
            <strong>Next.js</strong>
          </>,
          <>
            <strong>TypeScript</strong> / JavaScript
          </>,
          'CSS3 / Tailwind CSS / SCSS / PostCSS',
          'Gulp',
          'HTML5',
        ]}
        color={theme.sec}
      />
      <StackItem
        alt="Haskell logo"
        src="/assets/images/logos/node-js-hex.png"
        description="Backend"
        tools={[
          <>
            <strong>Node.js</strong> / Next.js / Strapi (CMS)
          </>,
          <>
            <strong>Haskell</strong> / Servant / Persist
          </>,
          'REST / GraphQL',
          'PostgreSQL / Redis',
        ]}
        color={theme.pri1}
      />
      <StackItem
        alt="Dev Ops tools"
        src="/assets/images/logos/linux.png"
        description="Dev-Ops"
        tools={[
          <>
            <strong>Linux</strong> / Terminal / Bash / Python
          </>,
          'AWS / Digital Ocean',
          'Nix',
          'Git / Github',
        ]}
        color="#eb999e"
      />
    </Section>
  )
}
