import React from 'react'
import styled from 'styled-components'
import { toCaps } from '../../utils'

function Section({ to, children, className }) {
  return (
    <section id={to} className={`${className} ${to}`}>
      <h3 className="heading">{toCaps(to)}</h3>
      <div className="children">{children}</div>
    </section>
  )
}

export default styled(Section)`
  min-height: 100vh;
  font-size: 1.5rem;
  // margin: 0 0 2rem 0;

  .children {
    padding: 4rem 0;
  }

  & {
    h3 {
      width: 100%;
    }
    width: 100%;
    display: flex;
    flex-direction: column;
    align-items: center;
    .children {
      width: 100%;
      max-width: 1000px;
    }
  }

  &.projects {
    z-index: 5;
    .children {
      max-width: none;
      padding-left: 2rem;
      padding-right: 2rem;
      display: grid;
      place-content: center;
      // grid-template: 250px / repeat(auto-fit, min(100%, 500px));
      grid-gap: 4rem;
      width: 100%;
      flex-grow: 1;
    }
  }

  &.skills {
    background: #ebe7ff;
    .children {
      display: grid;
      justify-items: center;
      padding: 2rem 2rem;
      @media (max-width: 768px) {
        padding: 2rem 2rem;
      }
    }
  }

  &.about {
    .children {
      h3 {
        margin: 1rem 0 0.5rem 0;
      }
      p {
        margin-top: 0;
      }
      strong,
      p {
        font-size: 1.15rem;
      }
      ul {
        padding: 0 1rem;
        list-style: none;
        li + li {
          margin-top: 2rem;
        }
      }
    }
  }

  &.contact {
    .children {
      // width: max-content;
    }
    address {
      padding: 0 1rem;
      width: 100%;
      display: flex;
      flex-direction: column;
      li {
        margin-bottom: 1rem;
        list-style: none;
        display: flex;
        @media (max-width: 500px) {
          * {
            font-size: 1rem;
          }
        }
        @media (max-width: 768px) {
          flex-direction: column;
        }

        span:nth-of-type(1) {
          width: 100px;
          font-style: normal;
        }
        span:nth-of-type(2) {
          margin-left: 20px;
          a {
            display: flex;
            align-items: center;
            text-decoration: none;
            color: ${(p) => p.theme.pri};
          }
          a:hover {
            text-decoration: underline;
          }
          svg {
            margin-right: 4px;
            width: 1.5rem;
            fill: ${(p) => p.theme.pri};
          }
        }
      }
    }
  }
`
