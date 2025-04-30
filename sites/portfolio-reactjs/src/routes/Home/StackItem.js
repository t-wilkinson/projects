import React from 'react'
import styled from 'styled-components'

function StackItem({ tools, src, alt, description, className, color }) {
  return (
    <div
      className={className}
      style={{
        backgroundColor: color,
      }}
    >
      <div className="development-side">
        <img alt={alt} src={src} />
        <span className="description">{description}</span>
      </div>
      <ul className="tools">
        {tools.map((v, i) => (
          <li key={i}>{v}</li>
        ))}
      </ul>
    </div>
  )
}

export default styled(StackItem)`
  margin: 2rem 0;
  align-items: center;
  display: grid;
  grid-template: auto / 200px 1fr;
  @media (max-width: 768px) {
    grid-template: 200px 1fr / auto;
  }

  align-items: center;
  border-radius: 4px;
  max-width: 700px;
  width: 100%;
  border: white solid 4px;

  .development-side {
    width: 100%;
    height: 100%;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    .description {
      padding-top: 4px;
      font-size: 1.5rem;
    }
    img {
      object-fit: contain;
      width: 6rem;
    }
  }

  .tools {
    margin: 0;
    padding: 2rem 2rem;
    background: white;
    @media (max-width: 768px) {
      padding: 1rem 1rem;
      * {
        font-size: 0.9rem;
      }
    }
    * {
      font-size: 1rem;
    }
    li {
      line-height: 2.25;
      display: block;
      list-style: none;
    }
  }
`
