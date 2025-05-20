/** @jsx jsx */
import React from 'react'
import {jsx, css} from '@emotion/react'

const H3 = ({children, ...props}) => {
  return <h3 css={css`font-size: 4rem; text-decoration: underline tomato; margin-bottom: 2rem; text-align: center;
    @media(max-width: 768px) {font-size: 3rem;}
    `} {...props}>
    {children}
  </h3>
}

function Sweaters() {
  return <main css={{
    padding: '0 1rem',
    display: 'grid',
    placeContent: 'center',
  }}>
    <div css={{
      maxWidth: '600px',
    }}>
      <H3>Hmm...</H3>
      <p>
        No Penguin sweaters here.
        However, would you like to donate to penguin conservation efforts?  If so please visit the
        {' '}<a target='_blank' href='https://penguinfoundation.org.au/get-involved/penguin-jumpers'>Penguin Foundation</a>
        {' '}or <a target='_blank' href='https://www.worldwildlife.org/'>World Wild Life</a> website.
      </p>
    </div>
  </main>
}

export default Sweaters
