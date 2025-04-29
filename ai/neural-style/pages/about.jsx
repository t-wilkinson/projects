import React from 'react'
import { theme, Button } from '../components'

export default function About() {
  return <main>
    <style jsx>{`
    main { display: grid; grid-template-columns: 1fr 500px 1fr; }
    a { text-decoration: underline; }
    p { line-height: 1.5em; }
    `}</style>
    <h2 style={{fontFamily: theme.fontPri, textAlign: 'center', fontSize: '4em', gridColumn: 2}}>About</h2>
    <p style={{gridColumn: 2}}>
      This website is a basic implementation of <a href='https://en.wikipedia.org/wiki/Neural_Style_Transfer' target='_blank'>Neural Style Transfer</a>, using <a href="https://arxiv.org/abs/1508.06576" target="_blank">this</a> paper, and is implemented with <a href="https://pytroch.org" target="_blank">PyTorch</a>.
      The code for this website (including the code for the style transfer) is available <a href='https://github.com/t-wilkinson/ai-neural-style' target='_blank'>here</a>.
    </p>
  </main>
}
