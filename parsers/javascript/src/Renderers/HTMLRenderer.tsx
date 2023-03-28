/*
import React from 'react'
import { InlineMath, BlockMath } from 'react-katex'
import 'katex/dist/katex.min.css'
import './styles.css'

import { ZettelRenderer, Syntax } from '../Syntax'

export const defRender = (s: Syntax[keyof Syntax]) => components[s.type]?.(s as any)

// prettier-ignore
const parseLink = ({ link, text }: Syntax['link']) => {
  const href = text.startsWith('http')
    ? text
    : !link
    ? '#'
    : link.startsWith('/home')
    ? `http://localhost:4000/files${link}`
    : link
  const target = href.startsWith('#') ? '_self' : '_blank'
  return { href, target }
}

// prettier-ignore
const components: ZettelRenderer = {
  list: s => <span className="z-list"><span className="z-listitem">{s.listitem}</span> {defRender(s.text)}</span>,
  operator: s => <span className="z-operator"> {s.text} </span>,
  comment: s => <span className="z-comment">{'>'} {s.text}</span>,
  link: s => <a {...parseLink(s)} className="z-link">{s.text}</a>,
  plaintext: s => s.text,
  quote: s => <q className="z-quote">{s.text}</q>,
  inlinetex: s => <InlineMath>{s.text}</InlineMath>,
  blocktex: s => <BlockMath>{s.text}</BlockMath>,
  code: s => <code>{s.text}</code>,
  striked: s => <span className="z-striked">{s.text}</span>,
  bold: s => <span className="z-bold">{s.text}</span>,

  line: (s, i) => <div id={i?.toString()}className="z-line">{' '.repeat(s.indent)}{defRender(s.text)}</div>,
  tag: s => (
    <div className="z-tag">
      {'@'.repeat(s.num)}
      {defRender(s.text)}
    </div>
  ),
  text: ({ text }) => text?.map((t) => defRender(t as any)),
  empty: s => Array(s.num).fill(<br />),
}

export default components
 */
