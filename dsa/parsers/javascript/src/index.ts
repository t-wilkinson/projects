import { render } from './Render'
import { parse } from './Parser'
import { ZettelRenderer, zettellines, ZettelLine } from './Syntax'
import HTMLRenderer from './Renderers/HTMLRenderer'

export type Tag = {tag: string, num: number}
export type Tags = Tag[]

export function fileTags(content: string): Tags {
  const tags = content.match(/^@.*$/gm) || []
  return tags.map(tag => ({ tag, num: (tag.match(/^@+/g) || [''])[0].length }))
}

export class Zettel {
  content: string
  syntax: ZettelLine[]
  render: any
  tags: Tags

  constructor(content: string, renderer: ZettelRenderer = HTMLRenderer) {
    this.content = content || ''
    this.syntax = parse(this.content, zettellines).value
    this.render = render(this.syntax, renderer)
    this.tags = fileTags(this.content)
  }
}

export default Zettel
