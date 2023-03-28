import { ZettelRenderer, TextItem, ZettelLine } from './Syntax'

export const getText = ({ text, type }) => {
  switch (type) {
    case 'list':
      return getText(text)
    case 'text':
      return text.map((t: TextItem) => t.text).join('')
    default:
      return text?.text
  }
}

export const render = (syntaxes: ZettelLine[], renderer: ZettelRenderer) =>
  syntaxes.map((syntax: ZettelLine, i) => {
    if (!syntax) {
      return null
    } else if (Array.isArray(syntax)) {
      return render(syntax, renderer)
    } else {
      return renderer[syntax.type](syntax as any, i)
    }
  })
