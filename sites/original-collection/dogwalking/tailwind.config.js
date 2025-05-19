const
  base = require('./base.js')
  plugin = require('tailwindcss/plugin')
const { variants, color, theme, plugins } = base;
const dir = '/home/trey/dev/sites/all/nyah.w-dogwalking/src'

module.exports = {
  ...base,

  purge: {
    enabled: true,
    options: [],
    content: [`${dir}/**/*.tsx`, `${dir}/**/*.js`],
  },

  theme: {
    ...theme,
    colors: {
      pri: color("#20c9c6"),
      sec: color("#fff"),
      err: color('#f03a3a'),
      black: theme.colors.black,
      white: theme.colors.white,
      gray: theme.colors.gray,
    },

    fontFamily: {
      ...theme.fontFamily,
      pri: "'Lemonada', serif",
      sec: "'Roboto', sans-serif",
    },
  },

  plugins: [

    plugin(({ addBase, theme }) => {
        addBase({
          '*': { 'font-family': '"Roboto", sans-serif' },
          'h1': { fontSize: `${theme('fontSize.6xl')}`, },
          'h2': { fontSize: `${theme('fontSize.5xl')}`, },
          'h3': { fontSize: `${theme('fontSize.4xl')}`, },
          'h4': { fontSize: `${theme('fontSize.3xl')}`, },
          'h5': { fontSize: `${theme('fontSize.2xl')}`, },
          'h6': { fontSize: `${theme('fontSize.1xl')}`, },
        })
    }),

    plugin(({ addComponents }) => {
      addComponents({
        '.grid-info': {
          display: 'grid',
          grid: 'auto / repeat(3, 1fr)',
          'place-items': 'stretch center',
          'place-content': 'space-evenly',
          'grid-auto-flow': 'column',
        },
      })
    }),

    require('tailwindcss-animations'),
    plugins.base,
    plugins.components,
    plugins.pseudoElements,
    require('autoprefixer'),
  ],
}
