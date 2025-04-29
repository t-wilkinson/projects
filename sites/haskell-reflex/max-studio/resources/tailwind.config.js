const
  base = require('./base.js')
  plugin = require('tailwindcss/plugin')
const { color, theme, plugins } = base;

module.exports = {
  ...base,

  purge: {
    enabled: false,
    // content: ['/home/trey/dev/sites/max-studio/frontend-result/bin/frontend.jsexe/all.js'],
  },

  theme: {
    ...theme,
    colors: {
      pri: color("#000"),
      sec: color("#fff"),
      purple: theme.colors.purple,
      black: theme.colors.black,
      white: theme.colors.white,
      gray: theme.colors.gray,
    },
  },

  plugins: [

    plugin(({ addBase, theme }) => {
        addBase({
          '*': { 'font-family': '"Open Sans", sans-serif' },
          'h1': { font: `${theme('fontSize.6xl')} "Abril Fatface", serif`, },
          'h2': { font: `${theme('fontSize.5xl')} "Abril Fatface", serif`, },
          'h3': { font: `${theme('fontSize.4xl')} "Abril Fatface", serif`, },
          'h4': { font: `${theme('fontSize.3xl')} "Abril Fatface", serif`, },
          'h5': { font: `${theme('fontSize.2xl')} "Abril Fatface", serif`, },
          'h6': { font: `${theme('fontSize.1xl')} "Abril Fatface", serif`, },
          'img': { filter: 'grayscale(100%)', },
        })
    }),

    plugin(({ addComponents }) => {
      addComponents({
        '.grid-body': {
          display: 'grid',
          'grid': '',
        },
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
    plugins.components,
    plugins.pseudoElements,
    plugins.utilities,
    require('autoprefixer'),
  ],
}
