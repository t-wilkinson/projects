const
  base = require('./base.js')
  plugin = require('tailwindcss/plugin')
const { variants, color, theme, plugins } = base;
const dir = '/home/trey/dev/sites/all/max.k-studio/src'

module.exports = {
  ...base,

  purge: {
    enabled: true,
    options: [],
    content: [dir+"/**/*.tsx", dir+"/**/*.js"],
  },

  theme: {
    ...theme,
    colors: {
      pri: color("#000"),
      sec: color("#fff"),
      err: theme.colors.red,
      black: theme.colors.black,
      white: theme.colors.white,
      gray: theme.colors.gray,
    },

    fontFamily: {
      ...theme.fontFamily,
      pri: "'Abril Fatface', serif",
      sec: "'Open Sans', sans-serif",
    },
  },

  variants: {
    ...variants,
  },

  plugins: [

    plugin(({ addBase, theme }) => {
        addBase({
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
      })
    }),

    require('tailwindcss-animations'),
    plugins.base,
    plugins.components,
    plugins.pseudoElements,
    require('autoprefixer'),
  ],
}
