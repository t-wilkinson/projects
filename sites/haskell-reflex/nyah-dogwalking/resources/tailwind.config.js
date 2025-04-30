const
  { theme, plugins, variants } = require('./base.js')
  plugin = require('tailwindcss/plugin')
  autoprefixer = require('autoprefixer')

module.exports = {
  prefix: '',
  important: false,
  separator: ':',

  theme: {
    ...theme,
    colors: {
      pri: theme.colors.pink,
      sec: theme.colors.purple,
      black: theme.colors.black,
      white: theme.colors.white,
      gray: theme.colors.gray,
    },
  },

  variants: { ...variants, },

  corePlugins: {},

  plugins: [
    plugin(plugins.base),
    plugin(plugins.pseudoElements),
    plugin(plugins.utilities),
    autoprefixer,
  ],
}
