import { createTheme } from '@shopify/restyle'

export const palette = {
  pri1: '#e7ddcb',
  pri2: '#ad9253',
  sec1: '#a3bcb6',
  sec2: '#39603d',
  black: '#000000',
  white: '#ffffff',
  gray1: '#f0f0f0',
  gray2: '#e0e0e0',
  gray3: '#c0c0c0',
  gray4: '#a0a0a0',
  gray5: '#808080',
  gray6: '#606060',
  gray7: '#404040',
  gray8: '#202020',
  gray9: '#101010',
  red: '#ff3f22',
}

export const fonts = {
  cinzel: 'Cinzel_400Regular',
  barlow: 'Barlow_300Light',
  barlow_light: 'Barlow_200ExtraLight',
  lato: 'Lato_400Regular',
  lato_bold: 'Lato_700Bold',
}

export const breakpoints = {
  base: 0, // sm
  mobile: 480, // md
  tablet: 768, // lg
  laptop: 1024, // xl
  desktop: 1200, // 2xl
  max: 1440, // 3xl
}

// TODO
const cardVariants = {
  defaults: {
    borderColor: 'black',
    borderWidth: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  circle: {
    borderRadius: 999,
  },
  rounded: {
    borderRadius: 4,
  },
  sharp: {},
}

// TODO
// https://ethercreative.github.io/react-native-shadow-generator/
const shadowVariants = {
  defaults: {},
  none: {
    shadowOpacity: 0.0,
    elevation: 0,
  },
  sm: {
    shadowColor: 'black',
    shadowOffset: {
      width: 0,
      height: 2,
    },
    shadowOpacity: 0.25,
    shadowRadius: 3.84,
    elevation: 5,
  },
  md: {
    shadowColor: 'black',
    shadowOffset: {
      width: 0,
      height: 2,
    },
    shadowOpacity: 0.25,
    shadowRadius: 3.84,
    elevation: 5,
  },
  lg: {
    shadowColor: 'black',
    shadowOffset: {
      width: 0,
      height: 2,
    },
    shadowOpacity: 0.25,
    shadowRadius: 3.84,
    elevation: 5,
  },
}

export const theme = createTheme({
  breakpoints,
  fonts,
  spacing: {
    none: 0,
    '0': 0,
    px: 1,
    '2xs': 2,
    xs: 4,
    sm: 8,
    md: 16,
    lg: 24,
    xl: 40,
    '2xl': 48,
    '3xl': 52,
    '4xl': 64,
    '5xl': 92,
    '6xl': 128,
  },
  colors: {
    ...palette,
    pri: palette.pri1,
    'pri-light': palette.pri2,
    sec: palette.sec1,
    'sec-light': palette.sec2,
    warning: palette.red,
    'light-gray': '#efefef',
    'dark-gray': '#505050',
    fg: palette.black,
  },
  textVariants: {
    defaults: { fontFamily: fonts.lato, fontSize: 16 },
    header: { fontFamily: fonts.cinzel, fontSize: 42 },
    subheader: {
      fontFamily: fonts.barlow,
      fontSize: 36,
      textTransform: 'uppercase',
    },
    'subheader-light': { fontFamily: fonts.barlow_light, fontSize: 36 },
    body: { fontFamily: fonts.lato },
    'body-bold': { fontFamily: fonts.lato_bold },
    price: { fontFamily: fonts.lato_bold },
  },
  cardVariants,
  shadowVariants,
})
export default theme

export type Theme = typeof theme
