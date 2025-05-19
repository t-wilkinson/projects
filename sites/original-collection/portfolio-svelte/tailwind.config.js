const palette = {
  pri: '#5037FF', // '#2F12FF',
  sec: '#473D8F',
  bg: '#1C2124',
  light: '#EEECFF',
  mono: {
    default: '#',
    '700': '#3A364E',
  },
}

module.exports = {
  content: [
    "./pages/**/*.{js,ts,jsx,tsx}",
    "./components/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        ...palette,
      },
    },
  },
  plugins: [],
}
