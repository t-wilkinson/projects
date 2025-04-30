const plugin = require('tailwindcss/plugin');

// minize as much as possible, rebuild with what I want
// I will be using this for years to come, it should be
// very acustomed to my general workflow

function color(color) {
  var obj = {default: color};
  for (let i=0; i<=10; i++)
    obj[i] = pSBC((5-i)/5, color);
  return obj;
}

inset = {
  'top': '0',
  'bottom': '0',
  'left': '0',
  'right': '0',
}

module.exports = {
color,
utils: {
  inset: inset,
},

theme: {

// GENERAL
  screens: {
    sm: '640px',
    md: '768px',
    lg: '1024px',
    xl: '1280px',
    // smm: { max: '640px'},
    // mdm: { max: '768px'},
    // lgm: { max: '1024px'},
    // xlm: { max: '1280px'},
    // 'points': {'raw': '(hover: hover)'},
  },

  colors: {
    transparent: 'transparent',
    current: 'currentColor',
    black: '#000',
    white: '#fff',
    pink: color('#d53f8c'),
    gray: color('#888'),
    red: color('#eb4034'),
    orange: color('#fffaf0'),
    yellow: color('#fffff0'),
    green: color('#f0fff4'),
    teal: color('#e6fffa'),
    blue: color('#4299e1'),
    indigo: color('#ebf4ff'),
    purple: color('#9f7aea'),
  },

  spacing: {
    px: '1px',
    '2px': '2px',
    '4px': '4px',
    0: '0',
    1: '0.25rem',
    2: '0.5rem',
    3: '0.75rem',
    4: '1rem',
    5: '1.25rem',
    6: '1.5rem',
    8: '2rem',
    10: '2.5rem',
    12: '3rem',
    16: '4rem',
    20: '5rem',
    24: '6rem',
    32: '8rem',
    40: '10rem',
    48: '12rem',
    56: '14rem',
    64: '16rem',
    96: '24rem',
    128: '32rem',
    192: '48rem',
    256: '64rem',
  },


// DISPLAY/POSITION
  // gap
  gap: theme => theme('spacing'),
  gridTemplateColumns: {
    none: 'none',
    '1': 'repeat(1, minmax(0, 1fr))',
    '2': 'repeat(2, minmax(0, 1fr))',
    '3': 'repeat(3, minmax(0, 1fr))',
    '4': 'repeat(4, minmax(0, 1fr))',
  },
  gridColumn: {
    auto: 'auto',
    'span-1': 'span 1 / span 1',
    'span-2': 'span 2 / span 2',
    'span-3': 'span 3 / span 3',
    'span-4': 'span 4 / span 4',
  },
  gridColumnStart: {
    auto: 'auto',
    '1': '1',
    '2': '2',
    '3': '3',
    '4': '4',
    '5': '5',
  },
  gridColumnEnd: {
    auto: 'auto',
    '1': '1',
    '2': '2',
    '3': '3',
    '4': '4',
    '5': '5',
  },
  gridTemplateRows: {
    none: 'none',
    '1': 'repeat(1, minmax(0, 1fr))',
    '2': 'repeat(2, minmax(0, 1fr))',
    '3': 'repeat(3, minmax(0, 1fr))',
  },
  gridRow: {
    auto: 'auto',
    'span-1': 'span 1 / span 1',
    'span-2': 'span 2 / span 2',
    'span-3': 'span 3 / span 3',
  },
  gridRowStart: {
    auto: 'auto',
    '1': '1',
    '2': '2',
    '3': '3',
    '4': '4',
  },
  gridRowEnd: {
    auto: 'auto',
    '1': '1',
    '2': '2',
    '3': '3',
    '4': '4',
  },

  // flex
  flex: {
    '1': '1 1 0%',
    auto: '1 1 auto',
    initial: '0 1 auto',
    none: 'none',
  },
  flexGrow: {
    '0': '0',
    default: '1',
  },
  flexShrink: {
    '0': '0',
    default: '1',
  },
  order: {
    first: '-9999',
    last: '9999',
    none: '0',
    '1': '1',
    '2': '2',
    '3': '3',
    '4': '4',
  },

  // top/right/bottom/left
  inset: {
    '0': '0',
    '1/2': '50%',
    '50': '50%',
    auto: 'auto',
  },

  // child spacing
  space: (theme, { negative }) => ({
    ...theme('spacing'),
    ...negative(theme('spacing')),
  }),


// SIZE
  // container
  container: {},  // sets max-width of an element to min-width of the current breakpoint

  // width
  newSizes: {
    'min': 'min-content',
    'max': 'max-content',
  },
  maxWidth: (theme, { breakpoints }) => ({
    ...breakpoints(theme('screens')),
    ...theme('spacing'),
    none: 'none',
    xs: '20rem',
    sm: '24rem',
    md: '28rem',
    lg: '32rem',
    xl: '36rem',
    '2xl': '42rem',
    '3xl': '48rem',
    '4xl': '56rem',
    '5xl': '64rem',
    '6xl': '72rem',
    'screen-md': '768px',
    'screen-lg': '1024px',
    full: '100%',
  }),
  minWidth: theme => ({
    '0': '0',
    ...theme('spacing'),
    full: '100%',
    screen: '100vw',
  }),
  width: theme => ({
    ...theme('newSizes'),
    ...theme('spacing'),
    auto: 'auto',
    '1/2': '50%',
    '1/3': '33.333333%',
    '2/3': '66.666667%',
    '1/4': '25%',
    '2/4': '50%',
    '3/4': '75%',
    '1/5': '20%',
    '2/5': '40%',
    '3/5': '60%',
    '4/5': '80%',
    full: '100%',
    screen: '100vw',
  }),

  // height
  maxHeight: {
    full: '100%',
    screen: '100vh',
  },
  minHeight: {
    '0': '0',
    full: '100%',
    screen: '100vh',
  },
  height: theme => ({
    ...theme('newSizes'),
    ...theme('spacing'),
    auto: 'auto',
    '1/2': '50%',
    '1/3': '33.333333%',
    '2/3': '66.666667%',
    '1/4': '25%',
    '2/4': '50%',
    '3/4': '75%',
    '1/5': '20%',
    '2/5': '40%',
    '3/5': '60%',
    '4/5': '80%',
    full: '100%',
    screen: '100vh',
  }),

  // margin
  margin: (theme, { negative }) => ({
    auto: 'auto',
    full: '100%',
    screen: '100vh',
    ...theme('spacing'),
    ...negative(theme('spacing')),
  }),

  // border
  borderColor: theme => ({
    ...theme('colors'),
    default: theme('currentColor'),
  }),
  borderRadius: {
    none: '0',
    sm: '0.125rem',
    default: '0.25rem',
    md: '0.375rem',
    lg: '0.5rem',
    full: '9999px',
    cirle: '100%',
  },
  borderWidth: {
    default: '1px',
    '0': '0',
    '1': '1px',
    '2': '2px',
    '4': '4px',
    '8': '8px',
  },

  // padding
  padding: theme => theme('spacing'),


// ELEMENT STYLING
  // background
  backgroundColor: theme => theme('colors'),
  backgroundPosition: {
    bottom: 'bottom',
    center: 'center',
    left: 'left',
    'left-bottom': 'left bottom',
    'left-top': 'left top',
    right: 'right',
    'right-bottom': 'right bottom',
    'right-top': 'right top',
    top: 'top',
  },
  backgroundSize: {
    auto: 'auto',
    cover: 'cover',
    contain: 'contain',
  },

  // box-shadow
  boxShadow: {
    xs: '0 0 0 1px rgba(0, 0, 0, 0.05)',
    sm: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
    default: '0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)',
    md: '0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)',
    lg: '0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05)',
    xl: '0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04)',
    inner: 'inset 0 2px 4px 0 rgba(0, 0, 0, 0.06)',
    outline: '0 0 0 3px rgba(66, 153, 225, 0.5)',
    none: 'none',
  },

  // opacity
  opacity: {
    '0': '0',
    '25': '0.25',
    '50': '0.5',
    '75': '0.75',
    '100': '1',
  },


// TYPOGRAPHY
  // font
  fontWeight: {
    100: '100', 200: '200', 300: '300', 400: '400', 500: '500', 600: '600', 700: '700', 800: '800', 900: '900',
  },
  fontSize: {
    xs: '0.75rem',
    sm: '0.875rem',
    base: '1rem',
    lg: '1.125rem',
    xl: '1.25rem',
    '2xl': '1.5rem',
    '3xl': '1.875rem',
    '4xl': '2.25rem',
    '5xl': '3rem',
    '6xl': '4rem',
    '7xl': '6rem',
    '8xl': '8rem',
    '9xl': '10rem',
  },
  fontFamily: {
    sans: [
      'system-ui',
      '-apple-system',
      'BlinkMacSystemFont',
      '"Segoe UI"',
      'Roboto',
      '"Helvetica Neue"',
      'Arial',
      '"Noto Sans"',
      'sans-serif',
      '"Apple Color Emoji"',
      '"Segoe UI Emoji"',
      '"Segoe UI Symbol"',
      '"Noto Color Emoji"',
      '"Fjalla One"',
      '"Open Sans"',
    ],
    serif: [
      'Georgia',
      'Cambria',
      '"Times New Roman"',
      'Times',
      'serif',
      '"Abril Fatface"',
    ],
    mono: [
      'Menlo',
      'Monaco',
      'Consolas',
      '"Liberation Mono"',
      '"Courier New"',
      'monospace'
    ],
  },

  // line height
  lineHeight: {
    none: '1',
    tight: '1.25',
    snug: '1.375',
    normal: '1.5',
    relaxed: '1.625',
    loose: '2',
    '3': '.75rem',
    '4': '1rem',
    '5': '1.25rem',
    '6': '1.5rem',
    '7': '1.75rem',
    '8': '2rem',
    '9': '2.25rem',
    '10': '2.5rem',
  },

  // letter spacing
  letterSpacing: {
    tighter: '-0.05em',
    tight: '-0.025em',
    normal: '0',
    wide: '0.025em',
    wider: '0.05em',
    widest: '0.1em',
  },
  placeholderColor: theme => theme('colors'),
  textColor: theme => theme('colors'),


// ANIMATIONS
  // animation
  animations: {
    'slide-right': {
      '0%': { transform: 'translateX(-8rem)', },
      '100%': { transform: 'translateX(0)', },
    },
    'spin': {
      from: { transform: 'rotate(0deg)', },
      to: { transform: 'rotate(360deg)', },
    },
    'jump': {
      '0%': { transform: 'translateY(0%)', },
      '50%': { transform: 'translateY(-100%)', },
      '100%': { transform: 'translateY(0%)', },
    },
  },
  animationDuration: {
    'default': '1s',
    '0s': '0s',
    '1s': '1s',
    '2s': '2s',
    '3s': '3s',
    '4s': '4s',
    '5s': '5s',
  },
  animationTimingFunction: {
    'default': 'ease',
    'linear': 'linear',
    'ease': 'ease',
    'ease-in': 'ease-in',
    'ease-out': 'ease-out',
    'ease-in-out': 'ease-in-out',
  },
  animationDelay: {
    'default': '0s',
    '0s': '0s',
    '1s': '1s',
    '2s': '2s',
    '3s': '3s',
    '4s': '4s',
    '5s': '5s',
  },
  animationIterationCount: { // defaults to these values
    'default': 'infinite',
    'once': '1',
    'infinite': 'infinite',
  },
  animationDirection: { // defaults to these values
    'default': 'normal',
    'normal': 'normal',
    'reverse': 'reverse',
    'alternate': 'alternate',
    'alternate-reverse': 'alternate-reverse',
  },
  animationFillMode: { // defaults to these values
    'default': 'none',
    'none': 'none',
    'forwards': 'forwards',
    'backwards': 'backwards',
    'both': 'both',
  },
  animationPlayState: { // defaults to these values
    'running': 'running',
    'paused': 'paused',
  },

  // transition
  transitionProperty: {
    none: 'none',
    all: 'all',
    default: 'background-color, border-color, color, fill, stroke, opacity, box-shadow, transform',
    colors: 'background-color, border-color, color, fill, stroke',
    opacity: 'opacity',
    shadow: 'box-shadow',
    transform: 'transform',
  },
  transitionTimingFunction: {
    linear: 'linear',
    in: 'cubic-bezier(0.4, 0, 1, 1)',
    out: 'cubic-bezier(0, 0, 0.2, 1)',
    'in-out': 'cubic-bezier(0.4, 0, 0.2, 1)',
  },
  transitionDuration: {
    '75': '75ms',
    '100': '100ms',
    '150': '150ms',
    '200': '200ms',
    '300': '300ms',
    '500': '500ms',
    '700': '700ms',
    '1000': '1000ms',
  },
  transitionDelay: {
    '75': '75ms',
    '100': '100ms',
    '150': '150ms',
    '200': '200ms',
    '300': '300ms',
    '500': '500ms',
    '700': '700ms',
    '1000': '1000ms',
  },

  // transformations
  transformOrigin: {
    center: 'center',
    top: 'top',
    'top-right': 'top right',
    right: 'right',
    'bottom-right': 'bottom right',
    bottom: 'bottom',
    'bottom-left': 'bottom left',
    left: 'left',
    'top-left': 'top left',
  },
  scale: {
    '0': '0',
    '50': '.5',
    '75': '.75',
    '80': '.8',
    '85': '.85',
    '90': '.9',
    '95': '.95',
    '100': '1',
    '105': '1.05',
    '110': '1.1',
    '125': '1.25',
    '150': '1.5',
  },
  rotate: {
    '-180': '-180deg',
    '-90': '-90deg',
    '-45': '-45deg',
    '0': '0',
    '45': '45deg',
    '90': '90deg',
    '180': '180deg',
  },
  translate: (theme, { negative }) => ({
    ...theme('spacing'),
    ...negative(theme('spacing')),
    '-full': '-100%',
    '-1/2': '-50%',
    '1/2': '50%',
    full: '100%',
  }),
  skew: {
    '-12': '-12deg',
    '-6': '-6deg',
    '-3': '-3deg',
    '0': '0',
    '3': '3deg',
    '6': '6deg',
    '12': '12deg',
  },

// MISCELLANEOUS
  // svg
  fill: { // set the fill color of svg to current text color
    current: 'currentColor',
  },
  stroke: { // set the stroke color of svg to current text color
    current: 'currentColor',
  },
  strokeWidth: {
    '0': '0',
    '1': '1',
    '2': '2',
  },

  // list-style
  listStyleType: {
    none: 'none',
    disc: 'disc',
    decimal: 'decimal',
  },

  // divide
  divideColor: theme => theme('borderColor'),
  divideWidth: theme => theme('borderWidth'),

  // object
  objectPosition: {
    bottom: 'bottom',
    center: 'center',
    left: 'left',
    'left-bottom': 'left bottom',
    'left-top': 'left top',
    right: 'right',
    'right-bottom': 'right bottom',
    'right-top': 'right top',
    top: 'top',
  },

  // cursor
  cursor: {
    auto: 'auto',
    default: 'default',
    pointer: 'pointer',
    wait: 'wait',
    text: 'text',
    move: 'move',
    'not-allowed': 'not-allowed',
  },

  // z-index
  zIndex: {
    auto: 'auto',
    '-50': '-50',
    '-40': '-40',
    '-30': '-30',
    '-20': '-20',
    '-10': '-10',
    '0': '0',
    '10': '10',
    '20': '20',
    '30': '30',
    '40': '40',
    '50': '50',
  },

},

variants: {
    accessibility: ['responsive', 'focus'],
    alignContent: ['responsive'],
    alignItems: ['responsive'],
    alignSelf: ['responsive'],
    appearance: ['responsive'],
    backgroundAttachment: ['responsive'],
    backgroundColor: ['responsive', 'hover', 'focus', 'before', 'after'],
    backgroundPosition: ['responsive', 'hover'],
    backgroundRepeat: ['responsive'],
    backgroundSize: ['responsive', 'hover'],
    borderCollapse: ['responsive'],
    borderColor: ['responsive', 'hover', 'focus'],
    borderRadius: ['responsive'],
    borderStyle: ['responsive'],
    borderWidth: ['responsive'],
    boxShadow: ['responsive', 'hover', 'focus'],
    boxSizing: ['responsive'],
    cursor: ['responsive'],
    display: ['responsive', 'before', 'after'],
    divideColor: ['responsive'],
    divideWidth: ['responsive'],
    fill: ['responsive'],
    flex: ['responsive'],
    flexDirection: ['responsive'],
    flexGrow: ['responsive'],
    flexShrink: ['responsive'],
    flexWrap: ['responsive'],
    float: ['responsive'],
    clear: ['responsive'],
    fontFamily: ['responsive'],
    fontSize: ['responsive'],
    fontSmoothing: ['responsive'],
    fontStyle: ['responsive'],
    fontWeight: ['responsive', 'hover', 'focus'],
    height: ['responsive', 'before', 'after'],
    inset: ['responsive'],
    justifyContent: ['responsive'],
    letterSpacing: ['responsive'],
    lineHeight: ['responsive'],
    listStylePosition: ['responsive'],
    listStyleType: ['responsive'],
    margin: ['responsive'],
    maxHeight: ['responsive'],
    maxWidth: ['responsive'],
    minHeight: ['responsive'],
    minWidth: ['responsive'],
    objectFit: ['responsive'],
    objectPosition: ['responsive'],
    opacity: ['responsive', 'hover', 'focus'],
    order: ['responsive'],
    outline: ['responsive', 'focus'],
    overflow: ['responsive'],
    padding: ['responsive'],
    placeholderColor: ['responsive', 'focus'],
    pointerEvents: ['responsive'],
    position: ['responsive'],
    resize: ['responsive'],
    space: ['responsive'],
    stroke: ['responsive'],
    strokeWidth: ['responsive'],
    tableLayout: ['responsive'],
    textAlign: ['responsive'],
    textColor: ['responsive', 'hover', 'focus'],
    textDecoration: ['responsive', 'hover', 'focus'],
    textTransform: ['responsive'],
    userSelect: ['responsive'],
    verticalAlign: ['responsive'],
    visibility: ['responsive'],
    whitespace: ['responsive'],
    width: ['responsive', 'before', 'after'],
    wordBreak: ['responsive'],
    zIndex: ['responsive'],
    gap: ['responsive'],
    gridAutoFlow: ['responsive'],
    gridTemplateColumns: ['responsive'],
    gridColumn: ['responsive'],
    gridColumnStart: ['responsive'],
    gridColumnEnd: ['responsive'],
    gridTemplateRows: ['responsive'],
    gridRow: ['responsive'],
    gridRowStart: ['responsive'],
    gridRowEnd: ['responsive'],
    transform: ['responsive'],
    transformOrigin: ['responsive'],
    scale: ['responsive', 'hover', 'focus'],
    rotate: ['responsive', 'hover', 'focus'],
    translate: ['responsive', 'hover', 'focus'],
    skew: ['responsive', 'hover', 'focus'],
    transitionProperty: ['responsive'],
    transitionTimingFunction: ['responsive'],
    transitionDuration: ['responsive'],
    transitionDelay: ['responsive'],
    gradientsPlugin: ['focus', 'responsive', 'before', 'after'],
},

corePlugins: {},

plugins: {

  base: plugin(({ addBase, theme }) => {
    addBase({
      '*': {
        'font-family': `${theme('fontFamily.sec')}`
      },
      'h1,h2,h3,h4,h5,h6,strong': {
        'font-family': `${theme('fontFamily.pri')}`,
      },
    })
  }),

  components: plugin(({ addComponents, theme }) => {
    addComponents({
      '.col': {
        'display': 'flex',
        'flex-direction': 'column',
        'align-items': 'center',
      },
      '.font-pri': {
        fontFamily: `${theme('fontFamily.pri')}`,
      },
      '.font-sec': {
        fontFamily: `${theme('fontFamily.sec')}`,
      },
    })
  }),

  pseudoElements: plugin(({ addVariant, e }) => {
    const obj = {'after': 'a', 'before': 'b'};
    const pseudoElements = [
    'after', 'backdrop', 'before', 'cue', 'first-letter', 'first-line', 'grammar-error ', 'marker ', 'placeholder ', 'selection' ]
    pseudoElements.forEach(pseudo => {
      var prefix =  obj[pseudo] || pseudo;
      addVariant(pseudo, ({ modifySelectors, separator }) => {
        modifySelectors(({ className }) => {
          return `.${e(`${prefix}${separator}${className}`)}::${pseudo}`
        })
      })
    })
  }),

}

}

function pSBC(p,c0,c1,l) {
  // https://stackoverflow.com/questions/5560248/programmatically-lighten-or-darken-a-hex-color-or-rgb-and-blend-colors
    let r,g,b,P,f,t,h,i=parseInt,m=Math.round,a=typeof(c1)=="string";
    if(typeof(p)!="number"||p<-1||p>1||typeof(c0)!="string"||(c0[0]!='r'&&c0[0]!='#')||(c1&&!a))return null;
    if(!this.pSBCr)this.pSBCr=(d)=>{
        let n=d.length,x={};
        if(n>9){
            [r,g,b,a]=d=d.split(","),n=d.length;
            if(n<3||n>4)return null;
            x.r=i(r[3]=="a"?r.slice(5):r.slice(4)),x.g=i(g),x.b=i(b),x.a=a?parseFloat(a):-1
        }else{
            if(n==8||n==6||n<4)return null;
            if(n<6)d="#"+d[1]+d[1]+d[2]+d[2]+d[3]+d[3]+(n>4?d[4]+d[4]:"");
            d=i(d.slice(1),16);
            if(n==9||n==5)x.r=d>>24&255,x.g=d>>16&255,x.b=d>>8&255,x.a=m((d&255)/0.255)/1000;
            else x.r=d>>16,x.g=d>>8&255,x.b=d&255,x.a=-1
        }return x};
    h=c0.length>9,h=a?c1.length>9?true:c1=="c"?!h:false:h,f=this.pSBCr(c0),P=p<0,t=c1&&c1!="c"?this.pSBCr(c1):P?{r:0,g:0,b:0,a:-1}:{r:255,g:255,b:255,a:-1},p=P?p*-1:p,P=1-p;
    if(!f||!t)return null;
    if(l)r=m(P*f.r+p*t.r),g=m(P*f.g+p*t.g),b=m(P*f.b+p*t.b);
    else r=m((P*f.r**2+p*t.r**2)**0.5),g=m((P*f.g**2+p*t.g**2)**0.5),b=m((P*f.b**2+p*t.b**2)**0.5);
    a=f.a,t=t.a,f=a>=0||t>=0,a=f?a<0?t:t<0?a:a*P+t*p:0;
    if(h)return"rgb"+(f?"a(":"(")+r+","+g+","+b+(f?","+m(a*1000)/1000:"")+")";
    else return"#"+(4294967296+r*16777216+g*65536+b*256+(f?m(a*255):0)).toString(16).slice(1,f?undefined:-2)
}

