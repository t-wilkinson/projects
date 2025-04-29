import '@expo/match-media'
import { Platform } from 'react-native'
import { useMediaQuery } from 'react-responsive'

import { breakpoints } from 'Shared/theme'

interface Breakpoint {
  children: any
}

export const Desktop = ({ children = null }: Breakpoint) => {
  const isDesktop = useMediaQuery({ minWidth: breakpoints.laptop + 1 })
  return isDesktop ? children : null
}

export const Tablet = ({ children = null }: Breakpoint) => {
  const isTablet = useMediaQuery({
    minWidth: breakpoints.tablet + 1,
    maxWidth: breakpoints.laptop,
  })
  return isTablet ? children : null
}

export const Mobile = ({ children = null }: Breakpoint) => {
  const isMobile = useMediaQuery({ maxWidth: breakpoints.tablet })
  return isMobile ? children : null
}

export const Default = ({ children = null }: Breakpoint) => {
  const isNotMobile = useMediaQuery({ minWidth: breakpoints.tablet + 1 })
  return isNotMobile ? children : null
}

export const Web = ({ children = null }: Breakpoint) =>
  Platform.select({
    native: null,
    default: children,
  })

export const Native = ({ children = null }: Breakpoint) =>
  Platform.select({
    native: children,
    default: null,
  })
