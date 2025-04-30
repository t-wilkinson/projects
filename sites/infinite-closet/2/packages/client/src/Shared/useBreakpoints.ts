import { useMediaQuery, MediaQueryAllQueryable } from 'react-responsive'
import { breakpoints } from 'Shared/theme'

type MediaQuerySettings = Partial<MediaQueryAllQueryable & { query?: string }>
interface Queries {
  [key: string]: MediaQuerySettings
}

const defaultMediaQueries: Queries = {
  base: {
    maxWidth: breakpoints.mobile,
  },
  mobile: {
    minWidth: breakpoints.mobile + 1,
    maxWidth: breakpoints.tablet,
  },
  tablet: {
    minWidth: breakpoints.tablet + 1,
    maxWidth: breakpoints.laptop,
  },
  desktop: {
    minWidth: breakpoints.laptop + 1,
    maxWidth: breakpoints.desktop,
  },
  max: {
    minWidth: breakpoints.desktop + 1,
    maxWidth: breakpoints.max,
  },
  top: {
    minWidth: breakpoints.max + 1,
  },
}

export function useBreakpoints(
  mediaQueries: Queries = defaultMediaQueries,
): Queries {
  return Object.entries(mediaQueries).reduce((acc, [k, query]) => {
    const key = 'is' + k[0].toUpperCase() + k.slice(1)
    acc[key] = useMediaQuery(query)
    return acc
  }, {})
}
export default useBreakpoints
