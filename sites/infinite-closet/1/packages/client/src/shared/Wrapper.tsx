import React from 'react'
import { Box } from 'shared/components'
import { breakpoints } from 'shared/theme'

export const Wrapper = ({ children, inner = {}, outer = {} }) => (
  <Box alignItems="center" {...outer}>
    <Box
      width="100%"
      maxWidth={breakpoints.max}
      {...inner}
      children={children}
    />
  </Box>
)
export default Wrapper
