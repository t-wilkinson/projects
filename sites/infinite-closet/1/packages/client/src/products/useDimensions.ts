import React from 'react'
import { Dimensions } from 'react-native'

export const useDimensions = () => {
  const [dimensions, setDimensions] = React.useState({
    window: Dimensions.get('window'),
    screen: Dimensions.get('screen'),
  })
  const onChange = ({ window, screen }) => setDimensions({ window, screen })
  React.useLayoutEffect(() => {
    Dimensions.addEventListener('change', onChange)
    return () => Dimensions.removeEventListener('change', onChange)
  })

  return dimensions
}
