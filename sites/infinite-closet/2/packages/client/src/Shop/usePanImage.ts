import React from 'react'
import { LayoutRectangle } from 'react-native'
import { Platform, Easing, Animated } from 'Shared/components'

export const usePanImage = (
  scale: number,
  layout?: LayoutRectangle,
  hover?: Pick<MouseEvent, 'clientX' | 'clientY'>,
) => {
  const scaleFactor = 1 - 1 / scale
  const pan = React.useRef(new Animated.ValueXY()).current

  React.useEffect(() => {
    if (!layout || !hover || Platform.OS !== 'web') return
    Animated.timing(pan, {
      useNativeDriver: true,
      easing: Easing.in as any,
      toValue: {
        x: scaleFactor * (layout.left + layout.width / 2 - hover.clientX),
        y: scaleFactor * (layout.top + layout.height / 2 - hover.clientY),
      },
      duration: 1.5,
    }).start()
  }, [layout, hover])

  return pan
}

export default usePanImage
