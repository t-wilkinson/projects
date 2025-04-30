import React from 'react'
import { LayoutRectangle } from 'react-native'

import { extras } from 'shared/constants'
import Hoverable from 'shared/Hoverable'
import { Box, AspectView, Animated, Platform } from 'shared/components'

import usePanImage from './usePanImage'

export const FeaturedImage = ({ image, ...props }) => {
  const [mainHover, setMainhover] = React.useState<{
    clientX: number
    clientY: number
  }>()
  const [layout, setLayout] = React.useState<LayoutRectangle>()
  const scale = 2
  const pan = usePanImage(scale, layout, mainHover)

  return (
    <AspectView maxWidth={500} overflow="hidden" {...props}>
      <Box
        onLayout={({ nativeEvent: { layout } }) => setLayout(layout)}
        width="100%"
        height="100%"
      >
        <Hoverable
          onHoverOut={() => setMainhover(undefined)}
          onMouseMove={({ clientX, clientY }) =>
            setMainhover({ clientX, clientY })
          }
          style={{ width: '100%', height: '100%' }}
        >
          <Animated.Image
            style={[
              Platform.OS === 'web' &&
                mainHover &&
                layout && {
                  transform: [
                    { scale },
                    { translateX: pan.x },
                    { translateY: pan.y },
                  ],
                },
              {
                width: '100%',
                height: '100%',
              },
            ]}
            source={{ uri: extras.api + image.url }}
          />
        </Hoverable>
      </Box>
    </AspectView>
  )
}
export default FeaturedImage
