import React from 'react'

import { Image, Pressable } from 'shared/components'
import Hoverable from 'shared/Hoverable'
import { Platform } from 'shared/components'
import { extras } from 'shared/constants'

export const ImageSlider = ({
  sideHover,
  setSelected,
  setSideHover,
  startIndex,
  images,
  style = {},
}) =>
  images
    .slice(startIndex, startIndex + 3)
    .map((v: { url: string } & unknown, index: number) => (
      <Hoverable
        onHoverOut={() =>
          setSideHover((sideHover: number) =>
            sideHover === startIndex + index ? undefined : sideHover,
          )
        }
        onHoverIn={() => setSideHover(startIndex + index)}
        style={Platform.OS === 'web' && { cursor: 'pointer' }}
        key={v.url}
      >
        <Pressable onPress={() => setSelected(startIndex + index)} key={v.url}>
          <Image
            style={[
              {
                width: 100,
                height: 100,
              },
              style,
              startIndex + index === sideHover && { opacity: 0.8 },
            ]}
            source={{ uri: extras.api + v.url }}
          />
        </Pressable>
      </Hoverable>
    ))
export default ImageSlider
