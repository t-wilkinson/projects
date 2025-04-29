import React from 'react'

import { Image, Pressable } from 'Shared/components'
import Hoverable from 'Shared/Hoverable'
import { Platform } from 'Shared/components'
import { extras } from 'Shared/constants'

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
