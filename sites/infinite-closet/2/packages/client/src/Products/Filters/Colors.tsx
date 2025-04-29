import React from 'react'
import { Feather } from '@expo/vector-icons'

import { Box, TouchableOpacity } from 'Shared/components'
import Hoverable from 'Shared/Hoverable'

import { merge } from './FilterItem'

export default merge({
  label: 'Colors',
  filter: 'colors',
  render({ params, setParams }) {
    return (
      <>
        <Box flexDirection="row" flexWrap="wrap">
          {data.map((v) => (
            <Hoverable key={v.color}>
              <Box m="sm">
                <TouchableOpacity
                  onPress={() => {
                    params.has(v.color)
                      ? params.delete(v.color)
                      : params.add(v.color)
                    setParams(params)
                  }}
                >
                  <Box
                    borderRadius={999}
                    width={32}
                    height={32}
                    style={{ backgroundColor: v.value }}
                    alignItems="center"
                    justifyContent="center"
                  >
                    {params.has(v.color) && (
                      <Feather
                        name="check"
                        size={24}
                        style={{
                          color: pickFgColorFromBgColor(
                            v.value,
                            '#ffffff',
                            '#000000',
                          ),
                        }}
                      />
                    )}
                  </Box>
                </TouchableOpacity>
              </Box>
            </Hoverable>
          ))}
        </Box>
      </>
    )
  },
})

const data = [
  { color: 'white', label: 'White', value: '#ffffff' },
  { color: 'gray', label: 'Gray', value: '#cccccc' },
  { color: 'black', label: 'Black', value: '#000000' },
  { color: 'red', label: 'Red', value: '#ff0000' },
  { color: 'blue', label: 'Blue', value: '#0000ff' },
  { color: 'green', label: 'Green', value: '#00ff00' },
] as const

const pickFgColorFromBgColor = (
  bgColor: string,
  lightColor: string,
  darkColor: string,
) => {
  /* https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color */
  let color = bgColor.charAt(0) === '#' ? bgColor.substring(1, 7) : bgColor
  let r = parseInt(color.substring(0, 2), 16) // hexToR
  let g = parseInt(color.substring(2, 4), 16) // hexToG
  let b = parseInt(color.substring(4, 6), 16) // hexToB
  return r * 0.299 + g * 0.587 + b * 0.114 > 186 ? darkColor : lightColor
}
