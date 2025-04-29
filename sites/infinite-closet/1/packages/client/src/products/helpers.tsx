import React from 'react'
import { useSelector } from 'shared/store'
import { shopSelectors } from './slice'
import { Text } from 'shared/components'

export const FiltersCount = (props: any) => {
  const numToggled = useSelector((state) => shopSelectors.numToggled(state))
  return <Text {...props}>Filters{numToggled > 0 && ` (${numToggled})`}</Text>
}

export const pickFgColorFromBgColor = (
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
