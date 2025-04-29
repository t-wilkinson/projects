import React from 'react'
import { useRoute } from '@react-navigation/native'

import { Text } from 'Shared/components'

import { filtersByRoute } from './constants'
import FilterItems from './Filters/FilterItems'

export const FiltersCount = ({ routeName, ...props }) => {
  const params = useRoute().params ?? {}
  const numToggled = filtersByRoute[routeName].reduce(
    (acc, filter) => acc + FilterItems[filter].size(params),
    0,
  )
  return <Text {...props}>Filters{numToggled > 0 && ` (${numToggled})`}</Text>
}

export const resetParams = (routeName: string, navigation) => {
  const emptyParams = filtersByRoute[routeName].reduce(
    (acc: object, item: string) => {
      acc[FilterItems[item].filter] = undefined
      return acc
    },
    {},
  )
  navigation.setParams(emptyParams)
}
