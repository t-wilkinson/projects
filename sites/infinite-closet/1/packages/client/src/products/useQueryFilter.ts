import React from 'react'
import { useQuery } from '@apollo/client'

import { typeOf } from 'shared/helpers'

import { QUERY, PAGE_SIZE } from './constants'
import { filterData } from './constants'
import { SortBy, Filter, Filters } from './types'

const getStateValue = (sortBy: SortBy) =>
  filterData.Sort.data.find(({ field }) => field === sortBy)?.value

export const useQueryFilter = (filters: Filters, pageNumber: number) => {
  const query = useQuery(QUERY, {
    variables: {
      PAGE_SIZE,
      start: 0,
      sort: getStateValue('Recommended'),
    },
  })
  query.data = query.data ?? query.previousData

  // TODO on load, Designers sets 'matches' according to query, causing re-render
  React.useEffect(() => {
    const filterValues = Filter.reduce((acc, filter) => {
      const filterName = filterData[filter].filterName
      const filterType = typeOf(filters[filter])
      const filterValues = filters[filter].filter

      if (!filterName) return acc

      if (filter === 'Sort') {
        acc[filterName!] = getStateValue(filterValues as SortBy)
      } else if (filterName && filterType === '[object Set]') {
        if (filterValues.size > 0) acc[filterName] = Array.from(filterValues)
      } else {
        acc[filterName] = undefined
      }
      return acc
    }, {})

    query.refetch({
      start: pageNumber * PAGE_SIZE,
      ...filterValues,
    })
  }, [filters, pageNumber])

  return query
}
export default useQueryFilter
