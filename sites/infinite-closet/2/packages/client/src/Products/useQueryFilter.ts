import React from 'react'
import { useQuery } from '@apollo/client'

import { QUERY, PAGE_SIZE, filtersByRoute, ProductRoutes } from './constants'
import FilterItems from './Filters/FilterItems'

// TODO everytime params change, this runs useQuery again
// maybe just move logic into default component
export const useQueryFilter = (
  pageNumber: number,
  params: object = {},
  route: ProductRoutes,
) => {
  const query = useQuery(QUERY, {
    variables: {
      PAGE_SIZE,
      start: 0,
      ...getQueryParams(params, route),
    },
  })

  // TODO on load, Designers sets 'matches' according to query, causing re-render
  React.useEffect(() => {
    query.refetch({
      PAGE_SIZE,
      start: pageNumber * PAGE_SIZE,
      ...getQueryParams(params, route),
    })
  }, [pageNumber])

  query.data = query.data ?? query.previousData
  return query
}
export default useQueryFilter

const getQueryParams = (params: object, route: ProductRoutes) =>
  Object.assign(
    {},
    ...filtersByRoute[route].map((k) => FilterItems[k].toFilter(params)),
  )
